use trinity::ir::{
  module::Module, Instruction,
  value::instruction::{InstOpcode, InstMutator, Store, InstructionRef, Load},
  Block, TypeRef
};
use crate::analysis::{
  topo::{analyze_topology, self, Node, ChildTraverse, LoopInfo, ChildIter, },
  dom_tree::DominatorTree
};

#[derive(Clone)]
enum InvariantType {
  Memory((usize, TypeRef)),
  Expr(usize),
  SideEffect,
  CtrlFlow,
  None,
}

fn color_loop_invariants<'ctx>(
  iter: topo::ChildIter,
  workspace: &mut Vec<InvariantType>,
  visited: &mut Vec<bool>) {
  iter.for_each(|elem| {
    match elem {
      Node::Loop(li) => {
        color_loop_invariants(li.child_iter(), workspace, visited);
        if let Some(ind) = li.get_loop_ind_var() {
          let src_key = ind.get_skey();
          visited[src_key] = true;
          workspace[src_key] = InvariantType::Expr(src_key);
          let mut q = vec![ind];
          while let Some(front) = q.pop() {
            for user in front.user_iter() {
              if !visited[user.get_skey()] {
                visited[user.get_skey()] = true;
                workspace[user.get_skey()] = match user.get_opcode() {
                  InstOpcode::Load(_) => InvariantType::Memory((src_key, user.get_type().clone())),
                  InstOpcode::Store(_) | InstOpcode::Call => InvariantType::SideEffect,
                  InstOpcode::Return | InstOpcode::Phi | InstOpcode::Branch(_)
                    => InvariantType::CtrlFlow,
                  _ => workspace[front.get_skey()].clone()
                };
                q.push(user);
              }
            }
          }
        }
      }
      _ => {}
    }
  });
}

fn no_loop_alias(iter: ChildIter, ty: &TypeRef) -> bool {
  for elem in iter {
    match elem {
      Node::Loop(sub_li) => {
        if !no_loop_alias(sub_li.child_iter(), ty) {
          return false;
        }
      }
      Node::Block(bb) => {
        for inst in bb.inst_iter() {
          if let Some(store) = inst.as_sub::<Store>() {
            if store.get_value().get_type(bb.ctx()) == *ty {
              return false;
            }
          }
        }
      }
    }
  }
  return true;
}

fn can_be_hoisted(workspace: &Vec<InvariantType>, inst: &InstructionRef, li: &LoopInfo) -> Option<usize> {
  let it = workspace.get(inst.get_skey()).unwrap();
  // eprintln!("Inspecting {}", inst.to_string(false));
  match it {
    InvariantType::Expr(ind_skey) => Some(*ind_skey),
    InvariantType::Memory((ind_skey, ty)) => {
      return if no_loop_alias(li.child_iter(), ty) {
        Some(*ind_skey)
      } else {
        None
      }
    }
    InvariantType::None => {
      if let Some(_) = inst.as_sub::<Load>() {
        return if no_loop_alias(li.child_iter(), inst.get_type()) {
          Some(0)
        } else {
          None
        }
      }
      Some(0)
    }
    _ => {
      None
    }
  }
}

fn analyze_hoistable_invariants(m: &Module) -> Vec<(usize, usize)> {
  let mut workspace = vec![InvariantType::None; m.context.capacity()];
  let mut visited = vec![false; m.context.capacity()];
  let dom = DominatorTree::new(m);
  let mut res = vec![];
  for f in m.func_iter().filter(|x| !x.is_declaration()) {
    let topo = analyze_topology(&f, &mut visited);
    // print_loop_info(topo.child_iter(), 0);
    for bb in f.block_iter() {
      for i in bb.inst_iter() {
        match i.get_opcode() {
          InstOpcode::Store(_) | InstOpcode::Call => {
            workspace[i.get_skey()] = InvariantType::SideEffect;
          }
          InstOpcode::Return | InstOpcode::Phi | InstOpcode::Branch(_) => {
            workspace[i.get_skey()] = InvariantType::CtrlFlow;
          }
          _ => {}
        }
      }
    }
    color_loop_invariants(topo.child_iter(), &mut workspace, &mut visited);
    // Dump the analyzed result log.
    for bb in f.block_iter() {
      if let Some(loop_info) = topo.get_loop_of_block(bb.get_skey()) {
        if let Some(loop_ind) = loop_info.get_loop_ind_var() {
          for inst in bb.inst_iter() {
            if let Some(ind_skey) = can_be_hoisted(&workspace, &inst, &loop_info) {
              if ind_skey != loop_ind.get_skey() {
                // Find the loop block hoist to.
                let mut loop_runner = loop_info.clone();
                let mut found = false;
                while let Some(parent) = loop_runner.get_parent() {
                  // eprintln!("ind nest: {}",
                  //   loop_runner.get_loop_ind_var().unwrap().to_string(false));
                  if let Some(parent_var) = parent.get_loop_ind_var() {
                    if parent_var.get_skey() == ind_skey {
                      found = true;
                      break;
                    }
                  }
                  loop_runner = parent;
                }
                let prehead = loop_runner.get_prehead();
                if found || ind_skey == 0 {
                  // eprintln!("[Expr Invariant] {}", inst.to_string(false));
                  if !inst.operand_iter().all(|operand| {
                    if let Some(operand_inst) = operand.as_ref::<Instruction>(&m.context) {
                      dom.i_dominates_i(&operand_inst, &prehead.last_inst().unwrap())
                    } else {
                      true
                    }
                  }) {
                    // eprintln!("  Cannot be hoisted yet because of dominance");
                    continue;
                  }
                  // let parent = loop_runner.get_parent().unwrap();
                  if ind_skey != 0 {
                    // Redundant sanity check.
                    let li = topo.get_loop_of_block(loop_runner.get_prehead().get_skey()).unwrap();
                    let ind_phi = Instruction::from_skey(ind_skey);
                    let ind_var = ind_phi.as_ref::<Instruction>(&m.context).unwrap();
                    assert!(li.get_loop_ind_var().unwrap().get_skey() == ind_var.get_skey());
                    // eprintln!("  Inducted by {}", ind_var.to_string(false));
                  }
                  // eprintln!("  Belong to loop: {}", loop_ind.to_string(false));
                  // eprintln!("  Hoist to: {}", prehead.get_name());
                  res.push((prehead.get_skey(), inst.get_skey()));
                }
              }
            }
          }
        }
      }
    }
  }
  res
}

pub fn hoist_invariants(m: &mut Module) {
  loop {
    let to_hoist = analyze_hoistable_invariants(m);
    if to_hoist.is_empty() {
      break;
    }
    for (block, inst) in to_hoist {
      let block_value = Block::from_skey(block);
      let block_ref = block_value.as_ref::<Block>(&m.context).unwrap();
      let idx = block_ref.get_num_insts() - 1;
      let inst_value = Instruction::from_skey(inst);
      // eprintln!("Hoist {}\nTo {}",
      //   inst_value.as_ref::<Instruction>(&m.context).unwrap().to_string(false),
      //   block_ref.get_name());
      let mut mutator = InstMutator::new(&mut m.context, &inst_value);
      mutator.move_to_block(&block_value, Some(idx));
      // eprintln!("{}", m.to_string());
    }
  }
}

