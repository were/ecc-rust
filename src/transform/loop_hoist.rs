use trinity::ir::{module::Module, Instruction, value::instruction::{InstOpcode, InstMutator}, Block};
use crate::analysis::{topo::{analyze_topology, self, Node, ChildTraverse, print_loop_info}, dom_tree::DominatorTree};

#[derive(Clone)]
enum InvariantType {
  Memory(usize),
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
                  InstOpcode::Load(_) => InvariantType::Memory(src_key),
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

fn analyze_hoistable_invariants(m: &Module) -> Vec<(usize, usize)> {
  let mut workspace = vec![InvariantType::None; m.context.capacity()];
  let mut visited = vec![false; m.context.capacity()];
  let dom = DominatorTree::new(m);
  let mut res = vec![];
  for f in m.func_iter().filter(|x| !x.is_declaration()) {
    let topo = analyze_topology(&f, &mut visited);
    print_loop_info(topo.child_iter(), 0);
    color_loop_invariants(topo.child_iter(), &mut workspace, &mut visited);
    // Dump the analyzed result log.
    for bb in f.block_iter() {
      if let Some(loop_info) = topo.get_loop_of_block(bb.get_skey()) {
        if let Some(loop_ind) = loop_info.get_loop_ind_var() {
          for inst in bb.inst_iter() {
            if let InvariantType::Expr(ind_skey) = workspace.get(inst.get_skey()).unwrap() {
              if *ind_skey != loop_ind.get_skey() {
                let ind_phi = Instruction::from_skey(*ind_skey);
                let ind_var = ind_phi.as_ref::<Instruction>(&m.context).unwrap();
                // Find the loop block hoist to.
                let mut loop_runner = loop_info.clone();
                let mut found = false;
                while let Some(parent) = loop_runner.get_parent() {
                  // eprintln!("ind nest: {}",
                  //   loop_runner.get_loop_ind_var().unwrap().to_string(false));
                  if let Some(parent_var) = parent.get_loop_ind_var() {
                    if parent_var.get_skey() == ind_var.get_skey() {
                      found = true;
                      break;
                    }
                  }
                  loop_runner = parent;
                }
                let prehead = loop_runner.get_prehead();
                if found {
                  eprintln!("[Expr Invariant] {}", inst.to_string(false));
                  if !inst.operand_iter().all(|operand| {
                    if let Some(operand_inst) = operand.as_ref::<Instruction>(&m.context) {
                      dom.i_dominates_i(&operand_inst, &prehead.last_inst().unwrap())
                    } else {
                      true
                    }
                  }) {
                    eprintln!("  Cannot be hoisted yet because of dominance");
                    continue;
                  }
                  // Redundant sanity check.
                  let li = topo.get_loop_of_block(loop_runner.get_prehead().get_skey()).unwrap();
                  let parent = loop_runner.get_parent().unwrap();
                  assert!(li.get_loop_ind_var().unwrap().get_skey() == ind_var.get_skey());
                  eprintln!("  Inducted by {}", ind_var.to_string(false));
                  eprintln!("  Belong to loop: {}", loop_ind.to_string(false));
                  eprintln!("  Hoist to: {}", prehead.get_name());
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

pub fn hoist_loop_invariants(m: &mut Module) {
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
      let mut mutator = InstMutator::new(&mut m.context, &inst_value);
      mutator.move_to_block(&block_value, Some(idx));
    }
  }
}

