use trinity::ir::{
  module::Module,
  value::{
    instruction::{PhiNode, InstMutator, BranchInst, InstructionRef, InstOpcode},
    block::BlockMutator
  },
  Instruction, Function, ValueRef, Block
};

// TODO(@were): This is more complicated than I expected, especially when phi is involved.
fn has_trivial_branch(module: &Module) -> Option<InstructionRef> {
  for func in module.func_iter() {
    for block in func.block_iter() {
      if let Some(br) = block.last_inst().unwrap().as_sub::<BranchInst>() {
        if let Some(dest) = br.dest_label() {
          if dest.pred_iter().count() == 1 {
            // eprintln!("[CFG] Find a trivial branch: {}", br.to_string());
            let res = block.last_inst().unwrap().as_super();
            return Some(res.as_ref::<Instruction>(&module.context).unwrap())
          }
        }
      }
    }
  }
  None
}

fn has_trivial_converge(module: &Module) -> Option<(ValueRef, ValueRef, ValueRef, ValueRef)> {
  for func in module.func_iter() {
    for block in func.block_iter() {
      if let Some(terminator) = block.last_inst() {
        if let Some(br) = terminator.as_sub::<BranchInst>() {
          if !br.is_cond_br() {
            continue;
          }
          let t = br.true_label().unwrap();
          let f = br.false_label().unwrap();
          if t.succ_iter().count() != 1 || f.succ_iter().count() != 1 {
            continue;
          }
          if t.succ_iter().next().unwrap().get_skey() != f.succ_iter().next().unwrap().get_skey() {
            continue;
          }
          if t.user_iter().count() != 1 || f.user_iter().count() != 1 {
            continue;
          }
          if t.get_num_insts() != 1 || f.get_num_insts() != 1 {
            continue;
          }
          let dest = t.succ_iter().next().unwrap();
          // eprintln!("[CFG] Find a trivial converge: {}", br.to_string());
          // eprintln!("Before:\n{}\n{}\n{}\n{}", block.to_string(true), t.to_string(true), f.to_string(true), dest.to_string(true));
          return Some((block.as_super(), t.as_super(), f.as_super(), dest.as_super()))
        }
      }
    }
  }
  None
}

pub fn merge_trivial_branches(module: &mut Module) -> bool {
  let mut modified = false;

  // a directly jumps to b: a -> b
  while let Some(br) = has_trivial_branch(module) {
    let src = br.get_parent().as_super();
    let func_key = br.get_parent().get_parent().get_skey();
    let erase_br = br.as_super();
    let br = br.as_sub::<BranchInst>().unwrap();
    let dest = br.dest_label().unwrap();
    let to_merge = dest.inst_iter().map(|i| i.as_super()).collect::<Vec<_>>();
    let dest = dest.get_skey();
    let mut erase_br = InstMutator::new(&mut module.context, &erase_br);
    erase_br.erase_from_parent();
    for elem in to_merge.iter() {
      let mut mutator = InstMutator::new(&mut module.context, &elem);
      mutator.move_to_block(&src, None);
    }

    let mut bm = BlockMutator::new(&mut module.context, Block::from_skey(dest));
    bm.replace_all_uses_with(src.clone());

    let func = Function::from_skey(func_key).as_mut::<Function>(&mut module.context).unwrap();
    func.basic_blocks_mut().retain(|b| *b != dest);
    module.context.dispose(dest);
    modified = true;
  }

  // a -> b -> d
  // \--> c ---^
  // b/c converge to d, and b/c only have one user, which is a's branch inst, we can merge them.
  while let Some((src, t, f, dest)) = has_trivial_converge(module) {

    let remove_branch = |x: &ValueRef, module: &mut Module, remove_block: bool| {
      let block = x.as_ref::<Block>(&module.context).unwrap();
      let br = block.last_inst().unwrap().as_super();
      let mut mutator = InstMutator::new(&mut module.context, &br);
      mutator.erase_from_parent();
      if remove_block {
        let mut mutator = BlockMutator::new(&mut module.context, x.clone());
        mutator.erase_from_parent();
      }
    };

    let mut mutator = BlockMutator::new(&mut module.context, dest.clone());
    mutator.replace_all_uses_with(src.clone());

    remove_branch(&src, module, false);
    remove_branch(&t, module, true);
    remove_branch(&f, module, true);

    let to_merge = dest
      .as_ref::<Block>(&module.context).unwrap()
      .inst_iter()
      .map(|i| i.as_super())
      .collect::<Vec<_>>();

    for elem in to_merge {
      let mut mutator = InstMutator::new(&mut module.context, &elem);
      mutator.move_to_block(&src, None);
    }

    let mut mutator = BlockMutator::new(&mut module.context, dest);
    mutator.erase_from_parent();

    // eprintln!("[CFG] After:\n{}", src.as_ref::<Block>(&module.context).unwrap().to_string(true));

    modified = true;
  }

  return modified;
}

fn has_select_phi<'ctx>(module: &'ctx Module)
  -> Option<(ValueRef, Vec<ValueRef>, ValueRef, Vec<(ValueRef, ValueRef, ValueRef)>)> {
  for func in module.func_iter() {
    for block in func.block_iter() {
      if let Some(last_inst) = block.last_inst() {
        if let Some(br) = last_inst.as_sub::<BranchInst>() {
          if let Some(cond) = br.cond() {
            let true_label = br.true_label().unwrap();
            let false_label = br.false_label().unwrap();
            // eprintln!("[CFG] Inspecting : {}", br.to_string());
            if true_label.succ_iter().count() != 1 || false_label.succ_iter().count() != 1 {
              // eprintln!("[CFG] Branch has more than one successor!");
              continue;
            }
            if true_label.get_num_insts() + false_label.get_num_insts() > 12 {
              // eprintln!("[CFG] Too many instructions to hoist.");
              continue;
            }
            // if true_label.get_num_insts() == 1 && false_label.get_num_insts() == 1 {
            //   eprintln!("[CFG] Both branches have only one branch instruction.");
            //   continue;
            // }
            let has_mem_op = |inst: InstructionRef| {
              match inst.get_opcode() {
                InstOpcode::Load(_) | InstOpcode::Store(_) | InstOpcode::Call => true,
                _ => false
              }
            };
            if true_label.inst_iter().any(has_mem_op) || false_label.inst_iter().any(has_mem_op) {
              // eprintln!("[CFG] Branch has memory operations!");
              continue;
            }
            let succ0 = true_label.succ_iter().next().unwrap();
            let succ1 = false_label.succ_iter().next().unwrap();
            if succ0.get_skey() != succ1.get_skey() {
              // eprintln!("[CFG] Branch has different successors!");
              continue;
            }
            // eprintln!("[CFG] Find an if-then-else!");
            let succ = succ0;
            // Iterate over all the phi nodes in succ.
            // Gather their incoming values within this succ.
            let mut res = Vec::new();
            for inst1 in succ.inst_iter() {
              if let Some(phi) = inst1.as_sub::<PhiNode>() {
                let phi0 = phi.get_incoming_block(0).unwrap().get_skey();
                let tlabel = br.true_label().unwrap().get_skey();
                let tf = if phi0 == tlabel {
                  (0, 1)
                } else {
                  (1, 0)
                };
                let tf = (
                  phi.get_incoming_value(tf.0).unwrap().clone(),
                  phi.get_incoming_value(tf.1).unwrap().clone()
                );

                res.push((inst1.as_super(), tf.0, tf.1));
              }
            }
            if res.len() > 0 {
              // eprintln!("[CFG] Hoisting {} instructions.",
              //  true_label.get_num_insts() + false_label.get_num_insts() - 2);
              let mut hoist = true_label.inst_iter().map(|i| i.as_super()).collect::<Vec<_>>();
              hoist.pop();
              hoist.extend(false_label.inst_iter().map(|i| i.as_super()));
              hoist.pop();
              return Some((cond.clone(), hoist, succ.as_super(), res));
            }
          }
        }
      }
    }
  }
  None
}

pub fn phi_to_select(module: &mut Module) -> bool {
  let mut modified = false;
  while let Some((cond, hoist, dest, phis)) = has_select_phi(module) {
    modified = true;
    for (i, elem) in hoist.into_iter().enumerate() {
      // eprintln!("[CFG] Hoisting {}",
      //   elem.as_ref::<Instruction>(&module.context).unwrap().to_string(false));
      let mut mutator = InstMutator::new(&mut module.context, &elem);
      mutator.move_to_block(&dest, Some(i));
    }
    for (phi, true_value, false_value) in phis.into_iter() {
      assert!(phi.as_ref::<Instruction>(&module.context).unwrap().get_num_operands() == 4);
      let mut mutator = InstMutator::new(&mut module.context, &phi);
      mutator.set_opcode(InstOpcode::Select);
      mutator.set_operand(0, cond.clone());
      mutator.set_operand(1, true_value.clone());
      mutator.set_operand(2, false_value.clone());
      mutator.remove_operand(3);
      // eprintln!("[CFG] Select {}",
      //   phi.as_ref::<Instruction>(&module.context).unwrap().to_string(false));
    }
    // eprintln!("[CFG] After:\n{}",
    //   dest.as_ref::<Block>(&mut module.context).unwrap().to_string(true));
  }
  modified
}

fn has_unreachable_block(module: &Module) -> Option<ValueRef> {
  for func in module.func_iter() {
    for block in func.block_iter() {
      if block.get_skey() == func.get_block(0).unwrap().get_skey() {
        continue;
      }
      if block.pred_iter().count() == 0 {
        // eprintln!("[CFG] Found unreachable block: {}", block.to_string(true));
        return Some(block.as_super());
      }
    }
  }
  return None;
}

pub fn remove_unreachable_block(module: &mut Module) -> bool {
  let mut modified = false;
  while let Some(block) = has_unreachable_block(module) {
    modified = true;
    let block_ref = block.as_ref::<Block>(&module.context).unwrap();
    let to_remove = block_ref.inst_iter().map(|i| i.as_super()).rev().collect::<Vec<_>>();
    for inst in to_remove.into_iter() {
      let mut mutator = InstMutator::new(&mut module.context, &inst);
      mutator.erase_from_parent();
    }
    let mut mutator = BlockMutator::new(&mut module.context, block);
    mutator.erase_from_parent();
  }
  modified
}

fn has_unconditional_branch_block(module: &Module) -> Option<(ValueRef, ValueRef, usize)> {
  for func in module.func_iter() {
    for bb in func.block_iter() {
      if bb.get_num_insts() != 1 {
        continue;
      }
      if bb.pred_iter().count() != 1 {
        continue;
      }
      if let Some(br) = bb.last_inst().unwrap().as_sub::<BranchInst>() {
        let dest = br.dest_label().unwrap().as_super();
        if !br.is_cond_br() {
          let pred = bb.pred_iter().next().unwrap();
          let pred_br = pred.as_sub::<BranchInst>().unwrap();
          if pred_br.is_cond_br() {
            // If one branch already points to the destination, we do not point the other to
            // TODO: relax this condition to "when there is some phi still uses that branch".
            if pred_br.true_label().unwrap().get_skey() == dest.skey ||
               pred_br.false_label().unwrap().get_skey() == dest.skey {
              continue;
            } else {
              for (i, opreand) in pred.operand_iter().enumerate() {
                if opreand.skey == bb.get_skey() {
                  return Some((pred.as_super(), dest, i));
                }
              }
            }
          } else {
            return (pred.as_super(), br.dest_label().unwrap().as_super(), 1).into()
          }
        }
      }
    }
  }
  None
}

/// Connect unconditional branches.
/// A -> B -> C, where B is a single-branched block.
/// Directly connect A to C.
pub fn connect_unconditional_branches(module: &mut Module) -> bool {
  let mut modified = false;
  while let Some((br, dest, idx)) = has_unconditional_branch_block(module) {
    {
      let br_to_connect = br.as_ref::<Instruction>(&module.context).unwrap();
      let bb_to_remove = br_to_connect.get_operand(idx).unwrap();
      let br_to_remove = bb_to_remove
        .as_ref::<Block>(&module.context).unwrap().last_inst().unwrap().as_super();
      let mut inst_mutator = InstMutator::new(&mut module.context, &br_to_remove);
      inst_mutator.erase_from_parent();
    }
    {
      let br_to_connect = br.as_ref::<Instruction>(&module.context).unwrap();
      let source = br_to_connect.get_parent().as_super();
      let bb_to_remove = br_to_connect.get_operand(idx).unwrap().clone();
      let mut bb_mutator = BlockMutator::new(&mut module.context, bb_to_remove);
      bb_mutator.replace_all_uses_with(source);
      bb_mutator.erase_from_parent();
    }
    {
      let mut br_mutator = InstMutator::new(&mut module.context, &br);
      br_mutator.set_operand(idx, dest.clone());
    }
    modified = true;
  }
  modified
}
