use trinity::ir::{
  module::Module,
  value::{instruction::{PhiNode, InstMutator, BranchInst, SubInst, InstructionRef, InstOpcode}, block::BlockRef},
  Instruction, Function, ValueRef, Block
};

fn find_block_to_replace_in_phis(master: &BlockRef) -> Vec<(usize, usize)> {
  let mut res = Vec::new();
  let func = master.get_parent();
  for block in func.block_iter() {
    for inst in block.inst_iter() {
      if let Some(phi) = inst.as_sub::<PhiNode>() {
        for (i, (in_coming, _)) in phi.iter().enumerate() {
          if in_coming.get_skey() == master.get_skey() {
            res.push((inst.get_skey(), i));
          }
        }
      }
    }
  }
  return res;
}

fn replace_phis_block(module: &mut Module, to_replace: &Vec<(usize, usize)>, new_block: &ValueRef) {

  for (phi_key, idx) in to_replace.iter() {
    let phi_value = Instruction::from_skey(*phi_key);
    let mut inst = InstMutator::new(&mut module.context, &phi_value);
    inst.set_operand((*idx) * 2 + 1, new_block.clone());
  }

}

// TODO(@were): This is more complicated than I expected, especially when phi is involved.
fn has_trivial_branch(module: &Module) -> Option<(InstructionRef, Vec<(usize, usize)>)> {
  for func in module.func_iter() {
    for block in func.block_iter() {
      if let Some(br) = block.last_inst().unwrap().as_sub::<BranchInst>() {
        if let Some(dest) = br.dest_label() {
          if dest.pred_iter().count() == 1 {
            eprintln!("[CFG] Find a trivial branch: {}", br.to_string());
            let res = block.last_inst().unwrap().as_super();
            let to_replace = find_block_to_replace_in_phis(&dest);
            return Some((res.as_ref::<Instruction>(&module.context).unwrap(), to_replace))
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
          eprintln!("[CFG] Find a trivial converge: {}", br.to_string());
          eprintln!("Before:\n{}\n{}\n{}\n{}", block.to_string(true), t.to_string(true), f.to_string(true), dest.to_string(true));
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
  while let Some((br, to_replace)) = has_trivial_branch(module) {
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

    replace_phis_block(module, &to_replace, &src);

    let func = Function::from_skey(func_key).as_mut::<Function>(&mut module.context).unwrap();
    func.basic_blocks_mut().retain(|b| *b != dest);
    module.context.dispose(dest);
    modified = true;
  }

  // a -> b -> d
  // \--> c ---^
  // b/c converge to d, and b/c only have one user, which is a's branch inst, we can merge them.
  while let Some((src, t, f, dest)) = has_trivial_converge(module) {

    let remove_branch = |x: &ValueRef, module: &mut Module| {
      let x = x.as_ref::<Block>(&module.context).unwrap();
      let br = x.last_inst().unwrap().as_super();
      let mut mutator = InstMutator::new(&mut module.context, &br);
      mutator.erase_from_parent();
    };

    let func_skey = {
      src.as_ref::<Block>(&module.context).unwrap().get_parent().get_skey()
    };

    let to_replace = find_block_to_replace_in_phis(&dest.as_ref::<Block>(&module.context).unwrap());
    replace_phis_block(module, &to_replace, &src);

    remove_branch(&src, module);

    remove_branch(&t, module);
    Function::from_skey(func_skey)
      .as_mut::<Function>(&mut module.context).unwrap()
      .basic_blocks_mut().retain(|b| *b != t.skey);
    module.context.dispose(t.skey);

    remove_branch(&f, module);
    Function::from_skey(func_skey)
      .as_mut::<Function>(&mut module.context).unwrap()
      .basic_blocks_mut().retain(|b| *b != f.skey);
    module.context.dispose(f.skey);

    let to_merge = dest
      .as_ref::<Block>(&module.context).unwrap()
      .inst_iter()
      .map(|i| i.as_super())
      .collect::<Vec<_>>();

    for elem in to_merge {
      let mut mutator = InstMutator::new(&mut module.context, &elem);
      mutator.move_to_block(&src, None);
    }
    Function::from_skey(func_skey)
      .as_mut::<Function>(&mut module.context).unwrap()
      .basic_blocks_mut().retain(|b| *b != dest.skey);
    module.context.dispose(dest.skey);

    eprintln!("[CFG] After:\n{}", src.as_ref::<Block>(&module.context).unwrap().to_string(true));

    modified = true;
  }

  return modified;
}

fn has_select_phi<'ctx>(module: &'ctx Module) -> Option<(ValueRef, Vec<ValueRef>, ValueRef, Vec<(ValueRef, ValueRef, ValueRef)>)> {
  for func in module.func_iter() {
    for block in func.block_iter() {
      if let Some(last_inst) = block.last_inst() {
        if let Some(br) = last_inst.as_sub::<BranchInst>() {
          if let Some(cond) = br.cond() {
            let true_label = br.true_label().unwrap();
            let false_label = br.false_label().unwrap();
            eprintln!("[CFG] Inspecting : {}", br.to_string());
            if true_label.succ_iter().count() != 1 || false_label.succ_iter().count() != 1 {
              eprintln!("[CFG] Branch has more than one successor!");
              continue;
            }
            if true_label.get_num_insts() + false_label.get_num_insts() > 12 {
              eprintln!("[CFG] Too many instructions to hoist.");
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
              eprintln!("[CFG] Branch has memory operations!");
              continue;
            }
            let succ0 = true_label.succ_iter().next().unwrap();
            let succ1 = false_label.succ_iter().next().unwrap();
            if succ0.get_skey() != succ1.get_skey() {
              eprintln!("[CFG] Branch has different successors!");
              continue;
            }
            eprintln!("[CFG] Find an if-then-else!");
            let succ = succ0;
            // Iterate over all the phi nodes in succ.
            // Gather their incoming values within this succ.
            let mut res = Vec::new();
            for inst1 in succ.inst_iter() {
              if let Some(phi) = inst1.as_sub::<PhiNode>() {
                let tf = if phi.get_incoming_block(0).unwrap().get_skey() == br.true_label().unwrap().get_skey() {
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
              eprintln!("[CFG] Hoisting {} instructions.",
                true_label.get_num_insts() + false_label.get_num_insts() - 2);
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
      eprintln!("[CFG] Hoisting {}", elem.as_ref::<Instruction>(&module.context).unwrap().to_string(false));
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
      eprintln!("[CFG] Select {}", phi.as_ref::<Instruction>(&module.context).unwrap().to_string(false));
    }
    eprintln!("[CFG] After:\n{}", dest.as_ref::<Block>(&mut module.context).unwrap().to_string(true));
  }
  modified
}

