use trinity::ir::{
  module::Module,
  value::instruction::{PhiNode, InstMutator, BranchInst, SubInst, InstructionRef}, Instruction, Function
};

// TODO(@were): This is more complicated than I expected, especially when phi is involved.
fn has_trivial_branch(module: &Module) -> Option<(InstructionRef, Vec<(usize, usize)>)> {
  for func in module.func_iter() {
    for block in func.block_iter() {
      if let Some(br) = block.last_inst().unwrap().as_sub::<BranchInst>() {
        if let Some(dest) = br.dest_label() {
          if dest.pred_iter().count() == 1 {
            eprintln!("[SIMP] Find a trivial branch: {}", br.to_string());
            let res = block.last_inst().unwrap().as_super();
            let mut to_replace = Vec::new();
            for block in func.block_iter() {
              for inst in block.inst_iter() {
                if let Some(phi) = inst.as_sub::<PhiNode>() {
                  for (i, (in_coming, _)) in phi.iter().enumerate() {
                    if in_coming.get_skey() == dest.get_skey() {
                      to_replace.push((inst.get_skey(), i));
                    }
                  }
                }
              }
            }
            return Some((res.as_ref::<Instruction>(&module.context).unwrap(), to_replace))
          }
        }
      }
    }
  }
  None
}

pub fn merge_trivial_branches(module: &mut Module) -> bool {
  let mut modified = false;
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
    for (phi_key, idx) in to_replace.iter() {
      let phi_value = Instruction::from_skey(*phi_key);
      let mut inst = InstMutator::new(&mut module.context, &phi_value);
      inst.set_operand((*idx) * 2 + 1, src.clone());
      // let block = src.as_mut::<Block>(&mut module.context).unwrap();
      // block.add_user(&phi_value);
    }
    let func = Function::from_skey(func_key).as_mut::<Function>(&mut module.context).unwrap();
    func.basic_blocks_mut().retain(|b| *b != dest);
    module.context.dispose(dest);
    modified = true;
  }
  return modified;
}

