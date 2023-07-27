use trinity::{ir::{module::Module, value::{instruction::{PhiNode, InstMutator, BranchInst, SubInst, InstructionRef}, block::BlockRef}, ValueRef, Instruction}, builder::Builder};

fn has_trivial_phi(module: &Module) -> Option<(usize, ValueRef)> {
  for func in module.func_iter() {
    for block in func.block_iter() {
      for inst in block.inst_iter() {
        if let Some(phi) = inst.as_sub::<PhiNode>() {
          let value = phi.get_incoming_value(0).unwrap();
          if phi.iter().all(|(_, v)| v.skey == value.skey) {
            eprintln!("[SIMP] Find a trivial phi: {}, replace by: {}", inst.to_string(false), value.to_string(&module.context, true));
            return Some((inst.get_skey(), value.clone()));
          }
        }
      }
    }
  }
  None
}

pub fn remove_trivial_phi(module: &mut Module) {
  while let Some((phi, value)) = has_trivial_phi(&module) {
    let phi = Instruction::from_skey(phi);
    let mut phi = InstMutator::new(&mut module.context, &phi);
    phi.replace_all_uses_with(value);
    phi.erase_from_parent();
  }
}

// TODO(@were): This is more complicated than I expected, especially when phi is involved.
//
// fn has_trivial_branch(module: &Module) -> Option<InstructionRef> {
//   for func in module.func_iter() {
//     for block in func.block_iter() {
//       if let Some(br) = block.last_inst().unwrap().as_sub::<BranchInst>() {
//         if !br.is_cond_br() {
//           if br.dest_label().unwrap().pred_iter().count() == 1 {
//             eprintln!("[SIMP] Find a trivial branch: {}", br.to_string());
//             let res = block.last_inst().unwrap().as_super();
//             return Some(res.as_ref::<Instruction>(&module.context).unwrap())
//           }
//         }
//       }
//     }
//   }
//   None
// }
// 
// pub fn merge_trivial_branches(module: &mut Module) {
//   while let Some(br) = has_trivial_branch(module) {
//     let src = br.get_parent().as_super();
//     let to_erase = br.as_super();
//     let to_merge = {
//       let br = br.as_sub::<BranchInst>().unwrap();
//       let dst = br.dest_label().unwrap();
//       dst.inst_iter().map(|i| i.as_super()).collect::<Vec<_>>()
//     };
//     let mut mutator = InstMutator::new(&mut module.context, &to_erase);
//     mutator.erase_from_parent();
//     for elem in to_merge.iter() {
//       let mut mutator = InstMutator::new(&mut module.context, &elem);
//       mutator.move_to_block(&src, None);
//     }
//   }
// }
// 
