
use trinity::{ir::{module::Module, value::{instruction::{PhiNode, InstMutator, BranchInst, SubInst, InstructionRef, InstOpcode, CastOp, BinaryOp, BinaryInst}, block::BlockRef}, ValueRef, Instruction, ConstScalar, TypeRef}, builder::Builder};

fn has_trivial_inst(module: &Module) -> Option<(usize, ValueRef)> {
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
        if let Some(binary) = inst.as_sub::<BinaryInst>() {
          match binary.get_op() {
            BinaryOp::Add => {
              if let const_scalar = binary.lhs().as_ref::<ConstScalar>(&module.context) {
              }
            }
            BinaryOp::Mul => {
            }
            _ => {}
          }
        }
      }
    }
  }
  None
}

pub fn remove_trivial_inst(module: &mut Module) {
  while let Some((inst, value)) = has_trivial_inst(&module) {
    let inst = Instruction::from_skey(inst);
    let mut inst = InstMutator::new(&mut module.context, &inst);
    inst.replace_all_uses_with(value);
    inst.erase_from_parent();
  }
}

fn has_const_inst(module: &mut Module) -> Option<(ValueRef, TypeRef, u64)> {
  for func in module.func_iter() {
    for block in func.block_iter() {
      for inst in block.inst_iter() {
        match inst.get_opcode() {
          InstOpcode::CastInst(subcast) => {
            if let CastOp::Trunc = subcast {
              let operand = inst.get_operand(0).unwrap();
              if let Some(const_scalar) = operand.as_ref::<ConstScalar>(&module.context) {
                let bits = inst.get_type().get_scalar_size_in_bits(module);
                let shift_bits = 64 - bits;
                let res = const_scalar.get_value();
                let res = res << shift_bits >> shift_bits;
                return (inst.as_super(), inst.get_type().clone(), res).into();
              }
            }
          }
          _ => {}
        }
      }
    }
  }
  None
}

pub fn const_propagate(module: &mut Module) {
  while let Some((inst, ty, value)) = has_const_inst(module) {
    let new_value = module.context.const_value(ty, value);
    let mut inst = InstMutator::new(&mut module.context, &inst);
    inst.replace_all_uses_with(new_value);
    inst.erase_from_parent();
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
