
use trinity::ir::{
  module::Module,
  value::instruction::{PhiNode, InstMutator, BranchInst, SubInst, InstructionRef, InstOpcode, CastOp, BinaryOp, BinaryInst},
  ValueRef, Instruction, ConstScalar, TypeRef, Function, Block
};

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
              for i in 0..2 {
                if let Some(const_scalar) = inst.get_operand(i).unwrap().as_ref::<ConstScalar>(&module.context) {
                  if const_scalar.get_value() == 0 {
                    let value = inst.get_operand(1 - i).unwrap().clone();
                    eprintln!("[SIMP] Find a trivial add: {}, replace by: {}", inst.to_string(false), value.to_string(&module.context, true));
                    return Some((inst.get_skey(), value));
                  }
                }
              }
            }
            BinaryOp::Mul => {
              for i in 0..2 {
                if let Some(const_scalar) = inst.get_operand(i).unwrap().as_ref::<ConstScalar>(&module.context) {
                  if const_scalar.get_value() == 1 {
                    let value = inst.get_operand(1 - i).unwrap().clone();
                    eprintln!("[SIMP] Find a trivial mul: {}, replace by: {}", inst.to_string(false), value.to_string(&module.context, true));
                    return Some((inst.get_skey(), value));
                  }
                }
              }
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

pub fn merge_trivial_branches(module: &mut Module) {
  while let Some((br, to_replace)) = has_trivial_branch(module) {
    let src = br.get_parent().as_super();
    let func = br.get_parent().get_parent().get_skey();
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
      let phi_mut = phi_value.as_mut::<Instruction>(&mut module.context).unwrap();
      phi_mut.set_operand((*idx) * 2 + 1, src.clone());
      let block = src.as_mut::<Block>(&mut module.context).unwrap();
      block.add_user(&phi_value);
    }
    let func = Function::from_skey(func).as_mut::<Function>(&mut module.context).unwrap();
    func.basic_blocks_mut().retain(|b| *b != dest);
  }
}

