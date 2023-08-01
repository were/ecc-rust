
use trinity::ir::{
  module::Module,
  value::instruction::{PhiNode, InstMutator, BranchInst, SubInst, InstructionRef, InstOpcode, CastOp, BinaryOp, BinaryInst},
  ValueRef, Instruction, ConstScalar, TypeRef, Function
};

fn has_arith_to_simplify(module: &Module) -> Option<(usize, InstOpcode, ValueRef, ValueRef)> {
  for func in module.func_iter() {
    for block in func.block_iter() {
      for inst in block.inst_iter() {
        if let Some(binary) = inst.as_sub::<BinaryInst>() {
          match binary.get_op() {
            BinaryOp::Add => {
              for i in 0..2 {
                if let Some(operand_inst) = inst.get_operand(i).unwrap().as_ref::<Instruction>(inst.ctx) {
                  if let Some(operand_bin) = operand_inst.as_sub::<BinaryInst>() {
                    if let BinaryOp::Sub = operand_bin.get_op() {
                      if let Some(const_scalar) = operand_inst.get_operand(0).unwrap().as_ref::<ConstScalar>(&module.context) {
                        if const_scalar.get_value() == 0 {
                          eprintln!("[SIMP] Add a negative value {}, can be fused into sub: {}",
                            operand_inst.to_string(false),
                            inst.to_string(false));
                          return Some((inst.get_skey(), InstOpcode::BinaryOp(BinaryOp::Sub), inst.get_operand(1 - i).unwrap().clone(), operand_bin.rhs().clone()));
                        }
                      }
                    }
                  }
                }
              }
            }
            BinaryOp::Sub => {
              // (a + x) - (b + x) = a - b
              if let Some(lhs) = binary.lhs().as_ref::<Instruction>(inst.ctx) {
                if let Some(bin_lhs) = lhs.as_sub::<BinaryInst>() {
                  if let BinaryOp::Add = bin_lhs.get_op() {
                    if let Some(rhs) = binary.rhs().as_ref::<Instruction>(inst.ctx) {
                      if let Some(bin_rhs) = rhs.as_sub::<BinaryInst>() {
                        if let BinaryOp::Add = bin_rhs.get_op() {
                          for x in 0..2 {
                            for y in 0..2 {
                              if lhs.get_operand(x).unwrap().skey == rhs.get_operand(y).unwrap().skey {
                                eprintln!("[SIMP] A+x {}", lhs.to_string(false));
                                eprintln!("[SIMP] B+x {}", rhs.to_string(false));
                                eprintln!("[SIMP] C: {}", inst.to_string(false));
                                let a = lhs.get_operand(1 - x).unwrap().clone();
                                let b = rhs.get_operand(1 - y).unwrap().clone();
                                return Some((inst.get_skey(), InstOpcode::BinaryOp(BinaryOp::Sub), a, b));
                              }
                            }
                          }
                        }
                      }
                    }
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

pub fn simplify_arith(module: &mut Module) -> bool {
  let mut modified = false;
  while let Some((skey, opcode, a, b)) = has_arith_to_simplify(module) {
    let inst = Instruction::from_skey(skey);
    eprintln!("[SIMP] Before {}", inst.as_ref::<Instruction>(&module.context).unwrap().to_string(false));
    let mut inst_mut = InstMutator::new(&mut module.context, &inst);
    inst_mut.set_operand(0, a);
    inst_mut.set_operand(1, b);
    inst_mut.set_opcode(opcode);
    eprintln!("[SIMP] After {}", inst.as_ref::<Instruction>(&module.context).unwrap().to_string(false));
    modified = true;
  }
  return modified;
}


fn has_trivial_inst(module: &mut Module) -> Option<(usize, ValueRef)> {
  let mut const_replace_tuple = None;
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
                    eprintln!("[SIMP] Find a trivial add: {}, replace by: {}",
                      inst.to_string(false), value.to_string(&module.context, true));
                    return Some((inst.get_skey(), value));
                  }
                }
              }
            }
            BinaryOp::Sub => {
              if let Some(const_scalar) = binary.rhs().as_ref::<ConstScalar>(&module.context) {
                if const_scalar.get_value() == 0 {
                  let value = binary.lhs().clone();
                  eprintln!("[SIMP] Find a trivial sub: {}, replace by: {}", inst.to_string(false), value.to_string(&module.context, true));
                  return Some((inst.get_skey(), value));
                }
              }
              if binary.lhs().skey == binary.rhs().skey {
                eprintln!("[SIMP] Find a trivial sub: {}, replace by: {}", inst.to_string(false), 0);
                const_replace_tuple = Some((inst.get_skey(), inst.get_type().clone(), 0));
                break;
              }
            }
            BinaryOp::Mul => {
              for i in 0..2 {
                if let Some(const_scalar) = inst.get_operand(i).unwrap().as_ref::<ConstScalar>(&module.context) {
                  if const_scalar.get_value() == 1 {
                    let value = inst.get_operand(1 - i).unwrap().clone();
                    eprintln!("[SIMP] Find a trivial mul: {}, replace by: {}",
                      inst.to_string(false), value.to_string(&module.context, true));
                    return Some((inst.get_skey(), value));
                  }
                }
              }
            }
            _ => {}
          }
        }
      }
      if const_replace_tuple.is_some() {
        break;
      }
    }
    if const_replace_tuple.is_some() {
      break;
    }
  }
  if let Some((skey, ty, scalar)) = const_replace_tuple {
    let zero = module.context.const_value(ty, scalar);
    let inst = Instruction::from_skey(skey);
    eprintln!("[SIMP] Find a constant inst: {}, replace by: {}",
      inst.to_string(&module.context, true),
      zero.to_string(&module.context, true));
    return Some((skey, zero));
  }
  None
}

pub fn remove_trivial_inst(module: &mut Module) -> bool {
  let mut modified = false;
  while let Some((inst, value)) = has_trivial_inst(module) {
    let inst = Instruction::from_skey(inst);
    let mut inst = InstMutator::new(&mut module.context, &inst);
    inst.replace_all_uses_with(value);
    inst.erase_from_parent();
    modified = true;
  }
  return modified;
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

pub fn merge_trivial_branches(module: &mut Module) -> bool {
  let mut modified = false;
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
      let mut inst = InstMutator::new(&mut module.context, &phi_value);
      inst.set_operand((*idx) * 2 + 1, src.clone());
      // let block = src.as_mut::<Block>(&mut module.context).unwrap();
      // block.add_user(&phi_value);
    }
    let func = Function::from_skey(func).as_mut::<Function>(&mut module.context).unwrap();
    func.basic_blocks_mut().retain(|b| *b != dest);
    modified = true;
  }
  return modified;
}

pub fn transform(module: &mut Module) -> bool {
  let mut modified = false;
  let mut iterative = true;
  while iterative {
    iterative = false;
    iterative |= remove_trivial_inst(module);
    iterative |= merge_trivial_branches(module);
    iterative |= super::dce::transform(module);
    iterative |= simplify_arith(module);
    modified |= iterative;
  }
  return modified;
}
