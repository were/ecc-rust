use std::collections::HashMap;

use trinity::{ir::{
  module::Module,
  value::instruction::{
    PhiNode, InstMutator, InstructionRef, InstOpcode, CastOp, BinaryOp, BinaryInst, SelectInst,
    const_folder::{fold_binary_op, fold_cmp_op}
  },
  ValueRef, Instruction, ConstScalar, IntType
}, builder::Builder};



fn add_sub_fuse(inst: &InstructionRef) -> Option<(InstOpcode, ValueRef, ValueRef)> {
  let mut count = HashMap::new();
  if let Some(binary) = inst.as_sub::<BinaryInst>() {
    let coef = match binary.get_op() {
      BinaryOp::Add => 1,
      BinaryOp::Sub => -1,
      _ => return None
    };
    if let Some(lhs) = binary.lhs().as_ref::<Instruction>(inst.ctx()) {
      if let Some(bin_lhs) = lhs.as_sub::<BinaryInst>() {
        if let Some(rhs) = binary.rhs().as_ref::<Instruction>(inst.ctx()) {
          if let Some(bin_rhs) = rhs.as_sub::<BinaryInst>() {
            // eprintln!("[SIMP] A = BinOp (B, C)");
            // eprintln!("[SIMP] A = {}", inst.to_string(false));
            // eprintln!("[SIMP] B = {}", lhs.to_string(false));
            // eprintln!("[SIMP] C = {}", rhs.to_string(false));
            // (a +/- b) +/- (c +/- d)
            let (b, b_coef) = if bin_lhs.get_op() == BinaryOp::Add {
              (bin_lhs.rhs().clone(), 1)
            } else if bin_lhs.get_op() == BinaryOp::Sub {
              (bin_lhs.rhs().clone(), -1)
            } else {
              return None;
            };
            let (c, c_coef, d, d_coef) = if bin_rhs.get_op() == BinaryOp::Add {
              (bin_rhs.lhs().clone(), coef, bin_rhs.rhs().clone(), coef)
            } else if bin_rhs.get_op() == BinaryOp::Sub {
              (bin_rhs.lhs().clone(), coef, bin_rhs.rhs().clone(), -coef)
            } else {
              return None;
            };
            let (a, a_coef) = (bin_lhs.lhs().clone(), 1);
            for (v, coef) in [(a, a_coef), (b, b_coef), (c, c_coef), (d, d_coef)].iter() {
              if let Some(count) = count.get_mut(v) {
                *count = *count + *coef;
              } else {
                count.insert(v.clone(), *coef);
              }
            }
          }
        }
      }
    }
  }
  let res = count.clone().into_iter().filter(|(_, coef)| *coef != 0).collect::<Vec<_>>();
  // for (v, coef) in count.iter() {
  //   eprintln!("[SIMP] Coef: {} x {}", v.to_string(inst.ctx, false), *coef);
  // }
  if res.len() == 2 {
    match (res.get(0).unwrap(), res.get(1).unwrap()) {
      ((a, 1), (b, 1)) => {
        return Some((InstOpcode::BinaryOp(BinaryOp::Add), a.clone(), b.clone()))
      }
      ((a, 1), (b, -1)) | ((b, -1), (a, 1)) => {
        return Some((InstOpcode::BinaryOp(BinaryOp::Sub), a.clone(), b.clone()))
      }
      _ => {}
    }
  }
  None
}

fn has_arith_to_simplify(module: &Module) -> Option<(usize, InstOpcode, ValueRef, ValueRef)> {
  for func in module.func_iter() {
    for block in func.block_iter() {
      for inst in block.inst_iter() {
        if let Some(binary) = inst.as_sub::<BinaryInst>() {
          match binary.get_op() {
            BinaryOp::Add => {
              for i in 0..2 {
                let operand = inst.get_operand(i).unwrap().as_ref::<Instruction>(inst.ctx());
                if let Some(operand_inst) = operand {
                  if let Some(operand_bin) = operand_inst.as_sub::<BinaryInst>() {
                    if let BinaryOp::Sub = operand_bin.get_op() {
                      let operand = operand_inst
                        .get_operand(0)
                        .unwrap()
                        .as_ref::<ConstScalar>(&module.context);
                      if let Some(const_scalar) = operand {
                        if const_scalar.get_value() == 0 {
                          // eprintln!("[SIMP] Add a negative value {}, can be fused into sub: {}",
                          //   operand_inst.to_string(false),
                          //   inst.to_string(false));
                          let opcode = InstOpcode::BinaryOp(BinaryOp::Sub);
                          let lhs = inst.get_operand(1 - i).unwrap().clone();
                          let rhs = operand_bin.rhs().clone();
                          return Some((inst.get_skey(), opcode, lhs, rhs));
                        }
                      }
                    }
                  }
                }
              }
              if let Some((op, a, b)) = add_sub_fuse(&inst) {
                // eprintln!("[SIMP] Sub a value {}, can be fused into add: {}",
                //   inst.to_string(false),
                //   inst.to_string(false));
                return Some((inst.get_skey(), op, a, b));
              }
            }
            // TODO(@were): Should have more principle way to do this.
            BinaryOp::Sub => {
              if let Some((op, a, b)) = add_sub_fuse(&inst) {
                // eprintln!("[SIMP] Sub a value {}, can be fused into add: {}",
                //   inst.to_string(false),
                //   inst.to_string(false));
                return Some((inst.get_skey(), op, a, b));
              }
            }
            _ => {}
          }
        } else if let Some(select) = inst.as_sub::<SelectInst>() {
          if let Some(fv) = select.get_false_value().as_ref::<ConstScalar>(&module.context) {
            if let Some(ity) = fv.get_type().as_ref::<IntType>(&module.context) {
              if ity.get_bits() != 1 {
                continue;
              }
            }
            if fv.get_value() == 0 {
              let opcode = InstOpcode::BinaryOp(BinaryOp::And);
              let cond = select.get_condition().clone();
              let tv = select.get_true_value().clone();
              return Some((inst.get_skey(), opcode, cond, tv));
            }
          }
          if let Some(tv) = select.get_true_value().as_ref::<ConstScalar>(&module.context) {
            if let Some(ity) = tv.get_type().as_ref::<IntType>(&module.context) {
              if ity.get_bits() != 1 {
                continue;
              }
            }
            if tv.get_value() == 1 {
              let opcode = InstOpcode::BinaryOp(BinaryOp::Or);
              // eprintln!("[SIMP] Select a value {}, can be fused into or: {}, {}, {}",
              //   inst.to_string(false),
              //   opcode.to_string(),
              //   select.get_condition().to_string(&module.context, true),
              //   select.get_false_value().to_string(&module.context, true));
              let cond = select.get_condition().clone();
              let value = select.get_false_value().clone();
              return Some((inst.get_skey(), opcode, cond, value));
            }
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
    let num_operands = {
      let inst = inst.as_ref::<Instruction>(&module.context).unwrap();
      // eprintln!("[SIMP] Before {}", inst.to_string(false));
      inst.get_num_operands()
    };
    let mut inst_mut = InstMutator::new(&mut module.context, &inst);
    inst_mut.set_operand(0, a);
    inst_mut.set_operand(1, b);
    inst_mut.set_opcode(opcode);
    for i in 2..num_operands {
      inst_mut.remove_operand(i);
    }
    // eprintln!("[SIMP] After {}",
    //   inst.as_ref::<Instruction>(&module.context).unwrap().to_string(false));
    modified = true;
  }
  return modified;
}


fn has_trivial_inst(builder: &mut Builder) -> Option<(usize, ValueRef)> {
  let mut const_replace_tuple = None;
  let mut cast_to_replace = None;
  let module = &builder.module;
  'func: for func in module.func_iter() {
    for block in func.block_iter() {
      for inst in block.inst_iter() {
        // Find phi node with all the branches are the same values.
        // e.g. phi [ block.1, %v0 ], [ block.1, %v0 ]
        if let Some(phi) = inst.as_sub::<PhiNode>() {
          let value = phi.get_incoming_value(0).unwrap();
          if phi.iter().all(|(_, v)| v.skey == value.skey) {
            // eprintln!("[SIMP] Find a trivial phi: {}, replace by: {}",
            //           inst.to_string(false), value.to_string(&module.context, true));
            return Some((inst.get_skey(), value.clone()));
          }
        }
        if let Some(binary) = inst.as_sub::<BinaryInst>() {
          match binary.get_op() {
            // Find trivial add: a + 0
            BinaryOp::Add => {
              for i in 0..2 {
                let operand = inst.get_operand(i).unwrap().as_ref::<ConstScalar>(&module.context);
                if let Some(const_scalar) = operand {
                  if const_scalar.get_value() == 0 {
                    let value = inst.get_operand(1 - i).unwrap().clone();
                    // eprintln!("[SIMP] Find a trivial add: {}, replace by: {}",
                    //           inst.to_string(false), value.to_string(&module.context, true));
                    return Some((inst.get_skey(), value));
                  }
                }
              }
            }
            BinaryOp::Sub => {
              // Find trivial sub: a - 0
              if let Some(const_scalar) = binary.rhs().as_ref::<ConstScalar>(&module.context) {
                if const_scalar.get_value() == 0 {
                  let value = binary.lhs().clone();
                  // eprintln!("[SIMP] Find a trivial sub: {}, replace by: {}",
                  //           inst.to_string(false), value.to_string(&module.context, true));
                  return Some((inst.get_skey(), value));
                }
              }
              // Find trivial sub: a - a
              if binary.lhs().skey == binary.rhs().skey {
                // eprintln!("[SIMP] Find a trivial sub: {}, replace by: {}",
                //           inst.to_string(false), 0);
                const_replace_tuple = Some((inst.get_skey(), inst.get_type().clone(), 0));
                break 'func;
              }
              if let Some(lhs_bin) = binary.lhs().as_ref::<Instruction>(inst.ctx()) {
                if let InstOpcode::BinaryOp(BinaryOp::Add) = lhs_bin.get_opcode() {
                  for i in 0..2 {
                    if lhs_bin.get_operand(i).unwrap().skey == binary.rhs().skey {
                      // eprintln!("[SIMP] a + x - x = a: {}", inst.to_string(false));
                      return Some((inst.get_skey(), lhs_bin.get_operand(1 - i).unwrap().clone()));
                    }
                  }
                }
              }
            }
            // Find trivial mul: a * 1
            BinaryOp::Mul => {
              for i in 0..2 {
                let operand = inst.get_operand(i).unwrap().as_ref::<ConstScalar>(&module.context);
                if let Some(const_scalar) = operand {
                  if const_scalar.get_value() == 1 {
                    let value = inst.get_operand(1 - i).unwrap().clone();
                    // eprintln!("[SIMP] Find a trivial mul: {}, replace by: {}",
                    //           inst.to_string(false), value.to_string(&module.context, true));
                    return Some((inst.get_skey(), value));
                  }
                }
              }
            }
            _ => {}
          }
        }
        if let Some(select) = inst.as_sub::<SelectInst>() {
          if let Some(tv) = select.get_true_value().as_ref::<ConstScalar>(inst.ctx()) {
            if let Some(fv) = select.get_false_value().as_ref::<ConstScalar>(inst.ctx()) {
              if tv.get_value() == fv.get_value() {
                // eprintln!("[SIMP] Find a trivial select: {}, replace by: {}",
                //           inst.to_string(false), tv.get_value());
                let tv = tv.get_value().clone();
                const_replace_tuple = Some((inst.get_skey(), inst.get_type().clone(), tv));
                break 'func;
              }
              if tv.get_value() == 1 && fv.get_value() == 0 {
                eprintln!("[SIMP] Find a trivial select: {}, replace by: {}",
                          inst.to_string(false),
                          select.get_condition().to_string(&module.context, true));
                let value = select.get_condition().clone();
                let ty = inst.get_type().clone();
                cast_to_replace = (inst.as_super(), ty.clone(), value).into();
                break 'func;
              }
            }
          }
        }
      }
    }
  }
  if let Some((inst, ty, value)) = cast_to_replace {
    let inst = inst.as_ref::<Instruction>(&builder.module.context).unwrap();
    let next_inst = inst.next_inst().unwrap().as_super();
    let block = inst.get_parent().as_super();
    let skey = inst.get_skey();
    builder.set_current_block(block);
    builder.set_insert_before(next_inst);
    let casted = builder.create_op_cast(CastOp::ZeroExt, value, ty);
    return (skey, casted).into();
  }
  if let Some((skey, ty, scalar)) = const_replace_tuple {
    let zero = builder.context().const_value(ty, scalar);
    // let inst = Instruction::from_skey(skey);
    // eprintln!("[SIMP] Find a constant inst: {}, replace by: {}",
    //   inst.to_string(&module.context, true),
    //   zero.to_string(&module.context, true));
    return Some((skey, zero));
  }
  None
}

pub fn remove_trivial_inst(module: Module) -> (bool, Module) {
  let mut modified = false;
  let mut builder = Builder::new(module);
  while let Some((inst, value)) = has_trivial_inst(&mut builder) {
    let inst = Instruction::from_skey(inst);
    let mut inst = InstMutator::new(builder.context(), &inst);
    inst.replace_all_uses_with(value);
    inst.erase_from_parent();
    modified = true;
  }
  return (modified, builder.module);
}

fn has_const_inst(module: &mut Module) -> Option<(ValueRef, ValueRef)> {
  let mut insts = vec![];
  for func in module.func_iter() {
    for block in func.block_iter() {
      for inst in block.inst_iter() {
        match inst.get_opcode() {
          InstOpcode::CastInst(_) | InstOpcode::BinaryOp(_) | InstOpcode::ICompare(_) => {
            insts.push(inst.as_super());
          }
          _ => {}
        }
      }
    }
  }
  for inst in insts {
    let (opcode, ty, operands) = {
      let inst = inst.as_ref::<Instruction>(&module.context).unwrap();
      let operands = inst.operand_iter().map(|x| x.clone()).collect::<Vec<_>>();
      (inst.get_opcode().clone(), inst.get_type().clone(), operands)
    };
    match opcode {
      InstOpcode::CastInst(subcast) => {
        if let CastOp::Trunc = subcast {
          let operand = operands.get(0).unwrap();
          if let Some(const_scalar) = operand.as_ref::<ConstScalar>(&module.context) {
            let bits = ty.get_scalar_size_in_bits(module);
            let shift_bits = 64 - bits;
            let res = const_scalar.get_value();
            let res = res << shift_bits >> shift_bits;
            let res = module.context.const_value(ty, res);
            return (inst.clone(), res).into();
          }
        }
      }
      InstOpcode::BinaryOp(op) => {
        if let Some(value) = fold_binary_op(&op, &mut module.context, &operands[0], &operands[1]) {
          return Some((inst, value));
        }
      }
      InstOpcode::ICompare(op) => {
        if let Some(value) = fold_cmp_op(&op, &mut module.context, &operands[0], &operands[1]) {
          return Some((inst, value));
        }
      }
      _ => {}
    }
  }
  None
}

pub fn const_propagate(module: &mut Module) -> bool {
  let mut modified = false;
  while let Some((inst, value)) = has_const_inst(module) {
    let mut inst = InstMutator::new(&mut module.context, &inst);
    inst.replace_all_uses_with(value);
    inst.erase_from_parent();
    modified = true;
  }
  modified
}

