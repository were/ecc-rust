use trinity::{
  ir::{
    module::Module, value::instruction::{SelectInst, BinaryInst, BinaryOp, CastOp, InstMutator, Call},
    Instruction, ConstScalar, ValueRef, TypeRef, TKindCode
  },
  builder::Builder
};

fn has_conditional_add(m: &Module) -> Option<(TypeRef, ValueRef, ValueRef, ValueRef, ValueRef)> {
  for f in m.func_iter() {
    for bb in f.block_iter() {
      for i in bb.inst_iter() {
        if let Some(select) = i.as_sub::<SelectInst>() {
          let true_val = select.get_true_value();
          if let Some(inst) = true_val.as_ref::<Instruction>(&m.context) {
            if let Some(bin) = inst.as_sub::<BinaryInst>() {
              if bin.get_op() != BinaryOp::Add {
                continue;
              }
              for j in 0..2 {
                let orig = select.get_false_value().clone();
                if *inst.get_operand(j).unwrap() == orig {
                  let other = inst.get_operand(1 - j).unwrap();
                  if let Some(cs) = other.as_ref::<ConstScalar>(&m.context) {
                    if cs.get_value() == 1 {
                      let ty = inst.get_type().clone();
                      let cond = select.get_condition().clone();
                      return Some((ty, orig, cond, other.clone(), i.as_super()));
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
  None
}

fn rewrite_conditional_add(m: Module) -> Module {
  let mut builder = Builder::new(m);
  while let Some((ty, orig, cond, coef, value)) = has_conditional_add(&builder.module) {
    // let inst = orig.as_ref::<Instruction>(&builder.module.context).unwrap();
    let bb = value.as_ref::<Instruction>(&builder.module.context).unwrap().get_parent().as_super();
    builder.set_current_block(bb);
    builder.set_insert_before(value.clone());
    let one = builder.create_op_cast(CastOp::ZeroExt, cond, ty);
    let mul = builder.create_mul(one, coef);
    let add = builder.create_add(orig, mul);
    let mut mutator = InstMutator::new(builder.context(), &value);
    mutator.replace_all_uses_with(add);
    mutator.erase_from_parent();
  }
  builder.module
}

fn has_print_int(m: &Module) -> Option<(ValueRef, ValueRef, ValueRef)> {
  for f in m.func_iter() {
    for bb in f.block_iter() {
      for inst in bb.inst_iter() {
        if let Some(call) = inst.as_sub::<Call>() {
          if call.get_callee().get_name() == "print" {
            if let Some(arg) = call.get_arg(0).as_ref::<Instruction>(&m.context) {
              if let Some(arg_call) = arg.as_sub::<Call>() {
                if arg_call.get_callee().get_name() == "toString" {
                  return Some((inst.as_super(), arg.as_super(), arg_call.get_arg(0).clone()));
                }
              }
            }
          }
        }
      }
    }
  }
  None
}

fn rewrite_print_int(m: Module) -> Module {
  let mut builder = Builder::new(m);
  let callee = builder.module
    .func_iter()
    .filter(|x| x.get_name() == "__print_int__")
    .next()
    .unwrap()
    .as_super();
  while let Some((print, to_string, value)) = has_print_int(&builder.module) {
    {
      let inst = print.as_ref::<Instruction>(&builder.module.context).unwrap();
      let block = inst.get_parent().as_super();
      let next_inst = inst.next_inst().unwrap().as_super();
      let ty = value.get_type(&builder.module.context);
      assert!(ty.kind() == &TKindCode::IntType);
      assert!(ty.get_scalar_size_in_bits(&builder.module) == 32);
      builder.set_current_block(block);
      builder.set_insert_before(next_inst);
      builder.create_func_call(callee.clone(), vec![value]);
    }
    // Erase print
    let mut mutator = InstMutator::new(builder.context(), &print);
    mutator.erase_from_parent();
    // Erase to_string
    let mut mutator = InstMutator::new(builder.context(), &to_string);
    mutator.erase_from_parent();
  }
  builder.module
}

pub fn transform(m: Module) -> Module {
  let m0 = rewrite_print_int(m);
  let m1 = rewrite_conditional_add(m0);
  m1
}

