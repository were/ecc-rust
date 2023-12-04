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
                if inst.get_operand(j).unwrap() == orig {
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

fn has_print_int(m: &Module) -> Option<(ValueRef, ValueRef, ValueRef, bool)> {
  for f in m.func_iter() {
    for bb in f.block_iter() {
      for inst in bb.inst_iter() {
        if let Some(call) = inst.as_sub::<Call>() {
          if call.get_callee().get_name() == "print" ||
             call.get_callee().get_name() == "println" {
            if let Some(arg) = call.get_arg(0).as_ref::<Instruction>(&m.context) {
              if let Some(arg_call) = arg.as_sub::<Call>() {
                if arg_call.get_callee().get_name() == "toString" {
                  let newline = call.get_callee().get_name() == "println";
                  let call = inst.as_super();
                  let arg = arg.as_super();
                  let arg_call = arg_call.get_arg(0).clone();
                  return Some((call, arg, arg_call, newline));
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
  let __print_int__ = m.func_iter()
    .filter(|x| x.get_name() == "__print_int__")
    .next()
    .unwrap()
    .as_super();
  let (print, newline) = {
    let raw = m.func_iter()
      .filter(|x| x.get_name() == "println")
      .next()
      .unwrap();
    let mut newline = None;
    let mut print = None;
    'bb: for bb in raw.block_iter() {
      for inst in bb.inst_iter().rev() {
        if let Some(call) = inst.as_sub::<Call>() {
          if call.get_callee().get_name().eq("print") {
            newline = Some(inst.get_operand(0).unwrap().clone());
            print = Some(call.get_callee().as_super());
            break 'bb;
          }
        }
      }
    }
    if print.is_some() {
      (print.unwrap(), newline.unwrap())
    } else {
      return m;
    }
  };
  let mut builder = Builder::new(m);
  while let Some((call, to_string, value, nl)) = has_print_int(&builder.module) {
    {
      let inst = call.as_ref::<Instruction>(&builder.module.context).unwrap();
      let block = inst.get_parent().as_super();
      let next_inst = inst.next_inst().unwrap().as_super();
      let ty = value.get_type(&builder.module.context);
      assert!(ty.kind() == &TKindCode::IntType);
      assert!(ty.get_scalar_size_in_bits(&builder.module) == 32);
      builder.set_current_block(block);
      builder.set_insert_before(next_inst);
      builder.create_func_call(__print_int__.clone(), vec![value]);
      if nl {
        builder.create_func_call(print.clone(), vec![newline.clone()]);
      }
    }
    // Erase print
    let mut mutator = InstMutator::new(builder.context(), &call);
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

