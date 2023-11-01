use trinity::{
  ir::{
    module::Module, value::instruction::{SelectInst, BinaryInst, BinaryOp, CastOp, InstMutator},
    Instruction, ConstScalar, ValueRef, TypeRef
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

pub fn rewrite_conditional_add(m: Module) -> Module {
  let mut builder = Builder::new(m);
  while let Some((ty, orig, cond, coef, value)) = has_conditional_add(&builder.module) {
    // let inst = orig.as_ref::<Instruction>(&builder.module.context).unwrap();
    let bb = value.as_ref::<Instruction>(&builder.module.context).unwrap().get_parent().as_super();
    builder.set_current_block(bb);
    builder.set_insert_before(value.clone());
    let one = builder.create_op_cast(CastOp::SignExt, cond, ty);
    let mul = builder.create_mul(one, coef);
    let add = builder.create_add(orig, mul);
    let mut mutator = InstMutator::new(builder.context(), &value);
    mutator.replace_all_uses_with(add);
    mutator.erase_from_parent();
  }
  builder.module
}

pub fn transform(m: Module) -> Module {
  rewrite_conditional_add(m)
}

