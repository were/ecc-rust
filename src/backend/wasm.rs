use trinity::ir::{module::Module, value::function::FunctionRef, VoidType};

fn emit_function(func: &FunctionRef, res: &mut String) {
  // emit the function signature.
  res.push_str(  "  (func $");
  res.push_str(func.emission_ready_name().as_str());
  if func.get_num_args() > 0 {
    res.push_str(" (param");
    for _ in 0..func.get_num_args() {
      res.push_str(" i32");
    }
    res.push_str(")");
  }
  let fty = func.get_type();
  if let None = fty.ret_ty().as_ref::<VoidType>(fty.ctx) {
    res.push_str(" (result i32)");
  }
  res.push_str("\n");
  // emit the function body.
  for block in func.iter() {
    for inst in block.inst_iter() {
      res.push_str("  ;;");
      res.push_str(inst.to_string(false).as_str());
      res.push_str("\n");
    }
  }
  res.push_str("  )\n");
}

pub(super) fn emit(module: &Module) -> String {
  let mut res = String::new();
  res.push_str("(module\n");
  res.push_str("  (type (;0;) (func (param i32) (result i32)))"); // malloc
  res.push_str("  (type (;1;) (func (param i32 i32)))");
  res.push_str("  (import \"env\" \"__linear_memory\" (memory (;0;) 1))\n");
  res.push_str("  (import \"env\" \"malloc\" (func (;0;) (type 0)))\n");
  res.push_str("  (import \"env\" \"__print_str__\" (func (;1;) (type 1)))\n");
  for func in module.iter() {
    if func.get_num_blocks() == 0 {
      continue;
    }
    emit_function(&func, &mut res);
  }
  res.push_str(")");
  res
}
