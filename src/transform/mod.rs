use trinity::ir::module::Module;

mod ssa;
mod dce;
mod cse;
mod simplify;

pub fn optimize(mut module: Module, opt_level: i32) -> Module {
  if opt_level == 0 {
    return module;
  }
  // eprintln!("{}", module);
  simplify::const_propagate(&mut module);
  let (ssa, dom) = ssa::transform(module);
  if opt_level == 2 {
    let cse = cse::transform(ssa, &dom);
    cse
  } else {
    ssa
  }
}

