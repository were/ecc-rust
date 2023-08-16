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
  let (mut ssa, dom) = ssa::transform(module);
  simplify::merge_trivial_branches(&mut ssa);
  if opt_level == 2 {
    let mut cse = cse::transform(ssa, &dom);
    simplify::remove_lifetime_hint(&mut cse);
    simplify::transform(&mut cse);
    cse
  } else {
    ssa
  }
}

