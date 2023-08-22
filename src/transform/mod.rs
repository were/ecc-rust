use trinity::ir::module::Module;

use self::simplify::{cfg::merge_trivial_branches, arith::const_propagate};

mod ssa;
mod dce;
mod cse;
mod lifetime;
mod simplify;

pub fn optimize(mut module: Module, opt_level: i32) -> Module {
  if opt_level == 0 {
    return module;
  }
  // eprintln!("{}", module);
  const_propagate(&mut module);
  simplify::cfg::remove_unreachable_block(&mut module);
  lifetime::remove_unpaired_lifetime(&mut module);
  let mut ssa = ssa::transform(module);
  merge_trivial_branches(&mut ssa);
  if opt_level == 2 {
    lifetime::remove_lifetime_hint(&mut ssa);
    let (simplified, _) = simplify::transform(ssa);
    simplified
  } else {
    ssa
  }
}

