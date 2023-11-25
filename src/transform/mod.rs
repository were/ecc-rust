use trinity::ir::module::Module;

use self::simplify::{cfg::merge_trivial_branches, arith::const_propagate};

mod inline;
mod mem;
mod ssa;
mod dce;
mod cse;
mod lifetime;
mod loops;
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
    let (mut simplified_1, _) = simplify::transform(ssa, 1);
    loops::hoist::hoist_invariants(&mut simplified_1);
    let canonicalized = loops::canonicalize::transform(simplified_1);
    let inlined = inline::transform(canonicalized);
    let unrolled = loops::unroll::unroll_small_loops(inlined);
    let (mut simplified_2, _) = simplify::transform(unrolled, 2);
    simplified_2.remove_unused_functions();
    simplified_2
  } else {
    ssa
  }
}

