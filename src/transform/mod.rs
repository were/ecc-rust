use trinity::ir::module::Module;

use crate::compiler::CompilerFlags;

use self::simplify::{cfg::merge_trivial_branches, arith::const_propagate};

mod inline;
mod mem;
mod ssa;
mod dce;
mod cse;
mod lifetime;
mod loops;
mod simplify;

pub fn optimize(mut module: Module, flags: &CompilerFlags) -> Module {
  let opt_level = flags.opt_level;
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
    lifetime::remove_lifetime_hint(&mut ssa, true);
    let (mut simplified_1, _) = simplify::transform(ssa, 1);
    loops::hoist::hoist_invariants(&mut simplified_1);
    let canonicalized = loops::canonicalize::transform(simplified_1);
    let mut res = canonicalized;
    loop {
      let (inlined_ir, inline_modified) = inline::transform(res, flags);
      let (unrolled_ir, unroll_modified) = loops::unroll::unroll_small_loops(inlined_ir, flags);
      let (simplified_ir, simplify_modified) = simplify::transform(unrolled_ir, 1);
      res = simplified_ir;
      if !inline_modified && !unroll_modified && !simplify_modified {
        break;
      }
    }
    let (mut simplified_2, _) = simplify::transform(res, 2);
    simplified_2.remove_unused_functions();
    simplified_2
  } else {
    ssa
  }
}

