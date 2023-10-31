pub mod arith;
pub mod cfg;
pub mod peephole;

use trinity::ir::module::Module;

use super::{dce, cse, mem};

pub fn transform(mut module: Module, level: usize) -> (Module, bool) {
  let mut modified = false;
  let mut iterative = true;
  while iterative {
    iterative = false;
    module = cse::transform(module);
    module = peephole::transform(module);
    iterative |= arith::remove_trivial_inst(&mut module);
    iterative |= cfg::merge_trivial_branches(&mut module);
    iterative |= dce::transform(&mut module);
    iterative |= arith::simplify_arith(&mut module);
    iterative |= cfg::phi_to_select(&mut module);
    iterative |= mem::remove_redundant_load(&mut module);
    if level == 2 {
      iterative |= cfg::connect_unconditional_branches(&mut module);
    }
    modified |= iterative;
  }
  return (module, modified);
}
