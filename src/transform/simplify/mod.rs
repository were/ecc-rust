pub mod arith;
pub mod cfg;
pub mod peephole;
pub mod trim;

use trinity::{ir::module::Module, verify};

use super::{dce, cse, mem};

pub fn transform(mut module: Module, level: usize) -> (Module, bool) {
  let mut modified = false;
  let mut iterative = true;
  while iterative {
    iterative = false;
    module = cse::transform(module);
    module = peephole::transform(module);
    let removed = arith::remove_trivial_inst(module);
    (iterative, module) = (iterative | removed.0, removed.1);
    iterative |= cfg::merge_trivial_branches(&mut module);
    iterative |= dce::transform(&mut module);
    iterative |= arith::simplify_arith(&mut module);
    iterative |= arith::const_propagate(&mut module);
    iterative |= cfg::phi_to_select(&mut module);
    iterative |= mem::remove_redundant_load(&mut module);
    if level == 2 {
      iterative |= cfg::simplify_constant_conditional_branches(&mut module);
      iterative |= cfg::connect_unconditional_branches(&mut module);
      let linearized = arith::linearize_addsub(module);
      (iterative, module) = (iterative | linearized.0, linearized.1);
      verify::verify(&module);
    }
    modified |= iterative;
  }
  return (module, modified);
}
