pub mod arith;
pub mod cfg;

use trinity::ir::module::Module;

use crate::analysis::dom_tree::DominatorTree;

pub fn transform(mut module: Module) -> (Module, bool) {
  let mut modified = false;
  let mut iterative = true;
  while iterative {
    iterative = false;
    let dt = DominatorTree::new(&module);
    module = super::cse::transform(module, &dt);
    iterative |= arith::remove_trivial_inst(&mut module);
    iterative |= cfg::merge_trivial_branches(&mut module);
    iterative |= super::dce::transform(&mut module);
    iterative |= arith::simplify_arith(&mut module);
    iterative |= cfg::phi_to_select(&mut module);
    modified |= iterative;
  }
  return (module, modified);
}
