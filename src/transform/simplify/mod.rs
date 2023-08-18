pub mod arith;
pub mod cfg;

use trinity::ir::{
  module::Module,
  value::instruction::{InstMutator, Call},
  ValueRef
};

use crate::analysis::dom_tree::DominatorTree;

fn has_lifetime_hint(module: &Module) -> Option<ValueRef> {
  for func in module.func_iter() {
    for block in func.block_iter() {
      for inst in block.inst_iter() {
        if let Some(call) = inst.as_sub::<Call>() {
          match call.get_callee().get_name().as_str() {
            "llvm.lifetime.end" | "llvm.lifetime.start" => {
              return Some(inst.as_super())
            }
            _ => {}
          }
        }
      }
    }
  }
  None
}

pub fn remove_lifetime_hint(module: &mut Module) {
  while let Some(to_remove) = has_lifetime_hint(module) {
    let mut inst = InstMutator::new(&mut module.context, &to_remove);
    inst.erase_from_parent();
  }
}

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
