use std::collections::HashSet;

use trinity::ir::{module::Module, ValueRef, value::instruction::{Load, InstructionRef}};

use crate::analysis::dom_tree::DominatorTree;

fn no_store_between(i0: &InstructionRef, i1: &InstructionRef) -> bool {
  let mut q = vec![i0.get_parent()];
  let mut i = 0;
  let mut visited = HashSet::new();
  visited.insert(i0.get_parent().get_skey());
  while i < q.len() {
    for bb in q[i].succ_iter() {
      if visited.contains(&bb.get_skey()) {
        continue;
      }
      q.push(bb);
    }
    i += 1;
  }
  false
}

fn has_redundant_load(m: &Module, dt: &DominatorTree) -> Option<(ValueRef, ValueRef)> {
  for f in m.func_iter() {
    for bb0 in f.block_iter() {
      for bb1 in f.block_iter() {
        if !dt.b_dominates_b(&bb0, &bb1) {
          continue;
        }
        for i0 in bb0.inst_iter() {
          if let Some(load0) = i0.as_sub::<Load>() {
            for i1 in bb1.inst_iter() {
              if let Some(load1) = i1.as_sub::<Load>() {
                if load0.get_ptr() != load1.get_ptr() {
                  continue;
                }
                if !dt.i_dominates_i(&i0, &i1) {
                  continue;
                }
                if no_store_between(&i0, &i1) {
                  return Some((i0.as_super(), i1.as_super()));
                }
              }
            }
          }
        }
      }
    }
  }
  None
}

fn remove_redundant_load(m: &mut Module) -> bool {
  let dt = DominatorTree::new(m);

  false
}
