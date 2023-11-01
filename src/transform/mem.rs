use trinity::ir::{module::Module, ValueRef, value::instruction::{Load, InstructionRef, Store, InstMutator}};

use crate::analysis::{
  dom_tree::DominatorTree, reachable::Reachability,
  topo::{TopoInfo, analyze_topology},
  mem::{AliasCache, AliasInfo}
};

fn no_store_between(i0: &InstructionRef, i1: &InstructionRef,
                    rt: &Reachability, topo: &TopoInfo, ac: &AliasCache) -> bool {
  let src = i0.get_parent();
  let dst = i1.get_parent();
  let slice = rt.slice(&src, &dst);
  let ty = i0.get_type().clone();
  let same_loop = {
    let src_loop = topo.get_loop_of_block(src.get_skey());
    let dst_loop = topo.get_loop_of_block(dst.get_skey());
    match (src_loop, dst_loop) {
      (Some(src_loop), Some(dst_loop)) => {
        src_loop.get_id() == dst_loop.get_id()
      }
      (None, None) => {
        true
      }
      _ => false
    }
  };
  let alias_info = ac.get(&i0.as_super());
  for bb in slice {
    let mut ii = bb.inst_iter();
    if i0.get_parent().get_skey() == bb.get_skey() {
      while let Some(inst) = ii.next() {
        if inst.get_skey() == i0.get_skey() {
          break;
        }
      }
    }
    if i1.get_parent().get_skey() == bb.get_skey() && same_loop {
      while let Some(inst) = ii.next_back() {
        if inst.get_skey() == i1.get_skey() {
          break;
        }
      }
    }
    for i in ii {
      if let Some(store) = i.as_sub::<Store>() {
        if store.get_value().get_type(i0.ctx()) == ty {
          match (&alias_info, &ac.get(&i.as_super())) {
            (AliasInfo::Array(a), AliasInfo::Array(b)) => {
              if a != b {
                continue;
              } else {
                return false;
              }
            }
            _ => {
              return false;
            }
          }
        }
      }
    }
  }
  return true;
}

fn has_redundant_load(m: &Module, dt: &DominatorTree, rt: &Reachability, ac: &AliasCache)
  -> Option<(ValueRef, ValueRef)> {
  let mut workspace = vec![false; m.context.capacity()];
  for f in m.func_iter().filter(|x| !x.is_declaration()) {
    let topo = analyze_topology(&f, &mut workspace);
    for bb0 in f.block_iter() {
      for bb1 in f.block_iter() {
        if !dt.b_dominates_b(&bb0, &bb1) {
          continue;
        }
        for i0 in bb0.inst_iter() {
          // i0 and i1 are two loads from the same pointer.
          if let Some(load0) = i0.as_sub::<Load>() {
            for i1 in bb1.inst_iter() {
              if i0.get_skey() == i1.get_skey() {
                continue;
              }
              if let Some(load1) = i1.as_sub::<Load>() {
                if load0.get_ptr() != load1.get_ptr() {
                  continue;
                }
                if !dt.i_dominates_i(&i0, &i1) {
                  continue;
                }
                if no_store_between(&i0, &i1, rt, &topo, ac) {
                  // eprintln!("{}", i0.get_parent().get_name());
                  // eprintln!("{}", i0.to_string(false));
                  // eprintln!("{}", i1.get_parent().get_name());
                  // eprintln!("{}\n", i1.to_string(false));
                  return Some((i0.as_super(), i1.as_super()));
                }
              }
            }
          }
          // i0 is a store that dominates the value of i1, which is a same-addressed load.
          if let Some(store) = i0.as_sub::<Store>() {
            for i1 in bb1.inst_iter() {
              if let Some(load) = i1.as_sub::<Load>() {
                if store.get_ptr() != load.get_ptr() {
                  continue;
                }
                if !dt.i_dominates_i(&i0, &i1) {
                  continue;
                }
                if no_store_between(&i0, &i1, rt, &topo, ac) {
                  // eprintln!("{}", i0.get_parent().get_name());
                  // eprintln!("{}", i0.to_string(false));
                  // eprintln!("{}", i1.get_parent().get_name());
                  // eprintln!("{}\n", i1.to_string(false));
                  return Some((store.get_value().clone(), i1.as_super()));
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

pub fn remove_redundant_load(m: &mut Module) -> bool {
  let dt = DominatorTree::new(m);
  let rt = Reachability::new(m);
  let ac = AliasCache::new(m);
  let mut res = false;
  while let Some((l0, l1)) = has_redundant_load(m, &dt, &rt, &ac) {
    let mut inst = InstMutator::new(&mut m.context, &l1);
    inst.replace_all_uses_with(l0);
    inst.erase_from_parent();
    res = true;
  }
  res
}
