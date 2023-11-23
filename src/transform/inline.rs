use std::collections::HashSet;

use trinity::{
  ir::{module::Module, ValueRef, value::instruction::Call},
  builder::Builder
};

use crate::analysis::call_graph;

fn gather_inlinable_functions(m: &Module) -> HashSet<ValueRef> {
  let call_graph = call_graph::analyze(m);
  let mut res = HashSet::new();
  for f in m.func_iter() {
    if call_graph.is_non_recursive(&f) {
      let inst_count = f.block_iter().map(|bb| bb.get_num_insts()).sum::<usize>();
      let block_count = f.get_num_blocks();
      let callee_count = f.user_iter().count();
      let weight = (inst_count + block_count) * callee_count;
      if weight != 0 && weight < 1000 {
        eprintln!("[INLINE] {} inlinable!", f.get_name());
        res.insert(f.as_super());
      }
    }
  }
  return res;
}

fn has_inlinable_call_site(m: &Module, inlinable: &HashSet<ValueRef>) -> Option<ValueRef> {
  for f in m.func_iter() {
    for bb in f.block_iter() {
      for inst in bb.inst_iter() {
        if let Some(call) = inst.as_sub::<Call>() {
          let func_value = call.get_callee().as_super();
          if inlinable.contains(&func_value) {
            return Some(inst.as_super());
          }
        }
      }
    }
  }
  None
}

pub fn transform(m: Module) -> Module {
  let mut builder = Builder::new(m);
  let inlinable_functions = gather_inlinable_functions(&builder.module);
  // while let Some(inliner) = has_inlinable_call_site(&builder.module, &inlinable_functions) {
  // }
  return builder.module;
}

