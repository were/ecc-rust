use std::collections::{HashSet, HashMap};

use trinity::{
  ir::{module::Module, ValueRef, value::{instruction::{Call, InstMutator}, block::BlockRef}, Instruction},
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
          let callee = call.get_callee();
          let func_value = callee.as_super();
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
  while let Some(call_site) = has_inlinable_call_site(&builder.module, &inlinable_functions) {
    let call_site = call_site.as_ref::<Instruction>(&builder.module.context).unwrap();
    let current_func = call_site.get_parent().get_parent().as_super();
    let call = call_site.as_sub::<Call>().unwrap();
    let mut replace = HashMap::new();
    let func = call.get_callee();
    // Gather all the arguments.
    for i in 0..func.get_num_args() {
      let arg = func.get_arg(i);
      let param = call_site.get_operand(i).unwrap();
      replace.insert(arg, param.clone());
    }
    let fbb = |bb: BlockRef| {
      (format!("{}.{}.inline", bb.get_name(), func.get_name()),
      bb.get_skey(),
      bb.inst_iter().map(|x| x.as_super()).collect::<Vec<_>>())
    };
    let bb_info = func.block_iter().map(fbb);
    builder.set_current_function(current_func);
    for (bb_name, _, _) in bb_info {
      builder.add_block(bb_name.clone());
    }
    // let mut mutator = InstMutator::new(builder.context(), &to_inline);
    // mutator.erase_from_parent();
  }
  return builder.module;
}

