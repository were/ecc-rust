use std::collections::{HashSet, HashMap};

use trinity::{
  ir::{
    module::Module, ValueRef, value::{
      instruction::{Call, InstMutator, InstOpcode},
      block::BlockRef
    },
    Instruction, Block
  },
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
    let call_inst = call_site.as_ref::<Instruction>(&builder.module.context).unwrap();
    let parent_block = call_inst.get_parent().as_super();
    let current_func = call_inst.get_parent().get_parent().as_super();
    let call = call_inst.as_sub::<Call>().unwrap();
    let mut replace = HashMap::new();
    let func = call.get_callee();
    let rty = func.get_ret_ty();
    // Gather all the arguments.
    for i in 0..func.get_num_args() {
      let arg = func.get_arg(i);
      let param = call_inst.get_operand(i).unwrap();
      replace.insert(arg, param.clone());
    }
    let func_name = func.get_name();
    let fbb = |bb: BlockRef| {
      (format!("{}.{}.inline", bb.get_name(), func_name.clone()),
       bb.as_super(),
       bb.inst_iter().map(|x| x.as_super()).collect::<Vec<_>>())
    };
    let bb_info = func.block_iter().map(fbb).collect::<Vec<_>>();
    builder.set_current_function(current_func);
    for (bb_name, origin, _) in bb_info.iter() {
      let inlined_bb = builder.create_block(bb_name.clone());
      replace.insert(origin.clone(), inlined_bb);
    }
    let splited = builder.split_block(&call_site);
    let ret_block = builder.create_block(format!("{}.return", func_name));
    let mut ret_phi = (vec![], vec![]);
    for (_, bb, insts) in bb_info.iter() {
      builder.set_current_block(replace.get(bb).unwrap().clone());
      for inst in insts {
        let (ty, op, operands, name) = {
          let freplace = |v: &ValueRef| {
            if let Some(operand) = replace.get(v) {
              operand.clone()
            } else {
              v.clone()
            }
          };
          let inst_ref = inst.as_ref::<Instruction>(&builder.module.context).unwrap();
          let ty = inst_ref.get_type().clone();
          let name = format!("inlined.{}", inst_ref.get_name());
          match inst_ref.get_opcode() {
            InstOpcode::Return => {
              if let Some(v) = inst_ref.get_operand(0) {
                ret_phi.0.push(freplace(&v));
                ret_phi.1.push(bb.clone());
              }
              (ty, InstOpcode::Branch(None), vec![ret_block.clone()], name)
            }
            _ => {
              let opcode = inst_ref.get_opcode().clone();
              let operands = inst_ref.operand_iter().map(|v| {
                freplace(v)
              }).collect::<Vec<_>>();
              (ty, opcode, operands, name)
            }
          }
        };
        let new_value = builder.create_instruction(ty, op, operands, name);
        replace.insert(inst.clone(), new_value);
      }
    }

    {
      let block = parent_block.as_ref::<Block>(&builder.module.context).unwrap();
      let br = block.last_inst().unwrap();
      if let InstOpcode::Branch(None) = br.get_opcode() {
      } else {
        panic!("{} not an unconditional branch!", br.to_string(false));
      }
      let br = br.as_super();
      let (_, entry_bb, _) = &bb_info[0];
      let entry_bb = replace.get(entry_bb).unwrap().clone();
      let mut mutator = InstMutator::new(builder.context(), &br);
      mutator.set_operand(0, entry_bb);
    }

    builder.set_current_block(ret_block);
    let ret_phi = builder.create_phi(rty, ret_phi.0, ret_phi.1);
    builder.create_unconditional_branch(splited);
    let mut mutator = InstMutator::new(builder.context(), &call_site);
    mutator.replace_all_uses_with(ret_phi);
    mutator.erase_from_parent();
  }
  return builder.module;
}

