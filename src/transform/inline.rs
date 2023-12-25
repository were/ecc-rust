use std::collections::{HashSet, HashMap};

use trinity::{
  ir::{
    module::{Module, namify}, ValueRef, value::{
      instruction::{Call, InstMutator, InstOpcode, BranchMetadata},
      block::BlockRef, function::{FuncMutator, FuncAttr}
    },
    Instruction, Block
  },
  builder::Builder, verify::verify
};

use crate::{analysis::call_graph, compiler::CompilerFlags};

fn gather_inlinable_functions(m: &Module) -> HashSet<ValueRef> {
  let call_graph = call_graph::analyze(m);
  let mut res = HashSet::new();
  for f in m.func_iter() {
    if call_graph.is_non_recursive(&f) {
      let callee_count = f.user_iter().count() as i64;
      if callee_count == 0 {
        continue;
      }
      if f.is_declaration() {
        continue;
      }
      let inst_count = f.block_iter().map(|bb| bb.get_num_insts()).sum::<usize>() as i64;
      let block_count = f.get_num_blocks() as i64;
      let num_args = f.get_num_args() as i64;
      let binary_increase = (inst_count + block_count - num_args - 1) * (callee_count - 1);
      if block_count < 5 && inst_count < 100 {
        if binary_increase < 1000 {
          // eprintln!("[INLINE] inlining function {}, binary will increase {}",
          //           f.get_name(), binary_increase);
          res.insert(f.as_super());
        }
      } else {
        // eprintln!("[INLINE] {} is too complicated to inline.", f.get_name());
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

pub fn transform(m: Module, flags: &CompilerFlags) -> (Module, bool) {
  if flags.no_inline {
    return (m, false)
  }
  let mut builder = Builder::new(m);
  let mut modified = false;
  let void_ty = builder.context().void_type();
  let inlinable_functions = gather_inlinable_functions(&builder.module);
  if flags.target == "wasm" {
    for i in 0..builder.module.get_num_functions() {
      let func = builder.module.get_function(i).unwrap().as_super();
      if !inlinable_functions.contains(&func) {
        let mut mutator = FuncMutator::new(builder.context(), func);
        mutator.add_attr(FuncAttr::NoInline);
      }
    }
  }
  while let Some(call_site) = has_inlinable_call_site(&builder.module, &inlinable_functions) {
    modified = true;
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
    let func_name = namify(&func.get_name());
    let fbb = |bb: BlockRef| {
      (format!("{}.{}", bb.get_name(), func_name.clone()),
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
      let current_bb = replace.get(bb).unwrap().clone();
      builder.set_current_block(current_bb.clone());
      for inst in insts {
        let freplace = |v: &ValueRef| {
          if let Some(operand) = replace.get(v) {
            operand.clone()
          } else {
            v.clone()
          }
        };
        let inst_ref = inst.as_ref::<Instruction>(&builder.module.context).unwrap();
        let ty = inst_ref.get_type().clone();
        let name = format!("{}.{}", func_name, inst_ref.get_name());
        match inst_ref.get_opcode() {
          InstOpcode::Return => {
            if let Some(v) = inst_ref.get_operand(0) {
              ret_phi.0.push(freplace(&v));
              ret_phi.1.push(current_bb.clone());
            }
            builder.create_unconditional_branch(
              ret_block.clone(), BranchMetadata::ReturnJump);
          }
          _ => {
            let opcode = inst_ref.get_opcode().clone();
            let operands = inst_ref.operand_iter().map(|v| {
              freplace(v)
            }).collect::<Vec<_>>();
            let new_value = builder.create_instruction(ty, opcode, operands, name);
            replace.insert(inst.clone(), new_value);
          }
        }
      }
    }

    for (_, bb, _) in bb_info.iter() {
      let new_bb = replace.get(bb).unwrap().clone();
      let insts = new_bb
        .as_ref::<Block>(&builder.module.context)
        .unwrap()
        .inst_iter()
        .filter_map(|inst| {
          let set = inst
            .operand_iter()
            .enumerate()
            .filter_map(|(i, k)| { replace.get(k).map(|v| (i, v.clone())) })
            .collect::<Vec<_>>();
          if set.is_empty() {
            None
          } else {
            Some((inst.as_super(), set))
          }
        })
        .collect::<Vec<_>>();
      for (inst, replace_operands) in insts {
        let mut mutator = InstMutator::new(builder.context(), &inst);
        for (i, v) in replace_operands {
          mutator.set_operand(i, v);
        }
      }
    }

    {
      let block = parent_block.as_ref::<Block>(&builder.module.context).unwrap();
      let br = block.last_inst().unwrap();
      if let InstOpcode::Branch(BranchMetadata::None) = br.get_opcode() {
        assert!(br.get_num_operands() == 1);
      } else {
        // panic!("{} not an unconditional branch!", br.to_string(false));
      }
      let br = br.as_super();
      let (_, entry_bb, _) = &bb_info[0];
      let entry_bb = replace.get(entry_bb).unwrap().clone();
      let mut mutator = InstMutator::new(builder.context(), &br);
      mutator.set_operand(0, entry_bb);
    }

    {
      builder.set_current_block(ret_block.clone());
      let ret_val = if rty != void_ty {
        let ret_phi = builder.create_phi(rty, ret_phi.0, ret_phi.1);
        // eprintln!("return value: {}",
        //   ret_phi.as_ref::<Instruction>(&builder.module.context).unwrap().to_string(false));
        Some(ret_phi)
      } else {
        // eprintln!("{} has no return!", func_name);
        None
      };
      // eprintln!("{}", call_site.as_ref::<Instruction>(&builder.module.context).unwrap().to_string(true));
      builder.create_unconditional_branch(splited, BranchMetadata::None);
      let mut mutator = InstMutator::new(builder.context(), &call_site);
      if ret_val.is_some() {
        mutator.replace_all_uses_with(ret_val.unwrap());
      }
      mutator.erase_from_parent();
    }
  }
  verify(&builder.module);
  // eprintln!("after inlining:{}\n", builder.module.to_string());
  return (builder.module, modified);
}

