use std::collections::HashMap;

use trinity::ir::{
  value::{
    function::FunctionRef, instruction::{PhiNode, InstructionRef, InstOpcode}, block::BlockRef
  },
  module::namify, ValueRef, Instruction
};



pub(super) fn gather_locals(func: &FunctionRef) -> HashMap<usize, String> {
  let mut res = HashMap::new();
  for block in func.iter() {
    for inst in block.inst_iter() {
      // eprintln!("[WASM-CG] {} has {} user(s)", inst.to_string(false), inst.user_iter().count());
      if inst.user_iter().count() > 1 {
        res.insert(inst.get_skey(), namify(&inst.get_name()));
        // eprintln!("Pushed to local due to more than 1 user!");
      }
      if inst.user_iter().any(|x| if let InstOpcode::Phi = x.get_opcode() { true } else { false }) {
        res.insert(inst.get_skey(), namify(&inst.get_name()));
        // eprintln!("Pushed to local due to used by phi!");
      }
      if let InstOpcode::Phi = inst.get_opcode() {
        res.insert(inst.get_skey(), namify(&inst.get_name()));
        // eprintln!("Pushed to local due phi!");
      }
    }
  }
  res
}

pub(super) fn gather_block_downstreams<'ctx>(block: &'ctx BlockRef) -> Vec<(InstructionRef<'ctx>, ValueRef)> {
  let mut res = vec![];
  for elem in block.user_iter() {
    if let Some(phi) = elem.as_sub::<PhiNode>() {
      for (incoming_block, value) in phi.iter() {
        if incoming_block.get_skey() == block.get_skey() {
          let inst = elem.as_super();
          let inst = inst.as_ref::<Instruction>(block.ctx).unwrap();
          res.push((inst, value.clone()));
        }
      }
    }
  }
  res
}
