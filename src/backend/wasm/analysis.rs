use std::collections::HashMap;

use trinity::ir::{
  value::{
    function::FunctionRef, instruction::{PhiNode, InstructionRef, InstOpcode}, block::BlockRef
  }, module::namify, ValueRef, ConstScalar, Instruction
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
      if let InstOpcode::Phi = inst.get_opcode() {
        res.insert(inst.get_skey(), namify(&inst.get_name()));
        // eprintln!("Pushed to local due phi!");
      }
    }
  }
  res
}

pub(super) fn gather_block_downstreams<'ctx>(block: &'ctx BlockRef) -> Vec<(InstructionRef<'ctx>, Vec<ValueRef>)> {
  let mut res = vec![];
  for elem in block.user_iter() {
    let mut tmp = vec![];
    if let Some(phi) = elem.as_sub::<PhiNode>() {
      for elem in phi.iter() {
        if let Some(const_scalar) = elem.1.as_ref::<ConstScalar>(block.ctx) {
          tmp.push(const_scalar.as_super());
        }
      }
      let phi = elem.as_super();
      let phi = phi.as_ref::<Instruction>(block.ctx).unwrap(); 
      res.push((phi, tmp));
    }
  }
  res
}
