use std::collections::HashMap;

use trinity::ir::{
  value::{
    function::FunctionRef, instruction::{PhiNode, InstructionRef, InstOpcode, Call}, block::BlockRef
  },
  module::namify, ValueRef, Instruction, TKindCode
};

pub(super) fn gather_locals(func: &FunctionRef) -> HashMap<usize, String> {
  let mut res = HashMap::new();
  for block in func.block_iter() {
    for inst in block.inst_iter() {
      match inst.get_opcode() {
        InstOpcode::Branch(_) | InstOpcode::Return | InstOpcode::Store(_) => {
          continue;
        },
        InstOpcode::Call => {
          let call = inst.as_sub::<Call>().unwrap();
          let rty = call.get_callee().get_ret_ty();
          if let TKindCode::VoidType = rty.kind() {
            continue;
          }
        },
        _ => {}
      }
      if let InstOpcode::Phi = inst.get_opcode() {
        // If this is not a Phi
      } else {
        let mut user_iter = inst.user_iter();
        // If we have a user
        if let Some(user) = user_iter.next() {
          // But just a single user
          if user_iter.next().is_none() {
            // And this user belongs to this block
            if user.get_parent().get_skey() == inst.get_parent().get_skey() {
              continue;
            }
          }
        }
      }
      res.insert(inst.get_skey(), namify(&inst.get_name()));
      // // eprintln!("[WASM-CG] {} has {} user(s)", inst.to_string(false), inst.user_iter().count());
      // if inst.user_iter().count() > 1 {
      //   res.insert(inst.get_skey(), namify(&inst.get_name()));
      //   // eprintln!("Pushed to local due to more than 1 user!");
      // }
      // if inst.user_iter().any(|x| if let InstOpcode::Phi = x.get_opcode() { true } else { false }) {
      //   res.insert(inst.get_skey(), namify(&inst.get_name()));
      //   // eprintln!("Pushed to local due to used by phi!");
      // }
      // if let InstOpcode::Phi = inst.get_opcode() {
      //   res.insert(inst.get_skey(), namify(&inst.get_name()));
      //   // eprintln!("Pushed to local due phi!");
      // }
    }
  }
  res
}

pub(super) fn gather_block_downstreams<'ctx>(block: &'ctx BlockRef) -> Vec<(InstructionRef<'ctx>, ValueRef)> {
  let mut res = vec![];
  eprintln!("[CODEGEN] block: {}", block.get_name());
  for inst in block.user_iter() {
    if let Some(phi) = inst.as_sub::<PhiNode>() {
      eprintln!("[CODEGEN]  phi: {}", inst.to_string(false));
      for (incoming_block, value) in phi.iter() {
        if incoming_block.get_skey() == block.get_skey() {
          let inst = inst.as_super();
          let inst = inst.as_ref::<Instruction>(block.ctx).unwrap();
          res.push((inst, value.clone()));
        }
      }
    }
  }
  res
}
