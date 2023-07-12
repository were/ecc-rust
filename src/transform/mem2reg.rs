use std::collections::{HashMap, HashSet};
use trinity::ir::{
  module::Module,
  value::{Instruction, instruction::InstMutator},
  value::instruction::{InstOpcode, Store, Load},
};

pub fn transform(mut module: Module) -> Module {
  let mut to_replace = Vec::new();
  let mut to_remove = HashSet::new();
  for func in module.iter() {
    for block in 0..func.get_num_blocks() {
      let block = func.get_block(block).unwrap();
      let mut values = HashMap::new();
      for inst in block.inst_iter() {
        match inst.get_opcode() {
          InstOpcode::Alloca(_) => {
            // If it is allocate, create the entry.
            values.insert(inst.get_skey(), Vec::new());
          },
          // Store should be checked here.
          InstOpcode::Store(_) => {
            let store = inst.as_sub::<Store>().unwrap();
            if let Some(addr) = store.get_ptr().as_ref::<Instruction>(&module.context) {
              // If it is store, update the value of the entry for allocated addresses.
              if let InstOpcode::Alloca(_) = addr.get_opcode() {
                // We are not always in the same block of allocation.
                // If this entry is not in this block, create the entry.
                if values.get(&addr.get_skey()).is_none() {
                  values.insert(addr.get_skey(), Vec::new());
                }
                values.get_mut(&addr.get_skey()).unwrap().push(Instruction::from_skey(inst.get_skey()));
              }
            }
          },
          // If it is a load.
          InstOpcode::Load(_) => {
            let load = inst.as_sub::<Load>().unwrap();
            // And the pointer of this load is an alloca.
            if let Some(load_addr) = load.get_ptr().as_ref::<Instruction>(&module.context) {
              // If the load is from an allocated address.
              if let InstOpcode::Alloca(_) = load_addr.get_opcode() {
                if let Some(x) = values.get(&load_addr.get_skey()) {
                  // If the address is allocated in this block.
                  if let Some(store) = x.last() {
                    // Replace the load with the value.
                    let store = store.as_ref::<Instruction>(&module.context).unwrap();
                    let store = store.as_sub::<Store>().unwrap();
                    let value = store.get_value();
                    to_replace.push((Instruction::from_skey(inst.get_skey()), value.clone()));
                    to_remove.insert(Instruction::from_skey(inst.get_skey()));
                  }
                }
              }
            }
          }
          _ => {}
        }
      }
      for (_, v) in values.into_iter() {
        if v.len() != 0 {
          for idx in 0..v.len() - 1 {
            to_remove.insert(v.get(idx).unwrap().clone());
          }
        }
      }
    }
  }
  for (inst, value) in to_replace.into_iter() {
    let mut inst = InstMutator::new(&mut module.context, &inst);
    inst.replace_all_uses_with(value);
  }
  for inst in to_remove.into_iter() {
    let mut mutator = InstMutator::new(&mut module.context, &inst);
    mutator.erase_from_parent();
  }
  module
}
