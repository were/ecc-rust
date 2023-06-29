use std::collections::HashMap;

use trinity::context::component::GetSlabKey;
use trinity::ir::{module::Module, Block, Instruction, ValueRef};
use trinity::ir::value::instruction::{InstOpcode, Store, Load};

/// Count the number of use.
fn analysis(module: &Module) -> Vec<usize> {
  let mut cnt: Vec<usize> = Vec::new();
  cnt.resize(module.context.capacity(), 0);
  for func in module.iter() {
    let mut addr_update : HashMap<usize, usize> = HashMap::new();
    for block in func.iter() {
      let block = block.as_ref::<Block>(&module.context).unwrap();
      for inst_ref in block.iter() {
        let inst = inst_ref.as_ref::<Instruction>(&module.context).unwrap();
        match inst.get_opcode() {
          InstOpcode::Return | InstOpcode::Call | InstOpcode::Branch => {
            cnt[inst.get_skey()] = 1;
          },
          InstOpcode::Store(align) => {
            let store = Store::new(inst, *align);
            if let Some(inst_addr) = store.get_ptr().as_ref::<Instruction>(&module.context) {
              match inst_addr.get_opcode() {
                // Record an alloca address is updated by a store.
                InstOpcode::Alloca(_) => { addr_update.insert(inst_addr.get_skey(), inst.get_skey()); }
                // If this store is not updating an alloca address, anyways keep it.
                _ => { cnt[inst.get_skey()] = 1; }
              }
            }
          }
          InstOpcode::Load(align) => {
            let load = Load::new(inst, *align);
            if let Some(inst_addr) = load.get_ptr().as_ref::<Instruction>(&module.context) {
              match inst_addr.get_opcode() {
                // Record an alloca address is used by a load.
                InstOpcode::Alloca(_) => {
                  if let Some(store_op) = addr_update.get(&inst_addr.get_skey()) {
                    cnt[*store_op] += 1;
                  }
                }
                _ => {}
              }
            }
          }
          _ => {}
        }
        let n_operands = inst.get_num_operands();
        for idx in 0..n_operands {
          cnt[inst.get_operand(idx).skey] += 1;
        }
      }
    }
  }
  return cnt;
}

pub fn transform(module: &mut Module) {
  let mut iterative = true;
  while iterative {
    let cnt = analysis(module);
    let mut to_remove : Vec<ValueRef> = Vec::new();
    for func in module.iter() {
      for block_ref in func.iter() {
        let block = block_ref.as_ref::<Block>(&module.context).unwrap();
        block.iter().for_each(|x| if cnt[x.skey] == 0 { to_remove.push(x); });
      }
    }
    iterative = to_remove.len() != 0;
    for elem in to_remove {
      module.remove_inst(elem)
    }
  }
}
