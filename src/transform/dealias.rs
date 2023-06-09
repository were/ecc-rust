use std::collections::HashMap;

use trinity::ir::{module::Module, Block, value::instruction::{Store, InstOpcode, Load}, Instruction, ValueRef};

fn analysis(module: &Module) -> Vec<(ValueRef, usize, ValueRef)>{
  let mut res : Vec<(ValueRef, usize, ValueRef)> = Vec::new();
  for func in module.iter() {
    for block in func.iter() {
      let mut kv : HashMap<ValueRef, ValueRef> = HashMap::new();
      let block = block.as_ref::<Block>(&module.context).unwrap();
      for inst_ref in block.iter() {
        let inst = inst_ref.as_ref::<Instruction>(&module.context).unwrap();
        let n_operands = inst.get_num_operands();
        for idx in 0..n_operands {
          if let Some(operand) = inst.get_operand(idx).as_ref::<Instruction>(&module.context) {
            if let InstOpcode::Load(align) = operand.get_opcode() {
              let load = Load::new(&operand, *align);
              if let Some(v) = kv.get(load.get_ptr()) {
                res.push((inst_ref.clone(), idx, v.clone()));
              }
            }
          }
        }
        if let InstOpcode::Store(align) = inst.get_opcode() {
          let store = Store::new(&inst, *align);
          kv.insert(store.get_ptr().clone(), store.get_value().clone());
        }
      }
    }
  }
  return res;
}

pub fn transform(mut module: Module) -> Module {
  let to_replace = analysis(&module);
  for (inst, idx, new_operand) in to_replace {
    let inst = inst.as_mut::<Instruction>(&mut module.context).unwrap();
    inst.set_operand(idx, new_operand)
  }
  return module;
}

