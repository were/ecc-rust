use std::collections::HashMap;

use trinity::ir::{module::Module, value::instruction::{InstMutator, InstOpcode}, Instruction, ValueRef};

use crate::analysis::dom_tree::DominatorTree;

use super::dce;

fn analysis<'ctx>(module: &'ctx Module, dt: &DominatorTree) -> Vec<(ValueRef, Vec<ValueRef>)> {
  let mut to_eliminate = HashMap::new();
  for func in module.func_iter() {
    for block in func.block_iter() {
      for inst in block.inst_iter() {
        match inst.get_opcode() {
          InstOpcode::BinaryOp(_) | InstOpcode::GetElementPtr(_) | InstOpcode::CastInst(_) | InstOpcode::ICompare(_) => {
            let operands = (0..inst.get_num_operands())
              .map(|i| inst.get_operand(i).unwrap().clone())
              .collect::<Vec<_>>();
            let key = (inst.get_opcode().clone(), operands);
            if !to_eliminate.contains_key(&key) {
              to_eliminate.insert(key.clone(), Vec::new());
            }
            to_eliminate.get_mut(&key).unwrap().push(Instruction::from_skey(inst.get_skey()));
          },
          _ => {}
        }
      }
    }
  }
  let mut res = Vec::new();
  for (_, insts) in to_eliminate {
    if insts.len() < 2 {
      continue;
    }
    for inst in insts.iter() {
      let a = inst.as_ref::<Instruction>(&module.context).unwrap();
      if insts.iter().all(|b| {
        let b = b.as_ref::<Instruction>(&module.context).unwrap();
        dt.a_dominates_b(&a, &b)
      }) {
        let x = insts.iter()
          .filter(|x| x.skey != inst.skey)
          .map(|x| x.clone())
          .collect::<Vec<_>>();
        res.push((inst.clone(), x));
        break;
      }
    }
  }
  return res;
}

pub fn rewrite(module: &mut Module, to_replace: Vec<(ValueRef, Vec<ValueRef>)>) -> bool {
  let mut res = false;
  for (dom, subs) in to_replace.iter() {
    eprintln!("[CSE] Master: {}", dom.as_ref::<Instruction>(&module.context).unwrap().to_string(false));
    for sub in subs {
      eprintln!("[CSE] To replace: {}", sub.as_ref::<Instruction>(&module.context).unwrap().to_string(false));
      let mut mutator = InstMutator::new(&mut module.context, sub);
      mutator.replace_all_uses_with(dom.clone());
      res = true;
    }
  }
  return res;
}

pub fn transform(mut module: Module, dt: &DominatorTree) -> Module {
  loop {
    let to_replace = analysis(&module, dt);
    if !rewrite(&mut module, to_replace) {
      break;
    }
    dce::transform(&mut module);
  }
  module
}
