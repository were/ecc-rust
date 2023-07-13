use std::collections::HashMap;

use trinity::ir::{module::Module, value::instruction::{BinaryInst, InstMutator}, Instruction, ValueRef};

use super::{ssa::{DomInfo, a_dominates_b}, dce};

fn analysis<'ctx>(module: &'ctx Module, dom: &Vec<DomInfo>) -> Vec<(ValueRef, Vec<ValueRef>)> {
  let mut to_eliminate = HashMap::new();
  for func in module.iter() {
    for block in func.iter() {
      for inst in block.inst_iter() {
        if let Some(bin) = inst.as_sub::<BinaryInst>() {
          let key = (bin.get_op(), bin.lhs().clone(), bin.rhs().clone());
          if !to_eliminate.contains_key(&key) {
            to_eliminate.insert(key.clone(), Vec::new());
          }
          to_eliminate.get_mut(&key).unwrap().push(Instruction::from_skey(inst.get_skey()));
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
        a_dominates_b(dom, &a, &b)
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

pub fn transform(mut module: Module, dom: &Vec<DomInfo>) -> Module {
  loop {
    let to_replace = analysis(&module, dom);
    if !rewrite(&mut module, to_replace) {
      break;
    }
    dce::transform(&mut module);
  }
  module
}
