use std::collections::{HashMap, HashSet};

use trinity::ir::{module::Module, Block, value::instruction::{Store, InstOpcode, Load}, Instruction, ValueRef, Function};

fn graph_dominator(func: &Function, idom: &mut Vec<usize>) {
  let dom: HashSet<usize> = HashSet::new();

}

pub fn transform(module: &mut Module) {
  let mut idom = Vec::new();
  for func in module.iter() {
    let idom = graph_dominator(func, &mut idom);
  }
}

