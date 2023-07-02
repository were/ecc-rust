use std::collections::HashSet;

use trinity::{ir::{module::Module, Block, value::instruction::{Store, InstOpcode, Load}, Instruction, ValueRef, Function}, context::{Context, component::GetSlabKey}, builder::Builder};

fn analyze_dominance_frontiers(ctx: &Context, func: &Function, idom: &mut Vec<usize>) -> Vec<HashSet<usize>> {
  // Calculate the dominators
  let mut dominators:Vec<HashSet<usize>> = Vec::new();
  idom.resize(func.get_num_blocks(), 0);
  dominators.resize(func.get_num_blocks(), HashSet::new());
  dominators[0].insert(0);
  let universal = (0..func.get_num_blocks()).collect::<HashSet<usize>>();
  for i in 1..func.get_num_blocks() {
    dominators[i] = universal.clone();
  }
  // TODO(@were): Engineer this to a more efficient implementation.
  let mut changed = true;
  while changed {
    changed = false;
    for i in 1..func.get_num_blocks() {
      let block = func.get_block(i);
      let mut new_doms = dominators[i].clone();
      let block = block.as_ref::<Block>(&ctx).unwrap();
      for pred in 0..block.get_num_predecessors() {
        let pred_inst = block.get_predecessor(pred);
        let br = pred_inst.as_ref::<Instruction>(&ctx).unwrap();
        let pred_block = br.get_parent().skey;
        let idx = func.basic_blocks().iter().position(|x| *x == pred_block).unwrap();
        new_doms = new_doms.intersection(&dominators[idx]).cloned().collect();
      }
      new_doms.insert(i);
      if new_doms != dominators[i] {
        changed = true;
        dominators[i] = new_doms;
      }
    }
  }
  eprintln!("In function {}:", func.get_name());
  for i in 0..func.get_num_blocks() {
    let block = func.get_block(i);
    eprintln!("  Block {} dominated by:", block.to_string(ctx, false));
    for dom in dominators[i].iter() {
      eprintln!("    {}", func.get_block(*dom).to_string(ctx, false));
      if *dom != i {
        idom[i] = *dom;
      }
    }
    eprintln!("");
  }
  // Calculate the dominance frontiers
  let mut frontiers:Vec<HashSet<usize>> = Vec::new();
  frontiers.resize(func.get_num_blocks(), HashSet::new());
  for (cur_idx, block) in func.iter().enumerate() {
    let block = block.as_ref::<Block>(ctx).unwrap();
    if block.get_num_predecessors() > 1 {
      for pred in 0..block.get_num_predecessors() {
        let pred_block = block
          .get_predecessor(pred)
          .as_ref::<Instruction>(ctx)
          .unwrap()
          .get_parent()
          .as_ref::<Block>(ctx)
          .unwrap()
          .get_skey();
        let mut runner = func.basic_blocks().iter().position(|x| pred_block == *x).unwrap();
        while runner != idom[cur_idx] {
          frontiers[runner].insert(cur_idx);
          runner = idom[runner];
        }
      }
    }
  }
  for i in 0..func.get_num_blocks() {
    eprintln!("  Block {} frontiers:", func.get_block(i).to_string(ctx, false));
    for frontier in frontiers[i].iter() {
      eprintln!("    {}", func.get_block(*frontier).to_string(ctx, false));
    }
    eprintln!("");
  }
  frontiers
}

pub fn transform(module: Module) -> Module {
  let mut dominators:Vec<Vec<usize>> = Vec::new();
  let mut builder = Builder::new(module);
  for func in builder.module.iter() {
    dominators.push(Vec::new());
    if func.get_num_blocks() != 0 {
      let frontiers = analyze_dominance_frontiers(&builder.module.context, func, dominators.last_mut().unwrap());
    }
  }
  builder.module
}

