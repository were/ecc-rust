use std::collections::{HashSet, VecDeque};

use trinity::ir::{
  value::{instruction::InstructionRef, function::FunctionRef, block::BlockRef},
  Block,
  module::Module
};


pub struct DomInfo {
  dominators: HashSet<usize>,
  idom: usize,
  depth: usize,
}

impl DomInfo {
  fn new() -> Self {
    Self {
      dominators: HashSet::new(),
      idom: 0,
      depth: 0,
    }
  }

}

pub struct DominatorTree {
  dt: Vec<DomInfo>,
}

impl DominatorTree {

  pub fn new(module: &Module) -> Self {
    let mut dt = vec![];
    (0..module.context.capacity()).for_each(|_| dt.push(DomInfo::new()));

    let mut dt = Self { dt };

    for func in module.func_iter() {
      if func.get_num_blocks() != 0 {
        dt.analyze_dominators(&func);
      }
    }

    dt
  }

  pub fn block_idom(&self, block: &BlockRef) -> usize {
    let workspace = &self.dt;
    workspace[block.get_skey()].idom
  }

  pub fn is_entry(&self, block: &BlockRef) -> bool {
    return self.dt[block.get_skey()].depth == 1;
  }

  pub fn i_dominates_i(&self, a: &InstructionRef, b: &InstructionRef) -> bool {
    let workspace = &self.dt;
    let a_block = a.get_parent();
    let b_block = b.get_parent();
    if a_block.get_skey() == b_block.get_skey() {
      let block = a_block;
      let mut idx_a = 0;
      let mut idx_b = 0;
      let a_skey = a.get_skey();
      let b_skey = b.get_skey();
      for (i, inst) in block.inst_iter().enumerate() {
        if inst.get_skey() == a_skey {
          idx_a = i;
        }
        if inst.get_skey() == b_skey {
          idx_b = i;
        }
      }
      return idx_a <= idx_b;
    }
    if workspace[b_block.get_skey()].dominators.contains(&a_block.get_skey()) {
      true
    } else {
      false
    }
  }

  fn analyze_dominators(&mut self, func: &FunctionRef) {
    let workspace = &mut self.dt;
    let ctx = func.ctx();
    // Calculate the dominators
    let mut changed = true;
    while changed {
      changed = false;
      let block = func.get_block(0).unwrap();
      workspace[block.get_skey()].dominators.insert(block.get_skey());
      workspace[block.get_skey()].depth = 1;
      let mut visited = HashSet::new();
      visited.insert(block.get_skey());
      let mut q = VecDeque::new();
      q.push_back(block.as_super());
      while let Some(front) = q.pop_front() {
        let block = front.as_ref::<Block>(ctx).unwrap();
        let mut new_dom = HashSet::new();
        let mut first = true;
        for pred in block.pred_iter() {
          let pred = pred.get_parent();
          if workspace[pred.get_skey()].dominators.is_empty() {
            continue;
          }
          if first {
            new_dom = workspace[pred.get_skey()].dominators.clone();
            first = false;
          } else {
            new_dom = new_dom
              .intersection(&workspace[pred.get_skey()].dominators)
              .cloned()
              .collect::<HashSet<_>>();
          }
        }
        if !first {
          new_dom.insert(front.skey);
          if new_dom != workspace[front.skey].dominators {
            changed = true;
            assert!(new_dom.contains(&func.get_block(0).unwrap().get_skey()));
            // eprintln!("[DOM] block {} update to: {:?}", block.get_name(), new_dom);
            // for pred in block.pred_iter() {
            //   eprintln!("  pred: {}, intersects {:?}", pred.get_parent().get_name(),
            //             workspace[pred.get_parent().get_skey()].dominators);
            // }
            workspace[front.skey].dominators = new_dom;
          }
        } else {
          // eprintln!("[DOM] block {} skip for now.", block.get_name());
        }
        for succ in block.succ_iter() {
          if visited.contains(&succ.get_skey()) {
            continue;
          }
          q.push_back(succ.as_super());
          visited.insert(succ.get_skey());
        }
      }
    }
    {
      changed = true;
      while changed {
        changed = false;
        for block in func.block_iter() {
          for dom in workspace[block.get_skey()].dominators.clone().iter() {
            let dom = Block::from_skey(*dom);
            let dom = dom.as_ref::<Block>(ctx).unwrap();
            if block.get_skey() == dom.get_skey() {
              continue;
            }
            if workspace[block.get_skey()].depth < workspace[dom.get_skey()].depth + 1 {
              workspace[block.get_skey()].depth = workspace[dom.get_skey()].depth + 1;
              workspace[block.get_skey()].idom = dom.get_skey();
              changed = true;
            }
          }
        }
      }
    }
    // eprintln!("In function {}:", func.get_name());
    // for i in 0..func.get_num_blocks() {
    //   let block = func.get_block(i).unwrap();
    //   let entry = &workspace[block.get_skey()];
    //   eprintln!("  Block {} (Depth: {}), Pred: [{}] dominated by:",
    //     block.get_name(), entry.depth,
    //     block.pred_iter().map(|x| x.get_parent().get_name()).collect::<Vec<_>>().join(", "));
    //   for dom in entry.dominators.iter() {
    //     let block_ref= Block::from_skey(*dom);
    //     eprint!("    {}", block_ref.to_string(ctx, false));
    //     if *dom == entry.idom {
    //       eprintln!(" *")
    //     } else {
    //       eprintln!("")
    //     }
    //   }
    // }
  }

}
