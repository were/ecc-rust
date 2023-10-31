use std::collections::{HashSet, HashMap};

use trinity::ir::{module::Module, value::block::BlockRef, Block};

pub struct Reachability {
  data: HashMap<usize, HashSet<usize>>,
}

impl Reachability {

  pub fn new(m: &Module) -> Self {
    let mut res = Self{ data: HashMap::new() };
    for f in m.func_iter() {
      for bb in f.block_iter() {
        let mut q = vec![bb];
        let mut visited = HashSet::new();
        visited.insert(bb.get_skey());
        while let Some(front) = q.pop() {
          for succ in front.succ_iter() {
            if !visited.contains(&succ.get_skey()) {
              visited.insert(succ.get_skey());
              q.push(succ);
            }
          }
        }
        res.data.insert(bb.get_skey(), visited);
      }
    }
    res
  }

  /// Check if given block b0 can reach b1.
  pub fn reachable(&self, b0: &BlockRef, b1: &BlockRef) -> bool {
    if let Some(entry) = self.data.get(&b0.get_skey()) {
      return entry.contains(&b1.get_skey());
    }
    unreachable!("{} is NOT in reachability database!", b0.to_string(false));
  }

  /// Return the slices between the given two blocks.
  pub fn slice(&self, b0: &BlockRef, b1: &BlockRef) -> Vec<BlockRef> {
    if let Some(entry) = self.data.get(&b0.get_skey()) {
      return entry.iter().filter(|x| {
        let bb = Block::from_skey(**x).as_ref::<Block>(b0.ctx()).unwrap();
        return self.reachable(b0, &bb) && self.reachable(&bb, b1);
      }).map(|x| {
        Block::from_skey(*x).as_ref::<Block>(b0.ctx()).unwrap()
      }).collect::<Vec<_>>();
    }
    unreachable!("{} is NOT in reachability database!", b0.to_string(false));
  }

}

pub fn analyze_reachability(m: &Module) {
}
