use std::collections::{HashMap, HashSet};

use trinity::ir::{module::Module, value::{instruction::Call, function::FunctionRef}};

pub struct CallGraph {
  /// The raw graph
  g: HashMap<usize, HashSet<usize>>,
  /// The scc each function belongs to
  scc: HashMap<usize, usize>,
  /// The size of each scc
  size: HashMap<usize, usize>,
}

impl CallGraph {

  /// If the given function is not self recursive.
  pub fn is_self_recursive(&self, f: &FunctionRef) -> bool {
    self.g.get(&f.get_skey()).unwrap().contains(&f.get_skey())
  }

  /// If the given function is not dag recursive.
  pub fn is_non_recursive(&self, f: &FunctionRef) -> bool {
    let scc = self.scc.get(&f.get_skey()).unwrap();
    if *self.size.get(&scc).unwrap() == 1 {
      return !self.is_self_recursive(f);
    }
    return false;
  }

}

struct DFS {
  g: HashMap<usize, HashSet<usize>>,
  cur_stamp: usize,
  stamp: HashMap<usize, usize>,
  min_stamp: HashMap<usize, usize>,
  stack: Vec<usize>,
  scc: HashMap<usize, usize>,
  size: HashMap<usize, usize>,
}

impl DFS {

  fn new(g: HashMap<usize, HashSet<usize>>) -> Self {
    let funcs = g.iter().map(|(x, _)| *x).collect::<Vec<_>>();
    let mut res = Self {
      g,
      cur_stamp: 0,
      stamp: HashMap::new(),
      min_stamp: HashMap::new(),
      stack: Vec::new(),
      scc: HashMap::new(),
      size: HashMap::new(),
    };
    for func in funcs {
      if !res.stamp.contains_key(&func) {
        res.run(func);
      }
    }
    res
  }

  fn run(&mut self, src: usize) {
    self.cur_stamp += 1;
    self.stamp.insert(src, self.cur_stamp);
    self.min_stamp.insert(src, self.cur_stamp);
    self.stack.push(src);
    let edges = self.g.get(&src).unwrap().iter().map(|x| *x).collect::<Vec<_>>();
    for edge in edges {
      let min = if !self.stamp.contains_key(&edge) {
        self.run(edge);
        *self.min_stamp.get(&src).unwrap().min(self.min_stamp.get(&edge).unwrap())
      } else {
        *self.min_stamp.get(&src).unwrap().min(self.stamp.get(&edge).unwrap())
      };
      *self.min_stamp.get_mut(&src).unwrap() = min;
    }
    let cur_min_stamp = self.min_stamp.get(&src).unwrap();
    let mut cnt = 0;
    while !self.stack.is_empty() &&
          *self.min_stamp.get(self.stack.last().unwrap()).unwrap() == *cur_min_stamp {
      self.scc.insert(self.stack.pop().unwrap(), src);
      cnt += 1;
    }
    self.size.insert(src, cnt);
  }


}


pub fn analyze(m: &Module) -> CallGraph {

  let mut raw_graph:HashMap<usize, HashSet<usize>> = HashMap::new();
  for func in m.func_iter() {
    raw_graph.insert(func.get_skey(), HashSet::new());
    let edges = raw_graph.get_mut(&func.get_skey()).unwrap();
    for bb in func.block_iter() {
      for inst in bb.inst_iter() {
        if let Some(call) = inst.as_sub::<Call>() {
          edges.insert(call.get_callee().get_skey());
        }
      }
    }
  }

  let dfs = DFS::new(raw_graph);

  CallGraph { g: dfs.g, scc: dfs.scc, size: dfs.size }

}


