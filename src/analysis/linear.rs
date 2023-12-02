use std::collections::HashMap;

use trinity::ir::{ValueRef, module::Module, value::instruction::{InstOpcode, BinaryOp}};

pub struct LinearCombine {
  combine: HashMap<ValueRef, i64>,
  count: usize
}

impl LinearCombine {

  /// The number of linear combine terms.
  pub fn num_terms(&self) -> usize {
    return self.combine.len();
  }

  pub fn num_insts(&self) -> usize {
    return self.count;
  }

  /// Iterate over the terms.
  pub fn iter(&self) -> impl Iterator<Item = (&ValueRef, &i64)> {
    self.combine.iter()
  }

  /// If this linear combine is a primitive.
  pub fn is_primitive(&self) -> bool {
    return self.count == 0;
  }
}

/// Linear combination cache.
pub struct LCCache {
  /// The original value, (the instruction count, the linear combination)
  cache: HashMap<ValueRef, LinearCombine>
}

impl LCCache {

  pub fn get(&self, v: &ValueRef) -> Option<&LinearCombine> {
    self.cache.get(v)
  }

  pub fn iter(&self) -> impl Iterator<Item = (&ValueRef, &LinearCombine)> {
    self.cache.iter()
  }

  /// Linearize all the add/sub instructions.
  pub fn new(m: &Module) -> Self {
    let mut res = Self { cache: HashMap::new() };
    // initialize all the primitives
    for f in m.func_iter() {
      for bb in f.block_iter() {
        for i in bb.inst_iter() {
          match i.get_opcode() {
            InstOpcode::BinaryOp(BinaryOp::Add) => {}
            InstOpcode::BinaryOp(BinaryOp::Sub) => {}
            InstOpcode::Branch(_) => {}
            InstOpcode::Store(_) => {}
            _ => {
              let lc = HashMap::from([(i.as_super(), 1)]);
              let v = LinearCombine { combine: lc, count: 0 };
              res.cache.insert(i.as_super(), v);
            }
          }
        }
      }
    }
    let mut iterative = true;
    while iterative {
      iterative = false;
      for f in m.func_iter() {
        for bb in f.block_iter() {
          for i in bb.inst_iter() {
            if res.cache.contains_key(&i.as_super()) {
              continue;
            }
            let coef = match i.get_opcode() {
              InstOpcode::BinaryOp(BinaryOp::Add) => Some(1),
              InstOpcode::BinaryOp(BinaryOp::Sub) => Some(-1),
              _ => None
            };
            if let Some(coef) = coef {
              let lhs = i.get_operand(0).unwrap();
              let rhs = i.get_operand(1).unwrap();
              if let (Some(lhs), Some(rhs)) = (res.get(&lhs), res.get(rhs)) {
                iterative = true;
                let mut lc = lhs.combine.clone();
                let count = lhs.count.max(rhs.count) + 1;
                for (k, v) in rhs.combine.iter() {
                  if let Some(orig) = lc.get_mut(k) {
                    *orig += coef * v;
                  } else {
                    lc.insert(k.clone(), coef * v);
                  }
                }
                let zeros = lc.iter().filter_map(|(k, v)| {
                  if *v == 0 {
                    Some(k.clone())
                  } else {
                    None
                  }
                }).collect::<Vec<_>>();
                zeros.iter().for_each(|k| { lc.remove(k); });
                let v = LinearCombine { combine: lc, count };
                res.cache.insert(i.as_super(), v);
              }
            }
          }
        }
      }
    }
    res
  }

}
