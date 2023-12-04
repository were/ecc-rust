use std::collections::HashMap;

use trinity::{ir::{ValueRef, module::Module, value::instruction::{Call, Alloca}, VKindCode, Instruction}, context::WithSuperType};

pub struct VarLifetime {
  /// The cache of the given instruction.
  cache: HashMap<usize, (ValueRef, ValueRef)>
}

impl VarLifetime {

  pub fn new(module: &Module, verify: bool) -> Self {
    let mut cache = HashMap::new();
    for func in module.func_iter() {
      for block in func.block_iter() {
        for inst in block.inst_iter() {
          if let Some(_) = inst.as_sub::<Alloca>() {
            assert!(!cache.contains_key(&inst.get_skey()));
            let u0 = ValueRef {skey: 0, kind: VKindCode::Undef};
            let u1 = ValueRef {skey: 0, kind: VKindCode::Undef};
            cache.insert(inst.get_skey(), (u0, u1));
          }
        }
      }
    }
    for func in module.func_iter() {
      for block in func.block_iter() {
        for inst in block.inst_iter() {
          if let Some(call) = inst.as_sub::<Call>() {
            if call.get_callee().get_name() == "llvm.lifetime.start" {
              cache.get_mut(&call.get_arg(1).skey).unwrap().0 = inst.as_super();
            }
            if call.get_callee().get_name() == "llvm.lifetime.end" {
              cache.get_mut(&call.get_arg(1).skey).unwrap().1 = inst.as_super();
            }
          }
        }
      }
    }
    cache.retain(|_, v| v.0.skey != 0 && v.1.skey != 0);
    if verify {
      for (k, v) in cache.iter() {
        if v.0.skey == 0 {
          let inst = Instruction::from_skey(*k).as_ref::<Instruction>(&module.context).unwrap();
          panic!("{} missing lifetime.start", inst.to_string(false));
        }
        if v.1.skey == 0 {
          let inst = Instruction::from_skey(*k).as_ref::<Instruction>(&module.context).unwrap();
          panic!("{} missing lifetime.end", inst.to_string(false));
        }
      }
    }
    return Self { cache };
  }

  pub fn get(&self, inst: usize) -> Option<(ValueRef, ValueRef)> {
    self.cache.get(&inst).map(|x| x.clone())
  }

  pub fn iter(&self) -> impl Iterator<Item = (&usize, &(ValueRef, ValueRef))> {
    self.cache.iter()
  }

}
