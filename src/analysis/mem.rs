use std::collections::HashMap;

use trinity::ir::{ValueRef, module::Module, value::instruction::{Call, InstOpcode}, Instruction};

#[derive(Clone)]
pub enum AliasInfo {
  Array(ValueRef),
  None,
}

pub struct AliasCache {
  data: HashMap<usize, AliasInfo>,
}

impl AliasCache {

  pub fn new(m: &Module) -> Self {
    let mut data = HashMap::new();
    let mut visited = vec![false; m.context.capacity()];
    let mut propagate = vec![AliasInfo::None; m.context.capacity()];
    for f in m.func_iter() {
      let mut q = vec![];
      for bb in f.block_iter() {
        for inst in bb.inst_iter() {
          if let Some(call) = inst.as_sub::<Call>() {
            if call.get_callee().get_name() == "malloc" {
              q.push(inst.as_super().as_ref::<Instruction>(&m.context).unwrap());
              visited[inst.get_skey()] = true;
              propagate[inst.get_skey()] = AliasInfo::Array(inst.as_super());
            }
          }
        }
      }
      while let Some(front) = q.pop() {
        for user in front.user_iter() {
          if !visited[user.get_skey()] {
            visited[user.get_skey()] = true;
            q.push(user.as_super().as_ref::<Instruction>(&m.context).unwrap());
            propagate[user.get_skey()] = propagate[front.get_skey()].clone();
          }
        }
      }
    }
    propagate.iter().enumerate().filter(|(idx, value)| match value {
      AliasInfo::Array(_) => {
        if let Some(inst) = Instruction::from_skey(*idx).as_ref::<Instruction>(&m.context) {
          match inst.get_opcode() {
            InstOpcode::Load(_) | InstOpcode::Store(_) | InstOpcode::GetElementPtr(_) => true,
            _ => false,
          }
        } else {
          false
        }
      }
      _ => false,
    }).for_each(|(idx, value)| {
      data.insert(idx, value.clone());
    });
    Self { data }
  }

  pub fn get(&self, val: &ValueRef) -> AliasInfo {
    match self.data.get(&val.skey) {
      Some(info) => info.clone(),
      None => AliasInfo::None,
    }
  }

}

