use trinity::ir::{module::Module, ValueRef, value::instruction::{Call, InstMutator}, Instruction};

use crate::analysis::lifetime::VarLifetime;


fn has_lifetime_hint(module: &Module) -> Option<ValueRef> {
  for func in module.func_iter() {
    for block in func.block_iter() {
      for inst in block.inst_iter() {
        if let Some(call) = inst.as_sub::<Call>() {
          match call.get_callee().get_name().as_str() {
            "llvm.lifetime.end" | "llvm.lifetime.start" => {
              return Some(inst.as_super())
            }
            _ => {}
          }
        }
      }
    }
  }
  None
}

pub fn remove_lifetime_hint(module: &mut Module) {
  while let Some(to_remove) = has_lifetime_hint(module) {
    let mut inst = InstMutator::new(&mut module.context, &to_remove);
    inst.erase_from_parent();
  }
}

pub fn remove_unpaired_lifetime(module: &mut Module) {
  let vlt = VarLifetime::new(module, false);
  for (_, (start, end)) in vlt.iter() {
    let hints = [start, end];
    for i in 0..2 {
      if hints[i].skey == 0 {
        if hints[1 - i].skey != 0 {
          eprintln!("[WARNING] unpaired lifetime hint! {}",
            hints[1 - i].as_ref::<Instruction>(&module.context).unwrap().to_string(false));
          let mut inst = InstMutator::new(&mut module.context, hints[1 - i]);
          inst.erase_from_parent();
          break;
        }
      }
    }
  }
}

