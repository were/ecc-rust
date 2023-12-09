use trinity::ir::{module::Module, ValueRef, value::instruction::{Call, InstMutator}, Instruction};

use crate::analysis::lifetime::VarLifetime;

fn has_lifetime_hint(module: &Module) -> Vec<ValueRef> {
  let mut res = Vec::new();
  for func in module.func_iter() {
    for block in func.block_iter() {
      for inst in block.inst_iter() {
        if let Some(call) = inst.as_sub::<Call>() {
          match call.get_callee().get_name().as_str() {
            "llvm.lifetime.end" | "llvm.lifetime.start" => {
              res.push(inst.as_super());
            }
            _ => {}
          }
        }
      }
    }
  }
  return res;
}

pub fn remove_lifetime_hint(module: &mut Module, remove_func_decl: bool) {
  let hints = has_lifetime_hint(module);
  for to_remove in hints.into_iter() {
    let mut inst = InstMutator::new(&mut module.context, &to_remove);
    inst.erase_from_parent();
  }
  if remove_func_decl {
    let mut to_remove = Vec::new();
    for func in module.func_iter() {
      match func.get_name().as_str() {
        "llvm.lifetime.end" | "llvm.lifetime.start" => {
          to_remove.push(func.as_super());
        }
        _ => {}
      }
    }
    for func in to_remove.into_iter() {
      module.remove_func(func)
    }
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

