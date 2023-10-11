use trinity::ir::Instruction;
use trinity::ir::{module::Module, ValueRef};
use trinity::ir::value::instruction::{InstOpcode, InstMutator};

/// Count the number of use.
fn analysis(module: &Module) -> Vec<ValueRef> {
  let mut res = Vec::new();
  for func in module.func_iter() {
    for block in func.block_iter() {
      for inst in block.inst_iter() {
        match inst.get_opcode() {
          InstOpcode::Return | InstOpcode::Call | InstOpcode::Branch(_) | InstOpcode::Store(_) => {
            continue;
          },
          _ => {
            let mut users = inst.user_iter();
            if users.next().is_none() {
              res.push(inst.as_super());
            }
          }
        }
      }
    }
  }
  res
}

pub fn transform(module: &mut Module) -> bool {
  let mut iterative = true;
  let mut modified = false;
  while iterative {
    let to_remove = analysis(&module);
    iterative = to_remove.len() != 0;
    modified |= iterative;
    for elem in to_remove {
      let log = elem.as_ref::<Instruction>(&module.context).unwrap().to_string(false);
      // eprintln!("[DCE] Remove {}, due to no user.", log);
      let mut mutator = InstMutator::new(&mut module.context, &elem);
      mutator.erase_from_parent();
    }
  }
  return modified;
}
