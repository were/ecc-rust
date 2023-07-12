use trinity::ir::Instruction;
use trinity::ir::{module::Module, ValueRef};
use trinity::ir::value::instruction::{InstOpcode, InstMutator};

/// Count the number of use.
fn analysis(module: &Module) -> Vec<ValueRef> {
  let mut res = Vec::new();
  for func in module.iter() {
    for block in func.iter() {
      for inst in block.inst_iter() {
        match inst.get_opcode() {
          InstOpcode::Return | InstOpcode::Call | InstOpcode::Branch | InstOpcode::Store(_) => {
            continue;
          },
          _ => {
            let mut users = inst.user_iter();
            if users.next().is_none() {
              res.push(Instruction::from_skey(inst.get_skey()));
            }
          }
        }
      }
    }
  }
  res
}

pub fn transform(module: &mut Module) {
  let mut iterative = true;
  while iterative {
    let to_remove = analysis(&module);
    iterative = to_remove.len() != 0;
    for elem in to_remove {
      let mut mutator = InstMutator::new(&mut module.context, &elem);
      mutator.erase_from_parent();
    }
  }
}
