use trinity::ir::Instruction;
use trinity::ir::{module::Module, ValueRef};
use trinity::ir::value::instruction::{InstOpcode, InstMutator};

/// Count the number of use.
fn analysis(module: &Module) -> Vec<usize> {
  let mut cnt: Vec<usize> = Vec::new();
  cnt.resize(module.context.capacity(), 0);
  for func in module.iter() {
    for block in func.iter() {
      for inst in block.inst_iter() {
        match inst.get_opcode() {
          InstOpcode::Return | InstOpcode::Call | InstOpcode::Branch | InstOpcode::Store(_) => {
            cnt[inst.get_skey()] = 1;
          },
          _ => {}
        }
        let n_operands = inst.get_num_operands();
        for idx in 0..n_operands {
          cnt[inst.get_operand(idx).unwrap().skey] += 1;
        }
      }
    }
  }
  return cnt;
}

pub fn transform(module: &mut Module) {
  let mut iterative = true;
  while iterative {
    let cnt = analysis(&module);
    let mut to_remove : Vec<ValueRef> = Vec::new();
    for func in module.iter() {
      for block in func.iter() {
        block.inst_iter().for_each(|x| {
          if cnt[x.get_skey()] == 0 {
            to_remove.push(Instruction::from_skey(x.get_skey()));
          }
        });
      }
    }
    iterative = to_remove.len() != 0;
    for elem in to_remove {
      // eprintln!("remove {}", elem.skey);
      let mut mutator = InstMutator::new(&mut module.context, &elem);
      mutator.erase_from_parent();
    }
  }
}
