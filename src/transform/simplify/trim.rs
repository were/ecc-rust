use trinity::ir::{module::Module, Function, value::instruction::{InstMutator, InstOpcode}};

pub fn remove_uncalled_functions(m: &mut Module) -> bool {
  let mut i = 0;
  loop {
    let to_remove = m.func_iter().filter_map(|func| {
      if func.get_name() == "main" {
        return None
      }
      if func.user_iter().next().is_none() {
        Some(func.as_super())
      } else {
        None
      }
    }).collect::<Vec<_>>();
    if to_remove.is_empty() {
      return i != 0;
    }
    i += 1;
    for elem in to_remove.into_iter() {
      let func = elem.as_ref::<Function>(&m.context).unwrap();
      let mut to_maintain = Vec::new();
      for bb in func.block_iter() {
        for inst in bb.inst_iter() {
          if let InstOpcode::Call(_) = inst.get_opcode() {
            to_maintain.push(inst.as_super());
          }
        }
      }
      for inst in to_maintain.into_iter() {
        let ty = inst.get_type(&m.context);
        let undef = m.context.undef(ty);
        let mut mutator = InstMutator::new(&mut m.context, &inst);
        mutator.replace_all_uses_with(undef);
        mutator.erase_from_parent();
      }
      m.remove_func(elem);
    }
  }
}
