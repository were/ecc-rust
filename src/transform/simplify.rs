use trinity::ir::{module::Module, value::instruction::{PhiNode, InstMutator}, ValueRef, Instruction};

fn has_trivial_phi(module: &Module) -> Option<(usize, ValueRef)> {
  for func in module.func_iter() {
    for block in func.block_iter() {
      for inst in block.inst_iter() {
        if let Some(phi) = inst.as_sub::<PhiNode>() {
          let value = phi.get_incoming_value(0).unwrap();
          if phi.iter().all(|(_, v)| v.skey == value.skey) {
            eprintln!("[SSA] Find a trivial phi: {}, replace by: {}", inst.to_string(false), value.to_string(&module.context, true));
            return Some((inst.get_skey(), value.clone()));
          }
        }
      }
    }
  }
  None
}

pub fn remove_trivial_phi(module: &mut Module) {
  while let Some((phi, value)) = has_trivial_phi(&module) {
    let phi = Instruction::from_skey(phi);
    let mut phi = InstMutator::new(&mut module.context, &phi);
    phi.replace_all_uses_with(value);
    phi.erase_from_parent();
  }
}
