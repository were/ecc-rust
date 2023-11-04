use trinity::{
  context::Context,
  ir::{module::Module, value::instruction::{PhiNode, InstMutator, BranchInst}, ConstScalar, ValueRef, TKindCode, Instruction, Block}, builder::Builder 
};

use crate::analysis::topo::{analyze_topology, ChildIter, Node, ChildTraverse};

fn is_const(ctx: &Context, value: &ValueRef, raw: u64) -> bool {
  if let Some(cs) = value.as_ref::<ConstScalar>(ctx) {
    return *cs.get_type().kind() == TKindCode::IntType && cs.get_value() == raw
  }
  return false;
}

fn loops_to_canonicalize(
  iter: ChildIter, to_canonicalize: &mut Vec<(ValueRef, ValueRef, ValueRef, usize, ValueRef)>) {
  for elem in iter {
    match elem {
      Node::Block(_) => {},
      Node::Loop(li) => {
        loops_to_canonicalize(li.child_iter(), to_canonicalize);
        if let Some(ind_var) = li.get_loop_ind_var() {
          let prehead = li.get_prehead();
          if let Some(step) = li.get_loop_step() {
            if is_const(prehead.ctx(), &step, 1) {
              if let Some(end) = li.get_loop_n() {
                if li.is_loop_invariant(&end) {
                  let phi = ind_var.as_sub::<PhiNode>().unwrap();
                  for (idx, (block, init)) in phi.iter().enumerate() {
                    if block.get_skey() == prehead.get_skey() {
                      if !is_const(block.ctx(), init, 0) {
                        let prehead = prehead.as_super();
                        let latch = li.get_latch().as_super();
                        let ind_var = ind_var.as_super();
                        to_canonicalize.push((prehead, latch, ind_var, idx * 2, end.clone()));
                        break;
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

pub fn transform(m: Module) -> Module {
  let mut to_canonicalize = vec![];
  let topo = analyze_topology(&m);
  for f in m.func_iter().filter(|x| !x.is_declaration()) {
    let func_info = topo.get_function(f.get_skey());
    loops_to_canonicalize(func_info.child_iter(), &mut to_canonicalize);
  }
  let mut builder = Builder::new(m);
  for (prehead, latch, ind, idx, end) in to_canonicalize {
    // Build the "i32 0" const for init replace.
    let i32ty = builder.context().int_type(32);
    let zero = builder.context().const_value(i32ty, 0);
    // Analyze the values to replace.
    let inst = ind.as_ref::<Instruction>(&builder.module.context).unwrap();
    // eprintln!("ind: {}", inst.to_string(false));
    let latch_inst = latch.as_ref::<Instruction>(&builder.module.context).unwrap();
    let latch_br = latch_inst.as_sub::<BranchInst>().unwrap();
    let cond = latch_br.cond().unwrap().clone();
    let mut to_replace = vec![];
    // Manually gather all the instructions whose operands should be replaced by (i + init).
    for user in inst.user_iter() {
      if inst.operand_iter().any(|x| x.skey == user.get_skey()) {
        // This is the accumulator. i = i + 1, which should NOT be canonicalized.
      } else if user.get_skey() == cond.skey {
        // This is i < n, which should NOT be canonicalized.
        // Replace n with (end - init), and put the expression in the prehead.
      } else {
        // This is just a normal use, replace this i with (i + init)
        for (i, operand) in user.operand_iter().enumerate() {
          if operand.skey == inst.get_skey() {
            to_replace.push((user.get_skey(), i));
          }
        }
      }
    }
    // Get the init.
    let init = inst.get_operand(idx).unwrap().clone();
    // eprintln!("start: {}", init.to_string(&builder.module.context, true));
    let block = inst.get_parent();
    let ind_idx = block.inst_iter().position(|i| i.get_skey() == inst.get_skey()).unwrap() + 1;
    let next_inst = block.get_inst(ind_idx).unwrap().as_super();
    let ind_phi = inst.as_super();
    let block = block.as_super();
    // Calculate extent = end - begin.
    let prehead_block = prehead.as_ref::<Block>(&builder.module.context).unwrap();
    let last_inst = prehead_block.last_inst().unwrap();
    let last_inst = last_inst.as_super();
    // Insert it to the end of the prehead.
    builder.set_current_block(prehead.clone());
    builder.set_insert_before(last_inst);
    let extent = builder.create_sub(end, init.clone());
    // eprintln!("{}", prehead.as_ref::<Block>(&builder.module.context).unwrap().to_string(false));

    // Insert (i + init) right after the phi inductive.
    builder.set_current_block(block);
    builder.set_insert_before(next_inst);
    // Build (i + init)
    let add = builder.create_add(ind_phi.clone(), init);
    let mut phi = InstMutator::new(&mut builder.module.context, &ind_phi);
    // Replace the init with zero.
    phi.set_operand(idx, zero);
    // Replace i < n with i < extent.
    let mut latch_cond = InstMutator::new(&mut builder.module.context, &cond);
    latch_cond.set_operand(1, extent);
    // Replace all the qualified uses.
    for (skey, idx) in to_replace {
      let user = Instruction::from_skey(skey);
      let mut user_mutator = InstMutator::new(&mut builder.module.context, &user);
      user_mutator.set_operand(idx, add.clone());
    }
  }
  builder.module
}

