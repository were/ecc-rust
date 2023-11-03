use std::collections::HashMap;

use trinity::{
  ir::{
    module::Module, ConstScalar, ValueRef, Block,
    value::{instruction::{BranchInst, InstMutator, PhiNode}, block::BlockMutator},
    Instruction
  },
  builder::Builder
};

use crate::analysis::topo::{analyze_topology, ChildTraverse, ChildIter, Node, print_loop_info};

type FullyUnroll = (usize, ValueRef, ValueRef, ValueRef, ValueRef, Vec<ValueRef>);

fn gather_small_loops(iter: ChildIter, res: &mut Vec<FullyUnroll>) -> bool {
  let mut inner_most = true;
  for elem in iter {
    match elem {
      Node::Loop(li) => {
        inner_most = false;
        // If this loop is inner most, and it is small enough, we can unroll it.
        if gather_small_loops(li.child_iter(), res) {
          if let Some(_) = li.get_loop_ind_var() {
            if let Some(n) = li.get_loop_n() {
              if let Some(const_scalar) = n.as_ref::<ConstScalar>(li.ctx()) {
                let blocks = li.child_iter().map(|bb| {
                  if let Node::Block(b) = bb {
                    return b;
                  }
                  unreachable!("This is inner most loop!");
                }).collect::<Vec<_>>();
                let count = blocks.iter().map(|b| b.get_num_insts()).sum::<usize>();
                if count * (const_scalar.get_value() as usize) < 1000 {
                  eprintln!("Small loop: {} * {}", count, const_scalar.get_value());
                  print_loop_info(li.child_iter(), 0);
                  let latch = li.get_latch();
                  let latch = latch.as_super();
                  let n = const_scalar.get_value();
                  let blocks = blocks.iter().rev().map(|x| x.as_super()).collect();
                  let prehead = li.get_prehead().as_super();
                  let head = li.get_head().as_super();
                  let exit = li.get_exit().as_super();
                  res.push((n as usize, prehead, head, latch, exit, blocks));
                }
              }
            }
          }
        }
      }
      Node::Block(_) => {}
    }
  }
  return inner_most;
}

fn loops_to_unroll(m: &Module) -> Vec<FullyUnroll> {
  let mut visited = vec![false; m.context.capacity()];
  let mut res = vec![];
  for f in m.func_iter() {
    let topo = analyze_topology(&f, &mut visited);
    gather_small_loops(topo.child_iter(), &mut res);
  }
  return res;
}

fn build_unrolled_insts(
  builder: &mut Builder,
  blocks: &Vec<ValueRef>,
  mut value_map: HashMap<usize, ValueRef>,
  phi_current: &mut HashMap<usize, ValueRef>,
  phi_carried: &HashMap<usize, usize>,
  last_block: &mut ValueRef,
  latch: &ValueRef,
  is_last: bool) {
  // Gather all the instructions to be unrolled.
  for block in blocks.iter() {
    let n = {
      let block_ref = block.as_ref::<Block>(&builder.module.context).unwrap();
      block_ref.get_num_insts()
    };
    builder.set_current_block(value_map.get(&block.skey).unwrap().clone());
    for i in 0..n {
      let block_ref = block.as_ref::<Block>(&builder.module.context).unwrap();
      let inst = block_ref.get_inst(i).unwrap();
      let inst_skey = inst.get_skey();
      if phi_current.contains_key(&inst_skey) {
        continue;
      }
      let op = inst.get_opcode().clone();
      let ty = inst.get_type().clone();
      let name = inst.get_name();
      let operands = if inst.get_skey() == latch.skey {
        // If this is a latch, we need to rewrite the branch to a unconditional branch jumps to
        // the next iteration.
        if !is_last {
          *last_block = value_map.get(&block.skey).unwrap().clone();
        }
        vec![value_map.get(&0).unwrap().clone()]
      } else {
        inst.operand_iter().map(|operand| {
          if let Some(mapped) = value_map.get(&operand.skey) {
            mapped.clone()
          } else {
            operand.clone()
          }
        }).collect::<Vec<_>>()
      };
      let built = builder.create_instruction(ty, op, operands, name);
      value_map.insert(inst_skey, built.clone());
      if let Some(carried) = phi_carried.get(&inst_skey) {
        phi_current.insert(*carried, built);
      }
    }
    let block_ref = builder.get_current_block().unwrap().as_ref::<Block>(&builder.module.context).unwrap();
    eprintln!("[Built Block]\n{}", block_ref.to_string(false));
  }
}

fn connect_block(builder: &mut Builder, last_block: &ValueRef, current: &ValueRef) {
  let restore = builder.get_current_block();
  let to_rewrite = {
    // Sanity check: if the last block is a unconditional branch.
    let last_block = last_block.as_ref::<Block>(&builder.module.context).unwrap();
    let br = last_block.last_inst().unwrap();
    br.as_sub::<BranchInst>().unwrap();
    // Get the unconditional branch.
    last_block.last_inst().unwrap().as_super()
  };
  let mut inst = InstMutator::new(&mut builder.module.context, &to_rewrite);
  inst.erase_from_parent();
  builder.set_current_block(last_block.clone());
  builder.create_unconditional_branch(current.clone());
  if let Some(restore) = restore {
    builder.set_current_block(restore);
  }
}

pub fn unroll_small_loops(m: Module) -> Module {
  let to_unroll = loops_to_unroll(&m);
  let mut builder = Builder::new(m);
  for (n, prehead, head, latch, exit, blocks) in to_unroll.iter() {
    eprintln!("{}", prehead.as_ref::<Block>(&builder.module.context).unwrap().to_string(false));
    for block in blocks.iter() {
      eprintln!("{}", block.as_ref::<Block>(&builder.module.context).unwrap().to_string(false));
    }
    eprintln!("{}", exit.as_ref::<Block>(&builder.module.context).unwrap().to_string(false));
    let mut last_block = prehead.clone();
    let (mut phi_current, phi_carried) = {
      let head = head.as_ref::<Block>(&builder.module.context).unwrap();
      let mut current = HashMap::new();
      let mut carried = HashMap::new();
      // If a value is a phi in the head block, it is a loop carried value.
      for inst in head.inst_iter() {
        if let Some(phi) = inst.as_sub::<PhiNode>() {
          assert!(phi.iter().count() == 2);
          for (block, value) in phi.iter() {
            if block.get_skey() == last_block.skey {
              eprintln!("{} -> {}", inst.get_name(), value.to_string(inst.ctx(), false));
              // If its branch is from the prehead, it is the initial value.
              current.insert(inst.get_skey(), value.clone());
            } else {
              // If its branch is from the latch, it is the value to be carried to the next
              // iteration. 
              carried.insert(value.skey, inst.get_skey());
            }
          }
        }
      }
      builder.set_current_function(head.get_parent().as_super());
      (current, carried)
    };
    let placeholder = builder.add_block("placeholder".to_string());
    phi_current.insert(0, placeholder.clone());
    for i in 0..(n - 1) {
      // Map original values to unrolled values, including but not limited to loop carried phis
      // and blocks.
      let mut value_map = phi_current.clone();
      for block in blocks.iter() {
        let block_ref = block.as_ref::<Block>(&builder.module.context).unwrap();
        let name = block_ref.get_name();
        let current_block = builder.add_block(format!("{}.iter.{}", name, i));
        value_map.insert(block.skey, current_block.clone());
        // Connect the last iteration to this iteration.
        if block == head {
          connect_block(&mut builder, &last_block, &current_block);
          eprintln!("[Connect]\n{}", last_block.as_ref::<Block>(&builder.module.context).unwrap().to_string(false));
        }
      }

      // Build the unrolled instructions.
      build_unrolled_insts(
        &mut builder, blocks, value_map.clone(),
        &mut phi_current, &phi_carried, 
        &mut last_block, latch, false);
    }
    // let value_map = phi_current.clone();
    connect_block(&mut builder, &last_block, head);
    {
      let latch_block = latch
        .as_ref::<Instruction>(&builder.module.context).unwrap()
        .get_parent().as_super();
      connect_block(&mut builder, &latch_block, exit);
    }
    // Gather all the instructions to be unrolled.
    for block in blocks.iter() {
      let n = {
        let block_ref = block.as_ref::<Block>(&builder.module.context).unwrap();
        block_ref.get_num_insts()
      };
      for i in 0..n {
        let block_ref = block.as_ref::<Block>(&builder.module.context).unwrap();
        let inst = block_ref.get_inst(i).unwrap();
        let inst_value = inst.as_super();
        if let Some(value) = phi_current.get(&inst_value.skey) {
          let mut phi = InstMutator::new(&mut builder.module.context, &inst_value);
          phi.replace_all_uses_with(value.clone());
        }
      }
    }
    // build_unrolled_insts(&mut builder, blocks, value_map, &mut last_block, latch, true);
    let mut remover = BlockMutator::new(builder.context(), placeholder);
    remover.erase_from_parent();
  }
  builder.module
}

