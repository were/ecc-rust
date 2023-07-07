use std::collections::{HashSet, VecDeque};

use trinity::{
  ir::{
    module::Module, Block,
    value::instruction::{Store, InstOpcode, Load, BranchInst},
    Instruction, ValueRef, Function, VKindCode
  },
  context::{Context, component::{GetSlabKey, AsSuper}},
  builder::Builder
};

#[derive(Clone)]
struct WorkEntry {
  dominators: HashSet<usize>,
  idom: usize,
  depth: usize,
}

impl WorkEntry {
  fn new() -> Self {
    Self {
      dominators: HashSet::new(),
      idom: 0,
      depth: 0,
    }
  }
}


fn analyze_dominators(ctx: &Context, func: &Function, workspace: &mut Vec<WorkEntry>) {
  // Calculate the dominators
  // TODO(@were): Engineer this to a more efficient implementation.
  let mut changed = true;
  while changed {
    changed = false;
    let block = func.get_block(0).unwrap();
    workspace[block.skey].dominators.insert(block.skey);
    workspace[block.skey].depth = 1;
    let mut visited = HashSet::new();
    visited.insert(block.skey);
    let mut q = VecDeque::new();
    q.push_back(block.clone());
    while let Some(front) = q.pop_front() {
      let block = front.as_ref::<Block>(ctx).unwrap();
      let last_idx = block.get_num_insts() - 1;
      let inst = block
        .get_inst(last_idx)
        .unwrap();
      let inst = inst
        .as_ref::<Instruction>(ctx)
        .unwrap();
      if *inst.get_opcode() == InstOpcode::Branch {
        let successors = BranchInst::new(inst).get_successors();
        for succ in successors {
          if visited.get(&succ.skey).is_none() {
            visited.insert(succ.skey);
            let (new_set, diff) = if workspace[succ.skey].dominators.is_empty() {
              let mut new_set = workspace[front.skey].dominators.clone();
              new_set.insert(succ.skey);
              (new_set, true)
            } else {
              let mut new_set = workspace[succ.skey].dominators
                .intersection(&workspace[front.skey].dominators)
                .cloned()
                .collect::<HashSet<_>>();
              new_set.insert(succ.skey);
              let diff = new_set != workspace[succ.skey].dominators;
              (new_set, diff)
            };
            if diff {
              changed = true;
              workspace[succ.skey].dominators = new_set;
              let (idom, deepest) = workspace[succ.skey]
                .dominators
                .iter()
                .fold((0, 0),
                |acc, elem| {
                if workspace[*elem].depth + 1 > acc.1 {
                  (*elem, workspace[*elem].depth + 1)
                } else {
                  acc
                }
              });
              // Find the deepest one as the immediate dominator
              if deepest > workspace[succ.skey].depth {
                workspace[succ.skey].depth = deepest;
                workspace[succ.skey].idom = idom;
              }
            }
            q.push_back(succ);
          }
        }
      }
    }
  }
  eprintln!("In function {}:", func.get_name());
  for i in 0..func.get_num_blocks() {
    let block = func.get_block(i).unwrap();
    let entry = &workspace[block.skey];
    eprintln!("  Block {} (Depth: {}) dominated by:", block.to_string(ctx, false), entry.depth);
    for dom in entry.dominators.iter() {
      let block_ref= ValueRef{
        skey: *dom,
        kind: trinity::ir::VKindCode::Block
      };
      eprint!("    {}", block_ref.to_string(ctx, false));
      if *dom == entry.idom {
        eprintln!(" *")
      } else {
        eprintln!("")
      }
    }
  }
}

// fn dominates(a: &Instruction, b: &Instruction, workspace: &Vec<WorkEntry>, context: &Context) -> bool {
//   let a_block = a.get_parent();
//   let b_block = b.get_parent();
//   if a_block.skey == b_block.skey {
//     let block = a_block.as_ref::<Block>(context).unwrap();
//     let mut idx_a = 0;
//     let mut idx_b = 0;
//     let a_skey = a.as_super().skey;
//     let b_skey = b.as_super().skey;
//     for i in 0..block.get_num_insts() {
//       let inst = block.get_inst(i).unwrap();
//       if inst.skey == a_skey {
//         idx_a = i;
//       }
//       if inst.skey == b_skey {
//         idx_b = i;
//       }
//     }
//     return idx_a < idx_b;
//   }
//   if workspace[b_block.skey].dominators.contains(&a_block.skey) {
//     true
//   } else {
//     false
//   }
// }

fn inject_phis(module: Module, workspace: &mut Vec<WorkEntry>) -> Module {
  let mut to_remove = HashSet::new();
  let mut to_replace = Vec::new();
  for func in module.iter() {
    for block in 0..func.get_num_blocks() {
      let block = func.get_block(block).unwrap();
      let block = block.as_ref::<Block>(&module.context).unwrap();
      for inst in block.iter() {
        let inst_ref = inst.as_ref::<Instruction>(&module.context).unwrap();
        match inst_ref.get_opcode() {
          // If it is a load.
          InstOpcode::Load(_) => {
            let load = Load::new(inst_ref);
            // And the pointer of this load is an alloca.
            if let Some(load_addr) = load.get_ptr().as_ref::<Instruction>(&module.context) {
              // If the load is from an allocated address.
              if let InstOpcode::Alloca(_) = load_addr.get_opcode() {
                let mut res = Vec::new();
                // Remove the load.
                to_remove.insert(inst_ref.get_skey());
                let mut q = VecDeque::new();
                let mut visited = HashSet::new();
                q.push_back((None, block.as_super()));
                eprintln!("Looking for value for Load: {}", load.to_string(&module.context));
                while let Some((frontier_info, front)) = q.pop_front() {
                  eprintln!("Front: {}", front.to_string(&module.context, true));
                  let front = front.as_ref::<Block>(&module.context).unwrap();
                  let mut found = false;
                  let iterate = if front.get_skey() == block.get_skey() {
                    let pos = (0..block.get_num_insts())
                      .into_iter()
                      .position(|x| block.get_inst(x).unwrap().skey == inst.skey);
                    if visited.contains(&front.get_skey()) {
                      // Self loop, so from last to this inst.
                      (pos.unwrap()..block.get_num_insts()).into_iter().rev()
                    } else {
                      // First time, so from first to this inst.
                      (0..pos.unwrap()).into_iter().rev()
                    }
                  } else {
                    (0..front.get_num_insts()).into_iter().rev()
                  };
                  for inst_idx in iterate {
                    let another = front.get_inst(inst_idx).unwrap();
                    let another_ref = another.as_ref::<Instruction>(&module.context).unwrap();
                    // Cannot self update.
                    if another_ref.get_parent().skey == inst_ref.get_parent().skey {
                      if another_ref.get_skey() == inst_ref.get_skey() {
                        continue;
                      }
                    }
                    match another_ref.get_opcode() {
                      // Store instruction.
                      InstOpcode::Store(_) => {
                        let store = Store::new(another_ref);
                        if let Some(store_addr) = store.get_ptr().as_ref::<Instruction>(&module.context) {
                          if let InstOpcode::Alloca(_) = store_addr.get_opcode() {
                            if load_addr.get_skey() == store_addr.get_skey() {
                              res.push(store.get_value().clone());
                              res.push(front.as_super());
                              to_remove.insert(another_ref.get_skey());
                              eprintln!("  found store: {}", store.to_string(&module.context));
                              found = true;
                              break;
                            }
                          }
                        }
                      }
                      InstOpcode::Load(_) => {
                        let upstream_load = Load::new(another_ref);
                        if let Some(upstream_addr) = upstream_load.get_ptr().as_ref::<Instruction>(&module.context) {
                          if let InstOpcode::Alloca(_) = upstream_addr.get_opcode() {
                            if load_addr.get_skey() == upstream_addr.get_skey() {
                              res.push(another_ref.as_super());
                              res.push(front.as_super());
                              eprintln!("  found load: {}", upstream_load.to_string(&module.context));
                              found = true;
                              break;
                            }
                          }
                        }
                      }
                      _ => {}
                    }
                  }
                  if !found {
                    if front.get_num_predecessors() > 1 {
                      for i in 0..front.get_num_predecessors() {
                        let to_push = front.
                          get_predecessor(i)
                          .as_ref::<Instruction>(&module.context)
                          .unwrap()
                          .get_parent();
                        if visited.get(&to_push.skey).is_none() {
                          visited.insert(to_push.skey);
                          let new_frontier_info = Some((front.as_super(), to_push.clone()));
                          q.push_back((new_frontier_info, to_push));
                        }
                      }
                    } else {
                      if workspace[front.get_skey()].depth != 1 {
                        let to_push = ValueRef{ skey: workspace[front.get_skey()].idom, kind: VKindCode::Block };
                        if visited.get(&to_push.skey).is_none() {
                          visited.insert(to_push.skey);
                          q.push_back((frontier_info, to_push));
                        }
                      }
                    }
                  }
                }
                to_replace.push((inst_ref.as_super(), res));
              }
            }
          }
          _ => {}
        }
      }
    }
  }
  // Inject Phi nodes.
  let mut builder = Builder::new(module);
  let mut iterative = true;
  while iterative {
    iterative = false;
    for (inst, new_value) in to_replace.iter_mut() {
      let replaced_comment = {
        let inst = inst.as_ref::<Instruction>(&builder.module.context).unwrap();
        inst.to_string(&builder.module.context)
      };
      let replacement = if new_value.len() <= 2 {
        let res = new_value.get(0).unwrap().clone();
        res
      } else {
        let ty = new_value[0].get_type(&builder.module.context);
        let block = inst.as_ref::<Instruction>(&builder.module.context).unwrap().get_parent();
        builder.set_current_block(block.clone());
        let block = block.as_ref::<Block>(&builder.module.context).unwrap();
        let first_inst = block.get_inst(0).unwrap();
        builder.set_insert_before(first_inst);
        let res = builder.create_phi(ty, new_value.clone());
        let phi = res.as_mut::<Instruction>(builder.context()).unwrap();
        phi.set_comment(format!("replace {}", replaced_comment));
        *new_value = vec![res.clone()];
        res
      };
      eprintln!("Replacing {} with {}", replaced_comment, replacement.to_string(&builder.module.context, false));
      iterative = iterative || builder.module.replace_all_uses_with(inst.clone(), replacement);
    }
  }
  for skey in to_remove {
    let inst = ValueRef{skey, kind: VKindCode::Instruction};
    eprintln!("Removing {}", inst.to_string(&builder.module.context, false));
    builder.module.remove_inst(inst, false);
  }
  builder.module
}

pub fn transform(module: Module) -> Module {
  eprintln!("{}", module.to_string());
  let mut workspace: Vec<WorkEntry> = Vec::new();
  workspace.resize(module.context.capacity(), WorkEntry::new());
  for func in module.iter() {
    if func.get_num_blocks() != 0 {
      analyze_dominators(&module.context, func, &mut workspace);
    }
  }
  inject_phis(module, &mut workspace)
}

