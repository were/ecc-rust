use std::collections::{HashSet, VecDeque, HashMap};

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

fn inject_phis(module: Module, workspace: &mut Vec<WorkEntry>) -> Module {
  let mut to_remove = HashSet::new();
  let mut to_replace = Vec::new();
  for func in module.iter() {
    for block in 0..func.get_num_blocks() {
      let block = func.get_block(block).unwrap();
      let block = block.as_ref::<Block>(&module.context).unwrap();
      let mut values = HashMap::new();
      for inst in block.iter() {
        let inst = inst.as_ref::<Instruction>(&module.context).unwrap();
        match inst.get_opcode() {
          InstOpcode::Alloca(_) => {
            // If it is allocate, create the entry.
            values.insert(inst.get_skey(), Vec::new());
          },
          // Store should be checked here.
          InstOpcode::Store(_) => {
            let store = Store::new(inst);
            if let Some(addr) = store.get_ptr().as_ref::<Instruction>(&module.context) {
              // If it is store, update the value of the entry for allocated addresses.
              if let InstOpcode::Alloca(_) = addr.get_opcode() {
                // We are not always in the same block of allocation.
                // If this entry is not in this block, create the entry.
                if values.get(&addr.get_skey()).is_none() {
                  values.insert(addr.get_skey(), Vec::new());
                }
                *values.get_mut(&addr.get_skey()).unwrap() = vec![store.get_value().clone()];
              }
            }
            // Remove the store.
            to_remove.insert(inst.get_skey());
          },
          // If it is a load.
          InstOpcode::Load(_) => {
            let load = Load::new(inst);
            // And the pointer of this load is an alloca.
            if let Some(load_addr) = load.get_ptr().as_ref::<Instruction>(&module.context) {
              // If the load is from an allocated address.
              if let InstOpcode::Alloca(_) = load_addr.get_opcode() {
                let new_value = if let Some(x) = values.get(&load_addr.get_skey()) {
                  x.clone()
                } else {
                  let mut res = Vec::new();
                  // Remove the load.
                  to_remove.insert(load_addr.get_skey());
                  // If it has frontiers, goes to its frontiers.
                  // Otherwise, goes to its immediate dominator.
                  for pred_idx in 0..block.get_num_predecessors() {
                    let pred_br = block.get_predecessor(pred_idx);
                    let pred_block = {
                      let pred_br = pred_br.as_ref::<Instruction>(&module.context).unwrap();
                      pred_br.get_parent()
                    };
                    // Find its latest value.
                    let mut found = false;
                    // Find the latest value of this alloca.
                    let mut runner = pred_block.skey;
                    loop {
                      let block = ValueRef{ skey: runner, kind: VKindCode::Block };
                      let block = block.as_ref::<Block>(&module.context).unwrap();
                      for inst_idx in (0..block.get_num_insts()).into_iter().rev() {
                        let inst = block.get_inst(inst_idx).unwrap();
                        let inst = inst.as_ref::<Instruction>(&module.context).unwrap();
                        match inst.get_opcode() {
                          // Store instruction.
                          InstOpcode::Store(_) => {
                            let store = Store::new(inst);
                            if let Some(store_addr) = store.get_ptr().as_ref::<Instruction>(&module.context) {
                              if let InstOpcode::Alloca(_) = store_addr.get_opcode() {
                                if load_addr.get_skey() == store_addr.get_skey() {
                                  res.push(store.get_value().clone());
                                  res.push(pred_block.clone());
                                  found = true;
                                  break;
                                }
                              }
                            }
                          }
                          InstOpcode::Load(_) => {
                            let load = Load::new(inst);
                            if let Some(upstream_addr) = load.get_ptr().as_ref::<Instruction>(&module.context) {
                              if let InstOpcode::Alloca(_) = upstream_addr.get_opcode() {
                                if load_addr.get_skey() == upstream_addr.get_skey() {
                                  res.push(inst.as_super());
                                  res.push(pred_block.clone());
                                  found = true;
                                  break;
                                }
                              }
                            }
                          }
                          _ => {}
                        }
                      }
                      if found {
                        break;
                      }
                      if workspace[runner].depth == 1 {
                        break;
                      }
                      runner = workspace[runner].idom;
                    }
                    // If not found, there exists a path that makes this value uninitialized.
                    if !found {
                      res.push(ValueRef { skey: 0, kind: VKindCode::Unknown });
                      res.push(pred_block);
                    }
                  }
                  res
                };
                to_replace.push((inst.get_skey(), new_value));
                to_remove.insert(inst.get_skey());
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
  let mut phi_replace = HashMap::new();
  for (inst, new_value) in to_replace.iter() {
    let inst = ValueRef{skey: *inst, kind: VKindCode::Instruction};
    {
      let inst = inst.as_ref::<Instruction>(&builder.module.context).unwrap();
      eprintln!("Relacing all usage of {}", inst.to_string(&builder.module.context))
    }
    let replacement = if new_value.len() == 1 || new_value.len() == 2 {
      let res = new_value.get(0).unwrap().clone();
      eprintln!("Replaced by {}", res.to_string(&builder.module.context, false));
      res
    } else {
      let ty = new_value[0].get_type(&builder.module.context);
      let block = inst.as_ref::<Instruction>(&builder.module.context).unwrap().get_parent();
      builder.set_current_block(block.clone());
      let block = block.as_ref::<Block>(&builder.module.context).unwrap();
      let first_inst = block.get_inst(0).unwrap();
      builder.set_insert_before(first_inst);
      let res = builder.create_phi(ty, new_value.clone());
      phi_replace.insert(inst.skey, res.clone());
      let inst = res.as_ref::<Instruction>(&builder.module.context).unwrap();
      eprintln!("Replaced by {}", inst.to_string(&builder.module.context));
      res
    };
    builder.module.replace_all_uses_with(inst, replacement);
  }
  for skey in to_remove {
    let inst = ValueRef{skey, kind: VKindCode::Instruction};
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

