use std::collections::{HashSet, VecDeque, HashMap};

use trinity::{
  ir::{
    module::Module, Block,
    value::instruction::{Store, InstOpcode, Load, BranchInst, PhiNode, },
    Instruction, ValueRef, Function, VKindCode, PointerType
  },
  context::{Context, component::{GetSlabKey, AsSuper}},
  builder::Builder
};

use super::dce;

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
  // eprintln!("In function {}:", func.get_name());
  // for i in 0..func.get_num_blocks() {
  //   let block = func.get_block(i).unwrap();
  //   let entry = &workspace[block.skey];
  //   eprintln!("  Block {} (Depth: {}) dominated by:", block.to_string(ctx, false), entry.depth);
  //   for dom in entry.dominators.iter() {
  //     let block_ref= ValueRef{
  //       skey: *dom,
  //       kind: trinity::ir::VKindCode::Block
  //     };
  //     eprint!("    {}", block_ref.to_string(ctx, false));
  //     if *dom == entry.idom {
  //       eprintln!(" *")
  //     } else {
  //       eprintln!("")
  //     }
  //   }
  // }
}

// fn dominates_ii(a: &Instruction, b: &Instruction, workspace: &Vec<WorkEntry>, context: &Context) -> bool {
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

fn find_value_dominator(
  ctx: &Context,
  sub: &Instruction,
  block: &ValueRef,
  workspace: &Vec<WorkEntry>,
  phi_to_alloc: &HashMap<usize, usize>,
  return_store: bool) -> Option<ValueRef> {
  let addr = {
    match sub.get_opcode() {
      InstOpcode::Load(_) => {
        let load = Load::new(sub);
        load.get_ptr().clone()
      },
      InstOpcode::Phi => {
        let skey = phi_to_alloc.get(&sub.get_skey()).unwrap();
        Instruction::from(*skey)
      },
      _ => { panic!("Not an addressed instruction") }
    }
  };
  let mut runner = block.skey;
  let sub_parent = sub.get_parent();
  loop {
    let block_value = Block::from(runner);
    let block_ref = block_value.as_ref::<Block>(ctx).unwrap();
    let n = if runner == sub_parent.skey {
      // If we are at the source block, inspect all the instructions before.
      let pos = block_ref.inst_iter(ctx).position(|iter| { sub.get_skey() == iter.get_skey() });
      pos.unwrap()
    } else {
      // If we are at a dom block, inspect all the instructions.
      block_ref.get_num_insts()
    };
    for i in (0..n).into_iter().rev() {
      let dom = block_ref.get_inst(i).unwrap();
      if dom.skey == sub.get_skey() {
        continue;
      }
      let dom_inst = dom.as_ref::<Instruction>(ctx).unwrap();
      match dom_inst.get_opcode() {
        InstOpcode::Phi => {
          if *phi_to_alloc.get(&dom_inst.get_skey()).unwrap() == addr.skey {
            return Some(dom)
          }
        },
        InstOpcode::Store(_) => {
          let store = Store::new(dom_inst);
          if store.get_ptr().skey == addr.skey {
            return if return_store { Some(dom) } else { Some(store.get_value().clone()) }
          }
        },
        _ => {}
      }
    }
    // End at root node.
    if workspace[runner].depth == 1 {
      break;
    }
    runner = workspace[runner].idom;
  }
  // This should not happen at all.
  None
}

fn inject_phis(module: Module, workspace: &mut Vec<WorkEntry>) -> (Module, HashMap<usize, usize>) {
  let mut phis = HashMap::new();
  // Register values to be phi-resolved
  for func in module.iter() {
    for block in func.iter(&module.context) {
      if block.get_num_predecessors() > 1 {
        // The current block is the frontier
        let frontier = block.get_skey();
        phis.insert(frontier, HashSet::new());
        eprintln!("frontier: {}", block.as_super().to_string(&module.context, false));
        for pred in block.pred_iter(&module.context) {
          let mut runner = pred.get_parent().skey;
          while runner != workspace[block.get_skey()].idom {
            let runner_block = Block::from(runner);
            let runner_block = runner_block.as_ref::<Block>(&module.context).unwrap();
            runner_block.inst_iter(&module.context).rev().for_each(|inst| {
              match inst.get_opcode() {
                InstOpcode::Store(_) => {
                  let store = Store::new(inst);
                  if let Some(store_addr) = store.get_ptr().as_ref::<Instruction>(&module.context) {
                    if let InstOpcode::Alloca(_) = store_addr.get_opcode() {
                      phis.get_mut(&frontier).unwrap().insert(store_addr.get_skey());
                    }
                  }
                },
                _ => {}
              }
            });
            if workspace[runner].depth == 1 {
              break;
            }
            runner = workspace[runner].idom;
          }
        }
      }
    }
  }
  let mut phi_to_alloc = HashMap::new();
  // Inject preliminary PHI nodes.
  let mut builder = Builder::new(module);
  for (block_skey, addrs) in phis.iter() {
    let block = Block::from(*block_skey);
    for alloc_skey in addrs.iter() {
      let alloc = Instruction::from(*alloc_skey);
      let ptr_ty = alloc.get_type(&builder.module.context);
      let ty = ptr_ty.as_ref::<PointerType>(&builder.module.context).unwrap().get_pointee_ty();
      let comment = alloc.to_string(&builder.module.context, true);
      builder.set_current_block(block.clone());
      let block = block.as_ref::<Block>(&builder.module.context).unwrap();
      let first_inst = block.get_inst(0).unwrap();
      builder.set_insert_before(first_inst);
      let phi = builder.create_phi(ty, vec![]);
      phi_to_alloc.insert(phi.skey, *alloc_skey);
      {
        let phi = phi.as_mut::<Instruction>(builder.context()).unwrap();
        phi.set_comment(comment);
      }
      builder.create_store(phi, alloc).unwrap();
    }
  }
  // Resolve missing incomings of PHI nodes.
  let mut to_append = Vec::new();
  let mut to_replace = Vec::new();
  for func in builder.module.iter() {
    for block in func.iter(&builder.module.context) {
      let predeccessors = if block.get_num_predecessors() > 1 {
        block.pred_iter(&builder.module.context)
          .map(|inst| {
            let block = inst.get_parent();
            block.skey
          })
          .collect::<HashSet<_>>()
      } else {
        HashSet::new()
      };
      for inst in block.inst_iter(&builder.module.context) {
        match inst.get_opcode() {
          InstOpcode::Phi => {
            assert!(!predeccessors.is_empty());
            for pred in predeccessors.iter() {
              let incoming_block = Block::from(*pred);
              if let Some(incoming_value) = find_value_dominator(
                &builder.module.context,
                inst,
                &incoming_block,
                workspace,
                &phi_to_alloc,
                false) {
                to_append.push((inst.as_super(), incoming_value, incoming_block));
              } else {
                // TODO(@were): Warning here!
                to_append.push((inst.as_super(), ValueRef{ skey: 0, kind: VKindCode::Unknown }, incoming_block));
              }
            }
          }
          InstOpcode::Load(_) => {
            let load = Load::new(inst);
            if let Some(load_addr) = load.get_ptr().as_ref::<Instruction>(&builder.module.context) {
              if let InstOpcode::Alloca(_) = load_addr.get_opcode() {
                to_replace.push(inst.as_super())
              }
            }
          }
          _ => {}
        }
      }
    }
  }
  for (phi, incoming_value, incoming_block) in to_append {
    let incoming_value = if incoming_value.kind == VKindCode::Unknown {
      let ty = phi.get_type(&builder.module.context);
      builder.context().undef(ty)
    } else {
      incoming_value
    };
    let phi = phi.as_mut::<Instruction>(builder.context()).unwrap();
    phi.add_operand(incoming_value);
    phi.add_operand(incoming_block);
  }
  for inst in to_replace.iter() {
    let inst_ref = (*inst).as_ref::<Instruction>(&builder.module.context).unwrap();
    let block = inst_ref.get_parent();
    if let Some(new_value) = find_value_dominator(&builder.module.context, inst_ref, &block, workspace, &phi_to_alloc, false) {
      builder.module.replace_all_uses_with(inst.clone(), new_value);
    }
  }
  (builder.module, phi_to_alloc)
}

fn find_undominated_stores(
  module: &Module,
  workspace: &Vec<WorkEntry>,
  phi_to_alloc: &HashMap<usize, usize>) -> HashSet<usize> {
  let mut store_with_dom = HashSet::new();
  for func in module.iter() {
    for block in func.iter(&module.context) {
      for inst in block.inst_iter(&module.context) {
        match inst.get_opcode() {
          InstOpcode::Load(_) => {
            let load = Load::new(inst);
            if let Some(load_addr) = load.get_ptr().as_ref::<Instruction>(&module.context) {
              if let InstOpcode::Alloca(_) = load_addr.get_opcode() {
                if let Some(value) = find_value_dominator(
                  &module.context, inst, &block.as_super(), workspace, &phi_to_alloc, true) {
                  let inst = value.as_ref::<Instruction>(&module.context).unwrap();
                  if let InstOpcode::Store(_) = inst.get_opcode() {
                    store_with_dom.insert(value.skey);
                  }
                }
              }
            }
          }
          _ => {}
        }
      }
    }
  }
  store_with_dom
}

fn cleanup(module: &mut Module, workspace: &Vec<WorkEntry>, phi_to_alloc: &HashMap<usize, usize>) {
  dce::transform(module);
  loop {
    let dominated = find_undominated_stores(&module, &workspace, &phi_to_alloc);
    let mut to_remove = Vec::new();
    for func in module.iter() {
      for block in func.iter(&module.context) {
        for inst in block.inst_iter(&module.context) {
          match inst.get_opcode() {
            InstOpcode::Store(_) => {
              let store = Store::new(inst);
              let ptr = store.get_ptr();
              if let Some(ptr_inst) =  ptr.as_ref::<Instruction>(&module.context) {
                match ptr_inst.get_opcode() {
                  InstOpcode::Alloca(_) => {
                    if !dominated.contains(&inst.get_skey()) {
                      to_remove.push(inst.as_super());
                    }
                  },
                  _ => { }
                }
              } // if store ptr is a instruction
            }, // store
            _ => {}
          } // match inst opcode
        } // for inst
      } // for block
    } // for func
    if to_remove.is_empty() {
      return;
    }
    for elem in to_remove {
      module.remove_inst(elem, true);
    }
    dce::transform(module);
  }
}


pub fn transform(module: Module) -> Module {
  // eprintln!("{}", module.to_string());
  let mut workspace: Vec<WorkEntry> = Vec::new();
  workspace.resize(module.context.capacity(), WorkEntry::new());
  for func in module.iter() {
    if func.get_num_blocks() != 0 {
      analyze_dominators(&module.context, func, &mut workspace);
    }
  }
  let (mut injected, phi_to_alloc) = inject_phis(module, &mut workspace);
  cleanup(&mut injected, &workspace, &phi_to_alloc);
  injected
}

