use std::collections::{HashSet, VecDeque, HashMap};

use trinity::{
  ir::{
    module::Module, Block,
    value::instruction::{Store, InstOpcode, Load, BranchInst, PhiNode, Alloca},
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
  frontiers: HashSet<(usize, usize)>,
  /// <Key: Alloca informatio, Value: <Key: Come-From Block, Value>>
  phi_info: HashMap<usize, HashMap<usize, ValueRef>>
}

impl WorkEntry {
  fn new() -> Self {
    Self {
      dominators: HashSet::new(),
      idom: 0,
      depth: 0,
      frontiers: HashSet::new(),
      phi_info: HashMap::new()
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
  // Calculate the dominate frontiers.
  // (immediate predeccessor, frontier block)
  for block in func.iter() {
    let block_ref = block.as_ref::<Block>(ctx).unwrap();
    if block_ref.get_num_predecessors() > 1 {
      for pred_idx in 0..block_ref.get_num_predecessors() {
        let pred_block = block_ref
          .get_predecessor(pred_idx)
          .unwrap()
          .as_ref::<Instruction>(ctx)
          .unwrap()
          .get_parent();
        eprintln!("Block {} has pred {}",
          block.to_string(ctx, false),
          pred_block.to_string(ctx, false));
        let mut runner = pred_block.skey;
        while runner != workspace[block.skey].idom {
          eprintln!(" Add {} to frontier of {}",
            block.to_string(ctx, false),
            runner);
          workspace[runner].frontiers.insert((pred_block.skey, block.skey));
          if workspace[runner].depth != 1 {
            runner = workspace[runner].idom;
          } else {
            break;
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
  for i in 0..func.get_num_blocks() {
    let block = func.get_block(i).unwrap();
    let entry = &workspace[block.skey];
    if entry.frontiers.is_empty() {
      continue;
    }
    eprintln!("  Block {} (Depth: {}) frontiers are:", block.to_string(ctx, false), entry.depth);
    for (pred, frontier) in entry.frontiers.iter() {
      let frontier= ValueRef{
        skey: *frontier,
        kind: trinity::ir::VKindCode::Block
      };
      let pred = ValueRef{
        skey: *pred,
        kind: trinity::ir::VKindCode::Block
      };
      eprintln!("    {} by pred {}", frontier.to_string(ctx, false), pred.to_string(ctx, false));
    }
  }
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
  phi_to_alloc: &HashMap<usize, usize>) -> ValueRef {
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
      let pos = block_ref.iter().position(|iter| { sub.get_skey() == iter.skey });
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
            return dom
          }
        },
        InstOpcode::Store(_) => {
          let store = Store::new(dom_inst);
          if store.get_ptr().skey == addr.skey {
            return store.get_value().clone()
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
  ValueRef { skey: 0, kind: VKindCode::Unknown }
}

fn inject_phis(module: Module, workspace: &mut Vec<WorkEntry>) -> Module {
  // Register values to be phi-resolved
  for func in module.iter() {
    for block in 0..func.get_num_blocks() {
      let block = func.get_block(block).unwrap();
      let block = block.as_ref::<Block>(&module.context).unwrap();
      for inst in block.iter() {
        let inst_ref = inst.as_ref::<Instruction>(&module.context).unwrap();
        match inst_ref.get_opcode() {
          InstOpcode::Store(_) => {
            let store = Store::new(inst_ref);
            if let Some(store_addr) = store.get_ptr().as_ref::<Instruction>(&module.context) {
              if let InstOpcode::Alloca(_) = store_addr.get_opcode() {
                for (pred, frontier) in workspace[block.get_skey()].frontiers.clone().iter() {
                  if !workspace[*frontier].phi_info.contains_key(&store.get_ptr().skey) {
                    workspace[*frontier].phi_info.insert(store.get_ptr().skey, HashMap::new());
                  }
                  let phi_info = workspace[*frontier].phi_info.get_mut(&store.get_ptr().skey).unwrap();
                  phi_info.insert(pred.clone(), store.get_value().clone());
                }
              }
            }
          },
          _ => {}
        }
      }
    }
  }
  // Inject preliminary PHI nodes.
  let mut builder = Builder::new(module);
  let mut phi_to_alloc = HashMap::new();
  for (block_skey, elem) in workspace.iter().enumerate() {
    let block = ValueRef { skey: block_skey, kind: VKindCode::Block };
    for (alloc_skey, values) in elem.phi_info.iter() {
      let alloc = ValueRef {skey: *alloc_skey, kind: VKindCode::Instruction};
      let comment = alloc.to_string(&builder.module.context, true);
      let mut phi_operands = Vec::new();
      values.iter().for_each(|(key, value)| {
        phi_operands.push(value.clone());
        phi_operands.push(Block::from(*key));
      });
      eprintln!("!!! {}", phi_operands[0].skey);
      let ty = phi_operands[0].get_type(&builder.module.context);
      builder.set_current_block(block.clone());
      let block = block.as_ref::<Block>(&builder.module.context).unwrap();
      let first_inst = block.get_inst(0).unwrap();
      builder.set_insert_before(first_inst);
      let res = builder.create_phi(ty, phi_operands.clone());
      phi_to_alloc.insert(res.skey, *alloc_skey);
      let phi = res.as_mut::<Instruction>(builder.context()).unwrap();
      phi.set_comment(comment);
    }
  }
  // Resolve missing incomings of PHI nodes.
  let mut to_append = Vec::new();
  let mut to_replace = Vec::new();
  for func in builder.module.iter() {
    for block in func.iter() {
      let block_ref = block.as_ref::<Block>(&builder.module.context).unwrap();
      let predeccessors = if block_ref.get_num_predecessors() > 1 {
        (0..block_ref.get_num_predecessors())
          .map(|x| {
            let br_inst = Instruction::from(block_ref.get_predecessor(x).unwrap().skey);
            let block = br_inst.as_ref::<Instruction>(&builder.module.context).unwrap().get_parent();
            block.skey
          })
          .collect::<HashSet<_>>()
      } else {
        HashSet::new()
      };
      for inst in block_ref.iter() {
        let inst_ref = inst.as_ref::<Instruction>(&builder.module.context).unwrap();
        match inst_ref.get_opcode() {
          InstOpcode::Phi => {
            assert!(!predeccessors.is_empty());
            let phi = PhiNode::new(inst_ref);
            let mut to_inspect = predeccessors.clone();
            for i in 0..phi.get_num_incomings() {
              to_inspect.remove(&phi.get_incoming_block(i).unwrap().skey);
            }
            for pred in to_inspect {
              let incoming_block = Block::from(pred);
              let incoming_value = find_value_dominator(&builder.module.context, inst_ref, &incoming_block, workspace, &phi_to_alloc);
              to_append.push((inst_ref.as_super(), incoming_value, incoming_block));
            }
          }
          InstOpcode::Load(_) => {
            let load = Load::new(inst_ref);
            if let Some(load_addr) = load.get_ptr().as_ref::<Instruction>(&builder.module.context) {
              if let InstOpcode::Alloca(_) = load_addr.get_opcode() {
                to_replace.push(inst_ref.as_super())
              }
            }
          }
          _ => {}
        }
      }
    }
  }
  for (phi, incoming_value, incoming_block) in to_append {
    let phi = phi.as_mut::<Instruction>(builder.context()).unwrap();
    phi.add_operand(incoming_value);
    phi.add_operand(incoming_block);
  }
  for inst in to_replace.iter() {
    let inst_ref = inst.as_ref::<Instruction>(&builder.module.context).unwrap();
    let block = inst_ref.get_parent();
    let new_value = find_value_dominator(&builder.module.context, inst_ref, &block, workspace, &phi_to_alloc);
    eprintln!("Replace: {} -> {}", inst.to_string(&builder.module.context, true), new_value.to_string(&builder.module.context, true));
    builder.module.replace_all_uses_with(inst.clone(), new_value);
  }
  for inst in to_replace.iter() {
    builder.module.remove_inst(inst.clone(), false);
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

