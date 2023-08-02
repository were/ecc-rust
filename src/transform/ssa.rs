use std::collections::{HashSet, VecDeque, HashMap};

use trinity::{
  ir::{
    module::Module, Block,
    value::{instruction::{Store, InstOpcode, Load, InstructionRef, InstMutator}, function::FunctionRef},
    Instruction, ValueRef, VKindCode, PointerType
  },
  context::Context,
  builder::Builder
};

use super::{dce, simplify};

pub struct DomInfo {
  dominators: HashSet<usize>,
  idom: usize,
  depth: usize,
}

impl DomInfo {
  fn new() -> Self {
    Self {
      dominators: HashSet::new(),
      idom: 0,
      depth: 0,
    }
  }
}


fn analyze_dominators(ctx: &Context, func: &FunctionRef, workspace: &mut Vec<DomInfo>) {
  // Calculate the dominators
  let mut changed = true;
  while changed {
    changed = false;
    let block = func.get_block(0).unwrap();
    workspace[block.get_skey()].dominators.insert(block.get_skey());
    workspace[block.get_skey()].depth = 1;
    let mut visited = HashSet::new();
    visited.insert(block.get_skey());
    let mut q = VecDeque::new();
    q.push_back(block.as_super());
    while let Some(front) = q.pop_front() {
      let block = front.as_ref::<Block>(ctx).unwrap();
      let mut new_dom = HashSet::new();
      let mut first = true;
      for pred in block.pred_iter() {
        let pred = pred.get_parent();
        if first {
          new_dom = workspace[pred.get_skey()].dominators.clone();
          first = false;
        } else if !workspace[pred.get_skey()].dominators.is_empty() {
          new_dom = new_dom
            .intersection(&workspace[pred.get_skey()].dominators)
            .cloned()
            .collect::<HashSet<_>>();
        }
      }
      new_dom.insert(front.skey);
      if new_dom != workspace[front.skey].dominators {
        changed = true;
        workspace[front.skey].dominators = new_dom;
      }
      for succ in block.succ_iter() {
        if visited.contains(&succ.get_skey()) {
          continue;
        }
        q.push_back(succ.as_super());
        visited.insert(succ.get_skey());
      }
    }
  }
  {
    changed = true;
    while changed {
      changed = false;
      for block in func.block_iter() {
        for dom in workspace[block.get_skey()].dominators.clone().iter() {
          let dom = Block::from_skey(*dom);
          let dom = dom.as_ref::<Block>(ctx).unwrap();
          if block.get_skey() == dom.get_skey() {
            continue;
          }
          if workspace[block.get_skey()].depth < workspace[dom.get_skey()].depth + 1 {
            workspace[block.get_skey()].depth = workspace[dom.get_skey()].depth + 1;
            workspace[block.get_skey()].idom = dom.get_skey();
            changed = true;
          }
        }
      }
    }
  }
  eprintln!("In function {}:", func.get_name());
  for i in 0..func.get_num_blocks() {
    let block = func.get_block(i).unwrap();
    let entry = &workspace[block.get_skey()];
    eprintln!("  Block {} (Depth: {}), Pred: [{}] dominated by:", block.get_name(), entry.depth,
      block.pred_iter().map(|x| x.get_parent().get_name()).collect::<Vec<_>>().join(", "));
    for dom in entry.dominators.iter() {
      let block_ref= Block::from_skey(*dom);
      eprint!("    {}", block_ref.to_string(ctx, false));
      if *dom == entry.idom {
        eprintln!(" *")
      } else {
        eprintln!("")
      }
    }
  }
}

pub fn a_dominates_b(workspace: &Vec<DomInfo>, a: &InstructionRef, b: &InstructionRef) -> bool {
  let a_block = a.get_parent();
  let b_block = b.get_parent();
  if a_block.get_skey() == b_block.get_skey() {
    let block = a_block;
    let mut idx_a = 0;
    let mut idx_b = 0;
    let a_skey = a.get_skey();
    let b_skey = b.get_skey();
    for (i, inst) in block.inst_iter().enumerate() {
      if inst.get_skey() == a_skey {
        idx_a = i;
      }
      if inst.get_skey() == b_skey {
        idx_b = i;
      }
    }
    return idx_a <= idx_b;
  }
  if workspace[b_block.get_skey()].dominators.contains(&a_block.get_skey()) {
    true
  } else {
    false
  }
}

fn find_value_dominator(
  ctx: &Context,
  sub: &InstructionRef,
  block: &ValueRef,
  workspace: &Vec<DomInfo>,
  phi_to_alloc: &HashMap<usize, usize>,
  return_store: bool) -> Option<ValueRef> {
  let addr = {
    match sub.get_opcode() {
      InstOpcode::Load(_) => {
        let load = sub.as_sub::<Load>().unwrap();
        load.get_ptr().clone()
      },
      InstOpcode::Phi => {
        let skey = phi_to_alloc.get(&sub.get_skey()).unwrap();
        Instruction::from_skey(*skey)
      },
      _ => { panic!("Not an addressed instruction") }
    }
  };
  let mut runner = block.skey;
  let sub_parent = sub.get_parent();
  let mut first_iteration = true;
  loop {
    let block_value = Block::from_skey(runner);
    let block_ref = block_value.as_ref::<Block>(ctx).unwrap();
    let range = if runner == sub_parent.get_skey() {
      // If we are at the source block,
      let pos = block_ref.inst_iter().position(|iter| { sub.get_skey() == iter.get_skey() });
      // And the first iteration, inspect all the instructions before.
      if first_iteration {
        0..pos.unwrap()
      } else {
        // If not the first iteration, which indicates there is a backloop, inspect all the
        // instructions after.
        pos.unwrap()..block_ref.get_num_insts()
      }
    } else {
      // If we are at a dom block, inspect all the instructions.
      0..block_ref.get_num_insts()
    };
    for i in range.into_iter().rev() {
      let dom = block_ref.get_inst(i).unwrap();
      if dom.get_skey() == sub.get_skey() {
        continue;
      }
      match dom.get_opcode() {
        InstOpcode::Phi => {
          if *phi_to_alloc.get(&dom.get_skey()).unwrap() == addr.skey {
            return Some(dom.as_super())
          }
        },
        InstOpcode::Store(_) => {
          let store = dom.as_sub::<Store>().unwrap();
          if store.get_ptr().skey == addr.skey {
            return if return_store {
              Some(dom.as_super())
            } else {
              Some(store.get_value().clone())
            }
          }
        },
        _ => {}
      }
    }
    // End at root node.
    if workspace[runner].idom == 0 {
      break;
    }
    runner = workspace[runner].idom;
    first_iteration = false;
  }
  // This should not happen at all.
  None
}

fn inject_phis(module: Module, workspace: &mut Vec<DomInfo>) -> (Module, HashMap<usize, usize>) {
  let mut phis = HashMap::new();
  // Register values to be phi-resolved
  for func in module.func_iter() {
    for block in func.block_iter() {
      let predeccessors = block.pred_iter().collect::<Vec<_>>();
      if predeccessors.len() > 1 {
        // The current block is the frontier
        let frontier = block.get_skey();
        phis.insert(frontier, HashSet::new());
        for user_inst in predeccessors.iter() {
          let pred = user_inst;
          let mut runner = pred.get_parent().get_skey();
          while runner != workspace[block.get_skey()].idom {
            let runner_block = Block::from_skey(runner);
            let runner_block = runner_block.as_ref::<Block>(&module.context).unwrap();
            runner_block.inst_iter().rev().for_each(|inst| {
              if let Some(store) = inst.as_sub::<Store>() {
                if let Some(store_addr) = store.get_ptr().as_ref::<Instruction>(&module.context) {
                  if let InstOpcode::Alloca(_) = store_addr.get_opcode() {
                    phis.get_mut(&frontier).unwrap().insert(store_addr.get_skey());
                  }
                }
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
    let block = Block::from_skey(*block_skey);
    for alloc_skey in addrs.iter() {
      let alloc = Instruction::from_skey(*alloc_skey);
      let ptr_ty = alloc.get_type(&builder.module.context);
      let ptr_ty = ptr_ty.as_ref::<PointerType>(&builder.module.context).unwrap();
      let ty = ptr_ty.get_pointee_ty();
      let comment = alloc.to_string(&builder.module.context, true);
      builder.set_current_block(block.clone());
      let block = block.as_ref::<Block>(&builder.module.context).unwrap();
      let first_inst = block.get_inst(0).unwrap();
      builder.set_insert_before(first_inst.as_super());
      let phi = builder.create_phi(ty, vec![]);
      phi_to_alloc.insert(phi.skey, *alloc_skey);
      {
        let phi = phi.as_mut::<Instruction>(builder.context()).unwrap();
        phi.set_comment(format!("derive from {}", comment));
      }
      builder.create_store(phi, alloc).unwrap();
    }
  }
  // Resolve missing incomings of PHI nodes.
  let mut to_append = Vec::new();
  let mut to_replace = Vec::new();
  for func in builder.module.func_iter() {
    for block in func.block_iter() {
      let pred_branches = block.pred_iter().collect::<Vec<_>>();
      let predeccessors = if pred_branches.len() > 1 {
        let mut res = HashSet::new();
        pred_branches.iter().for_each(|inst| {
          // eprintln!("gather block: {}", inst.get_parent().to_string());
          res.insert(inst.get_parent().get_skey());
        });
        res
      } else {
        HashSet::new()
      };
      for inst in block.inst_iter() {
        match inst.get_opcode() {
          InstOpcode::Phi => {
            assert!(!predeccessors.is_empty());
            let incomings = predeccessors.iter().map(|pred| {
              let incoming_block = Block::from_skey(*pred);
              // eprintln!("incoming block: {}", incoming_block.to_string(&builder.module.context, true));
              // incoming_block.as_ref::<Block>(&builder.module.context).unwrap();
              if let Some(incoming_value) = find_value_dominator(
                &builder.module.context,
                &inst,
                &incoming_block,
                workspace,
                &phi_to_alloc,
                false) {
                eprintln!("[SSA] {}: [ {}, {} ]",
                  inst.get_name(),
                  incoming_value.to_string(&builder.module.context, true),
                  incoming_block.as_ref::<Block>(&builder.module.context).unwrap().get_name());
                (incoming_value, incoming_block)
              } else {
                // TODO(@were): Warning here!
                (ValueRef{ skey: 0, kind: VKindCode::Unknown }, incoming_block)
              }
            }).collect::<Vec<_>>();
            to_append.push((inst.as_super(), incomings));
          }
          InstOpcode::Load(_) => {
            let load = inst.as_sub::<Load>().unwrap();
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
  for (phi, incomings) in to_append {
    incomings.iter().for_each(|(incoming_value, incoming_block)| {
      let incoming_value = match incoming_value.kind {
        VKindCode::Unknown => {
          let ty = phi.get_type(&builder.module.context);
          builder.context().undef(ty)
        }
        _ => {
          incoming_value.clone()
        }
      };
      let mut phi = InstMutator::new(builder.context(), &phi);
      phi.add_operand(incoming_value);
      phi.add_operand(incoming_block.clone());
    });
  }
  for inst in to_replace.iter() {
    let inst_ref = (*inst).as_ref::<Instruction>(&builder.module.context).unwrap();
    let block = inst_ref.get_parent();
    let block = block.as_super();
    if let Some(new_value) = find_value_dominator(&builder.module.context, &inst_ref, &block, workspace, &phi_to_alloc, false) {
      let mut mutator = InstMutator::new(builder.context(), inst);
      mutator.replace_all_uses_with(new_value);
      // builder.module.replace_all_uses_with(inst.clone(), new_value);
      // eprintln!("SSA Replace: {} to {}",
      //  inst.to_string(&builder.module.context, false),
      //  new_value.to_string(&builder.module.context, false));
      //eprintln!("Replaced? {}", replaced);
    }
  }
  (builder.module, phi_to_alloc)
}

fn find_undominated_stores(
  module: &Module,
  workspace: &Vec<DomInfo>,
  phi_to_alloc: &HashMap<usize, usize>) -> HashSet<usize> {
  let mut store_with_dom = HashSet::new();
  for func in module.func_iter() {
    for block in func.block_iter() {
      for inst in block.inst_iter() {
        if let Some(load) = inst.as_sub::<Load>() {
          if let Some(load_addr) = load.get_ptr().as_ref::<Instruction>(&module.context) {
            if let InstOpcode::Alloca(_) = load_addr.get_opcode() {
              let block_ref = block.as_super();
              if let Some(value) = find_value_dominator(
                &module.context, &inst, &block_ref, workspace, &phi_to_alloc, true) {
                let inst = value.as_ref::<Instruction>(&module.context).unwrap();
                if let InstOpcode::Store(_) = inst.get_opcode() {
                  store_with_dom.insert(value.skey);
                }
              }
            }
          }
        }
      }
    }
  }
  store_with_dom
}

fn cleanup(module: &mut Module, workspace: &Vec<DomInfo>, phi_to_alloc: &HashMap<usize, usize>) {
  dce::transform(module);
  loop {
    let dominated = find_undominated_stores(&module, &workspace, &phi_to_alloc);
    let mut to_remove = Vec::new();
    for func in module.func_iter() {
      for block in func.block_iter() {
        for inst in block.inst_iter() {
          if let Some(store) = inst.as_sub::<Store>() {
            let ptr = store.get_ptr();
            if let Some(ptr_inst) = ptr.as_ref::<Instruction>(&module.context) {
              match ptr_inst.get_opcode() {
                InstOpcode::Alloca(_) => {
                  if !dominated.contains(&inst.get_skey()) {
                    let log = inst.to_string(false);
                    eprintln!("[SSA] Remove: {}, because stored value not loaded.", log);
                    to_remove.push(inst.as_super());
                  }
                },
                _ => { }
              }
            } // if store ptr is a instruction
          }
        } // for inst
      } // for block
    } // for func
    if to_remove.is_empty() {
      return;
    }
    for elem in to_remove {
      let mut mutator = InstMutator::new(&mut module.context, &elem);
      mutator.erase_from_parent();
    }
    simplify::remove_trivial_inst(module);
    dce::transform(module);
  }
}


pub fn transform(module: Module) -> (Module, Vec<DomInfo>) {
  // eprintln!("{}", module.to_string());
  let mut workspace: Vec<DomInfo> = Vec::new();
  (0..module.context.capacity()).for_each(|_| workspace.push(DomInfo::new()));
  for func in module.func_iter() {
    if func.get_num_blocks() != 0 {
      analyze_dominators(&module.context, &func, &mut workspace);
    }
  }
  let (mut injected, phi_to_alloc) = inject_phis(module, &mut workspace);
  cleanup(&mut injected, &workspace, &phi_to_alloc);
  (injected, workspace)
}

