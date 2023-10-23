use std::collections::HashMap;

use trinity::{
  ir::{
    value::{
      function::FunctionRef, block::BlockRef,
      instruction::{BranchInst, InstructionRef, InstOpcode, CmpPred}
    },
    Block, Instruction, ValueRef
  },
  context::Context
};

/// The implementation of a topological node.
/// The left is a block slab key, and the right is a loop recursive info.
enum NodeImpl {
  Block(usize),
  Loop(LoopImpl),
}

pub enum Node<'ctx> {
  Block(BlockRef<'ctx>),
  Loop(LoopInfo<'ctx>),
}

/// The information of a block in the topological format.
struct BlockInfo {
  /// The block id in the topological buffer.
  id: usize,
  /// The id of the parent loop of this block.
  parent: Option<usize>,
}

pub struct TopoInfo<'ctx> {
  /// The context of the IR module.
  ctx: &'ctx Context,
  /// The buffer of the topological nodes.
  buffer: Vec<NodeImpl>,
  /// The topological order of the root nodes.
  order: Vec<usize>,
  /// The mapping from block to node.
  bb2node: HashMap<usize, BlockInfo>,
}

pub struct LoopInfo<'ctx> {
  topo_info: &'ctx TopoInfo<'ctx>,
  loop_impl: &'ctx LoopImpl,
}

impl<'ctx> LoopInfo<'ctx> {

  fn new(topo_info: &'ctx TopoInfo<'ctx>, loop_impl: &'ctx LoopImpl) -> Self {
    Self {
      topo_info,
      loop_impl,
    }
  }

  pub fn get_latch(&self) -> InstructionRef<'ctx> {
    let ctx = self.topo_info.ctx;
    Instruction::from_skey(self.loop_impl.latch).as_ref::<Instruction>(ctx).unwrap()
  }

  pub fn get_head(&'ctx self) -> BlockRef<'ctx> {
    let ctx = self.topo_info.ctx;
    let head = Block::from_skey(self.loop_impl.head).as_ref::<Block>(ctx).unwrap();
    return head;
  }

  pub fn get_prehead(&'ctx self, ctx: &'ctx Context) -> BlockRef<'ctx> {
    let head = self.get_head();
    let latch = self.get_latch();
    assert!(head.pred_iter().count() == 2);
    for pred in head.pred_iter() {
      if pred.get_skey() == latch.get_skey() {
        continue;
      }
      assert!(pred.get_parent().get_name().starts_with("for.predhead."));
      let res = Block::from_skey(pred.get_parent().get_skey()).as_ref::<Block>(ctx).unwrap();
      return res;
    }
    unreachable!("Should not get here as each loop should always have a non-latch pred");
  }

  pub fn get_exit(&'ctx self) -> BlockRef<'ctx> {
    let ctx = self.topo_info.ctx;
    let exit = Block::from_skey(self.loop_impl.exit).as_ref::<Block>(ctx).unwrap();
    return exit;
  }

  // TODO(@were): Get the loop trip count.
  pub fn get_loop_n(&self) -> Option<ValueRef> {
    let ctx = self.topo_info.ctx;
    let latch = self.get_latch();
    if let Some(br) = latch.as_sub::<BranchInst>() {
      if let Some(cond) = br.cond() {
        if let Some(inst) = cond.as_ref::<Instruction>(ctx) {
          if let InstOpcode::ICompare(CmpPred::SLT) = inst.get_opcode() {
            return inst.get_operand(1).map(|x| x.clone());
          }
        }
      }
    }
    None
  }

  // TODO(@were): Check the incremental value to be one.
  /// Get the phi node of a canonical inductive loop.
  pub fn get_loop_ind_var(&'ctx self, ctx: &'ctx Context ) -> Option<InstructionRef<'ctx>> {
    let latch = self.get_latch();
    if let Some(br) = latch.as_sub::<BranchInst>() {
      if let Some(cond) = br.cond() {
        if let Some(inst) = cond.as_ref::<Instruction>(ctx) {
          if let InstOpcode::ICompare(CmpPred::SLT) = inst.get_opcode() {
            return inst.get_operand(0).unwrap().as_ref::<Instruction>(ctx)
          }
        }
      }
    }
    None
  }


}

impl <'ctx>TopoInfo<'ctx> {

  fn new(ctx: &'ctx Context) -> Self {
    Self {
      ctx,
      buffer: Vec::new(),
      order: Vec::new(),
      bb2node: HashMap::new(),
    }
  }

  /// Add a block as a node in this topology info record.
  fn add_block(&mut self, bb_skey: usize) -> Option<usize> {
    if let None = self.bb2node.insert(bb_skey, BlockInfo { id: self.buffer.len(), parent: None }) {
      let res = Some(self.buffer.len());
      self.buffer.push(NodeImpl::Block(bb_skey));
      res
    } else {
      unreachable!("A basic block cannot be added twice!");
    }
  }

  /// Add a loop as a node in this topology info record.
  fn add_loop(&mut self, latch: usize, head: usize, exit: usize) -> usize {
    let res = self.buffer.len();
    self.buffer.push(NodeImpl::Loop(LoopImpl::new(latch, head, exit)));
    res
  }

  /// Get the loop of the given ID.
  fn get_loop(&'ctx self, id: usize) -> LoopInfo<'ctx> {
    match &self.buffer[id] {
      NodeImpl::Block(_) => unreachable!("The id {} is not a loop!", id),
      NodeImpl::Loop(li) => { LoopInfo::new(self, li) },
    }
  }

  /// Get the loop to which the given block belongs.
  pub fn get_loop_of_block(&'ctx self, bb_skey: usize) -> Option<LoopInfo> {
    self.bb2node.get(&bb_skey).and_then(|x| {
      if let Some(parent) = x.parent {
        Some(self.get_loop(parent))
      } else {
        None
      }
    })
  }

  /// Add block as a loop body.
  fn finalize_block(&mut self, parent: Option<usize>, bb_skey: usize) {
    if let Some(block_node) = self.bb2node.get_mut(&bb_skey) {
      block_node.parent = parent;
      if let Some(loop_id) = parent {
        if let NodeImpl::Loop(li) = &mut self.buffer[loop_id] {
          li.children.push(block_node.id);
        }
      } else {
        self.order.push(block_node.id);
      }
    } else {
      unreachable!("The block {} is not added to the topo buffer!", bb_skey);
    }
  }

  fn fianlize_loop(&mut self, parent: Option<usize>, loop_id: usize) {
    if let Some(parent_node) = parent {
      if let NodeImpl::Loop(li) = &mut self.buffer[parent_node] {
        li.children.push(loop_id);
      }
    } else {
      self.order.push(loop_id);
    }
  }

}

pub struct LoopImpl {
  /// The latch branch of this loop.
  latch: usize,
  /// The loop entrance block.
  head: usize,
  /// The loop exit block.
  exit: usize,
  /// The child elements of this loop.
  children: Vec<usize>,
}

pub struct ChildIter<'ctx> {
  topo: &'ctx TopoInfo<'ctx>,
  vec: &'ctx Vec<usize>,
  i: usize,
}

impl <'ctx> ChildIter<'ctx> {
  fn new(topo: &'ctx TopoInfo, vec: &'ctx Vec<usize>) -> Self {
    Self {
      topo,
      vec,
      i: 0,
    }
  }
}

impl<'ctx> Iterator for ChildIter<'ctx> {
  type Item = Node<'ctx>;
  fn next(&mut self) -> Option<Self::Item> {
    if self.i < self.vec.len() {
      let idx = self.vec[self.i];
      self.i += 1;
      match &self.topo.buffer[idx] {
        NodeImpl::Block(block) => {
          let res = Block::from_skey(*block).as_ref::<Block>(self.topo.ctx).unwrap();
          Some(Node::Block(res))
        }
        NodeImpl::Loop(li) => {
          let res = LoopInfo::new(self.topo, li);
          Some(Node::Loop(res))
        }
      }
    } else {
      None
    }
  }
}

pub trait ChildTraverse<'ctx> {
  fn child_iter(&'ctx self) -> ChildIter;
  fn ctx(&'ctx self) -> &'ctx Context;
}

impl <'ctx>ChildTraverse<'ctx> for TopoInfo<'ctx> {

  fn child_iter(&'ctx self) -> ChildIter {
    ChildIter::new(self, &self.order)
  }

  fn ctx(&self) -> &'ctx Context {
    self.ctx
  }

}

impl <'ctx>ChildTraverse<'ctx> for LoopInfo<'ctx> {

  fn child_iter(&'ctx self) -> ChildIter {
    ChildIter::new(self.topo_info, &self.loop_impl.children)
  }

  fn ctx(&self) -> &'ctx Context {
    self.topo_info.ctx
  }

}


pub fn print_loop_info(iter: ChildIter, indent: usize) {
  let indent = indent + 1;
  for elem in iter {
    match elem {
      Node::Block(block) => {
        println!("{}Block: {}", " ".repeat(indent), block.get_name());
      }
      Node::Loop(loop_info) => {
        println!("{}Loop <head:{}><exit:{}><latch:{}>",
          " ".repeat(indent),
          loop_info.get_head().get_name(),
          loop_info.get_exit().get_name(),
          loop_info.get_latch().to_string(false));
        print_loop_info(loop_info.child_iter(), indent);
      }
    }
  }
}

impl <'ctx>LoopImpl {

  fn new(latch: usize, head: usize, exit: usize) -> Self {
    Self {
      latch,
      head,
      exit,
      children: Vec::new(),
    }
  }

}

fn dfs_topology<'ctx>(
  ctx: &'ctx Context,
  cur: &'_ BlockRef,
  visited: &mut Vec<bool>,
  loop_stack: &mut Vec<usize>,
  finalized_loops: &mut Vec<usize>,
  res: &mut TopoInfo) {

  let block_node = res.add_block(cur.get_skey()).unwrap();

  let is_head = if let Some(latch) = cur.is_loop_head() {
    // eprintln!("Loop latch: {}", latch.to_string(false));
    let branch = latch.as_sub::<BranchInst>().unwrap();
    let exit = branch.succ_iter().find(|x| x.get_skey() != cur.get_skey()).unwrap();
    let loop_id = res.add_loop(latch.get_skey(), cur.get_skey(), exit.get_skey());
    loop_stack.push(loop_id);
    true
  } else {
    false
  };

  let successors = cur.succ_iter().collect::<Vec<_>>();
  for succ in successors.iter() {
    // eprintln!("Visiting {}'s {}-th child, {}", block.get_name(), idx, succ.get_name());
    let dst_key = succ.get_skey();
    // We cannot go out this loop until we traverse all the blocks.
    let not_an_exit = loop_stack.iter().all(|x| {
      res.get_loop(*x).get_exit().get_skey() != dst_key
    });
    if not_an_exit {
      if !visited[dst_key] {
        // eprintln!("First visit, push {} to stack!", succ.get_name());
        visited[dst_key] = true;
        dfs_topology(ctx, &succ, visited, loop_stack, finalized_loops, res);
      }
    }
  }

  res.finalize_block(loop_stack.last().map(|x| *x), cur.get_skey());

  // This part is the trickiest. The whole algorithm makes the assumption that
  // all the loops are canonical. They have both only one conditional entrance
  // only one unified exit.
  //
  // Once a loop's topology inside is fully traversed, we can finalize it.
  // After finalization, we can go to the exit block of this loop to analyze
  // the rest topology. Finalized loops are put in finalized loops, they cannot
  // be popped to the topological order until all its exit directioned blocks
  // are analyzed.
  if is_head {
    let to_finalize = loop_stack.pop().unwrap();
    // eprintln!("Push exit block: {}", to_finalize.get_exit().get_name());
    // print_loop_info(&to_finalize, 0);
    // TODO(@were): The lifetime management is not elegant enough here.
    //              I can only reconstruct the exit block.
    let exit_block = {
      let to_finalize = res.get_loop(to_finalize);
      let exit_block = to_finalize.get_exit().as_super();
      exit_block.as_ref::<Block>(ctx).unwrap()
    };
    finalized_loops.push(to_finalize);

    if !visited[exit_block.get_skey()] {
      visited[exit_block.get_skey()] = true;
      dfs_topology(ctx, &exit_block, visited, loop_stack, finalized_loops, res);
    }

    // eprintln!("Finalized loop: {}", exit_block.get_name());
    let finalized = finalized_loops.pop().unwrap();
    // print_loop_info(&finalized, 0);

    res.fianlize_loop(loop_stack.last().map(|x| *x), finalized);
  }

}

pub fn analyze_topology<'ctx>(func: &'ctx FunctionRef, visited: &mut Vec<bool>) -> TopoInfo<'ctx> {
  let mut loop_stack = Vec::new();
  let mut finalized_loops = Vec::new();
  let mut res = TopoInfo::new(func.ctx);

  let entry = func.get_block(0).unwrap();
  visited[entry.get_skey()] = true;
  dfs_topology(func.ctx, &entry, visited, &mut loop_stack, &mut finalized_loops, &mut res);

  // eprintln!("[TOPO] Analyzed topology of func @{}", func.get_name());
  // for elem in res.iter() {
  //   match elem {
  //     Either::Left(block) => {
  //       let block = Block::from_skey(*block).as_ref::<Block>(func.ctx).unwrap();
  //       eprintln!(" Block: {}", block.get_name());
  //     }
  //     Either::Right(li) => {
  //       print_loop_info(li, 1)
  //     }
  //   }
  // }

  assert!(finalized_loops.is_empty(), "There are still some loops not finalized!");
  return res;
}

