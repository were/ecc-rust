use either::Either;

use trinity::{
  ir::{
    value::{
      function::FunctionRef, block::BlockRef, instruction::{BranchInst, InstructionRef}
    },
    Block, Instruction
  },
  context::Context
};

pub struct LoopInfo<'ctx> {
  ctx: &'ctx Context,
  /// The latch branch of this loop.
  latch: usize,
  /// The loop entrance block.
  head: usize,
  /// The loop exit block.
  exit: usize,
  /// The child elements of this loop.
  children: Vec<Either<usize, Box<LoopInfo<'ctx>>>>,
}

pub fn print_loop_info<'ctx>(li: &LoopInfo<'ctx>, indent: usize) {
  println!("{}Loop <head:{}><exit:{}><latch:{}>",
    " ".repeat(indent),
    li.get_head().get_name(), li.get_exit().get_name(), li.get_latch().to_string(false));
  let indent = indent + 1;
  for elem in li.children.iter() {
    match elem {
      Either::Left(block) => {
        let block = Block::from_skey(*block).as_ref::<Block>(li.ctx).unwrap();
        println!("{}Block: {}", " ".repeat(indent), block.get_name());
      }
      Either::Right(loop_info) => {
        print_loop_info(loop_info, indent);
      }
    }
  }
}

impl <'ctx>LoopInfo<'ctx> {

  fn new(ctx: &'ctx Context, latch: usize, head: usize) -> Self {
    let branch = Instruction::from_skey(latch).as_ref::<Instruction>(ctx).unwrap();
    let branch = branch.as_sub::<BranchInst>().unwrap();
    let exit = branch.succ_iter().find(|x| x.get_skey() != head).unwrap();
    Self {
      ctx,
      latch,
      head,
      exit: exit.get_skey(),
      children: Vec::new(),
    }
  }

  fn get_latch(&self) -> InstructionRef<'ctx> {
    Instruction::from_skey(self.latch).as_ref::<Instruction>(self.ctx).unwrap()
  }

  fn get_head(&'ctx self) -> BlockRef<'ctx> {
    let head = Block::from_skey(self.head).as_ref::<Block>(self.ctx).unwrap();
    return head;
  }

  fn get_exit(&'ctx self) -> BlockRef<'ctx> {
    let exit = Block::from_skey(self.exit).as_ref::<Block>(self.ctx).unwrap();
    return exit;
  }

}

pub fn analyze_topology<'ctx>(func: &'ctx FunctionRef, visited: &mut Vec<bool>) -> Vec<Either<usize, LoopInfo<'ctx>>> {
  let mut loop_stack = Vec::new();
  let mut stack = Vec::new();
  let mut res = Vec::new();

  let entry = func.get_block(0).unwrap();
  visited[entry.get_skey()] = true;
  stack.push((entry, 0 as usize));

  // This is a system-stack-free DFS.
  while let Some((block, idx)) = stack.last() {
    if *idx == 0 {
      if let Some(latch) = block.is_loop_head() {
        eprintln!("Loop latch: {}", latch.to_string(false));
        // TODO(@were): This lifetime management is not elegant enough.
        //              I can only pass the key of this variable.
        loop_stack.push(LoopInfo::new(func.ctx, latch.get_skey(), block.get_skey()));
        print_loop_info(loop_stack.last().unwrap(), 0);
      }
    }
    if let Some(succ) = block.get_succ(*idx) {
      eprintln!("Visiting {}'s {}-th child, {}", block.get_name(), idx, succ.get_name());
      let dst_key = succ.get_skey();
      // We cannot go out this loop until we traverse all the blocks.
      let not_an_exit = loop_stack.iter().all(|x| {
        x.get_exit().get_skey() != dst_key
      });
      if not_an_exit {
        if !visited[dst_key] {
          eprintln!("First visit, push {} to stack!", succ.get_name());
          stack.push((succ, 0 as usize));
          visited[dst_key] = true;
          continue;
        }
      }
    } else {
      if let Some(cur_loop) = loop_stack.last_mut() {
        cur_loop.children.push(Either::Left(block.get_skey()));
      } else {
        res.push(Either::Left(block.get_skey()));
      }
      let push_exit = if block.is_loop_head().is_some() {
        eprintln!("Finalized loop: {}, {}", block.get_name(), *idx);
        let finalized_loop = loop_stack.pop().unwrap();
        print_loop_info(&finalized_loop, 0);
        let to_push = {
          eprintln!("Pushing exit block: {}", finalized_loop.get_exit().get_name());
          // TODO(@were): The lifetime management is not elegant enough here.
          //              I can only reconstruct the exit block.
          let block = finalized_loop.get_exit().as_super();
          let block = block.as_ref::<Block>(func.ctx).unwrap();
          block
        };
        if let Some(parent) = loop_stack.last_mut() {
          parent.children.push(Either::Right(Box::new(finalized_loop)));
        } else {
          res.push(Either::Right(finalized_loop));
        }
        Some(to_push)
      } else {
        None
      };
      stack.pop();
      if let Some(block) = push_exit {
        visited[block.get_skey()] = true;
        stack.push((block, 0 as usize));
      }
    }
    if let Some((_, idx)) = stack.last_mut() {
      *idx += 1;
    }
  }

  println!("Function {}:", func.get_name());
  for elem in res.iter() {
    match elem {
      Either::Left(block) => {
        let block = Block::from_skey(*block).as_ref::<Block>(func.ctx).unwrap();
        println!("Block: {}", block.get_name());
      }
      Either::Right(li) => {
        print_loop_info(li, 0)
      }
    }
  }

  res
}

