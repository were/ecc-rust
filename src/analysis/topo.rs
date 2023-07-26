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
  children: Vec<Either<BlockRef<'ctx>, Box<LoopInfo<'ctx>>>>,
}

pub fn print_loop_info<'ctx>(li: &LoopInfo<'ctx>, indent: usize) {
  println!("{}Loop <head:{}><exit:{}><latch:{}>",
    " ".repeat(indent),
    li.get_head().get_name(), li.get_exit().get_name(), li.get_latch().to_string(false));
  let indent = indent + 1;
  for elem in li.children.iter() {
    match elem {
      Either::Left(block) => {
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

  pub fn get_latch(&self) -> InstructionRef<'ctx> {
    Instruction::from_skey(self.latch).as_ref::<Instruction>(self.ctx).unwrap()
  }

  pub fn get_head(&'ctx self) -> BlockRef<'ctx> {
    let head = Block::from_skey(self.head).as_ref::<Block>(self.ctx).unwrap();
    return head;
  }

  pub fn get_exit(&'ctx self) -> BlockRef<'ctx> {
    let exit = Block::from_skey(self.exit).as_ref::<Block>(self.ctx).unwrap();
    return exit;
  }

  pub fn child_iter(&'ctx self) -> impl Iterator<Item=&Either<BlockRef<'ctx>, Box<LoopInfo<'ctx>>>> {
    self.children.iter()
  }

  pub fn children(&'ctx self) -> &Vec<Either<BlockRef<'ctx>, Box<LoopInfo<'ctx>>>> {
    &self.children
  }

}

pub fn dfs_topology<'ctx>(
  ctx: &'ctx Context,
  cur: &'_ BlockRef,
  visited: &mut Vec<bool>,
  loop_stack: &mut Vec<LoopInfo<'ctx>>,
  finalized_loops: &mut Vec<LoopInfo<'ctx>>,
  res: &mut Vec<Either<usize, LoopInfo<'ctx>>>) {

  let is_head = if let Some(latch) = cur.is_loop_head() {
    // eprintln!("Loop latch: {}", latch.to_string(false));
    loop_stack.push(LoopInfo::new(ctx, latch.get_skey(), cur.get_skey()));
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
      x.get_exit().get_skey() != dst_key
    });
    if not_an_exit {
      if !visited[dst_key] {
        // eprintln!("First visit, push {} to stack!", succ.get_name());
        visited[dst_key] = true;
        dfs_topology(ctx, &succ, visited, loop_stack, finalized_loops, res);
      }
    }
  }


  let cleanup = |block: &BlockRef<'_>, loop_stack: &mut Vec<LoopInfo<'ctx>>, res: &mut Vec<Either<usize, LoopInfo<'ctx>>>| {
    if let Some(cur_loop) = loop_stack.last_mut() {
      // TODO(@were): Make this later a method?
      let clone = block.as_super().as_ref::<Block>(ctx).unwrap();
      cur_loop.children.push(Either::Left(clone));
    } else {
      res.push(Either::Left(block.get_skey()));
    }
  };

  cleanup(cur, loop_stack, res);

  if is_head {
    let to_finalize = loop_stack.pop().unwrap();
    // eprintln!("Push exit block: {}", to_finalize.get_exit().get_name());
    print_loop_info(&to_finalize, 0);
    // TODO(@were): The lifetime management is not elegant enough here.
    //              I can only reconstruct the exit block.
    let exit_block = to_finalize.get_exit().as_super();
    let exit_block = exit_block.as_ref::<Block>(ctx).unwrap();
    finalized_loops.push(to_finalize);

    if !visited[exit_block.get_skey()] {
      visited[exit_block.get_skey()] = true;
      dfs_topology(ctx, &exit_block, visited, loop_stack, finalized_loops, res);
    }

    // eprintln!("Finalized loop: {}", exit_block.get_name());
    let finalized = finalized_loops.pop().unwrap();
    // print_loop_info(&finalized, 0);

    if let Some(parent) = loop_stack.last_mut() {
      parent.children.push(Either::Right(Box::new(finalized)));
    } else {
      res.push(Either::Right(finalized));
    }
  }


}

pub fn analyze_topology<'ctx>(func: &'ctx FunctionRef, visited: &mut Vec<bool>) -> Vec<Either<BlockRef<'ctx>, Box<LoopInfo<'ctx>>>> {
  let mut loop_stack = Vec::new();
  let mut finalized_loops = Vec::new();
  let mut res = Vec::new();


  let entry = func.get_block(0).unwrap();
  visited[entry.get_skey()] = true;
  dfs_topology(func.ctx, &entry, visited, &mut loop_stack, &mut finalized_loops, &mut res);

  // println!("Function {}:", func.get_name());
  // for elem in res.iter() {
  //   match elem {
  //     Either::Left(block) => {
  //       let block = Block::from_skey(*block).as_ref::<Block>(func.ctx).unwrap();
  //       println!(" Block: {}", block.get_name());
  //     }
  //     Either::Right(li) => {
  //       print_loop_info(li, 1)
  //     }
  //   }
  // }

  assert!(finalized_loops.is_empty(), "There are still some loops not finalized!");

  res.into_iter().map(|x| {
    match x {
      Either::Left(block) => Either::Left(Block::from_skey(block).as_ref::<Block>(func.ctx).unwrap()),
      Either::Right(li) => Either::Right(Box::new(li)),
    }
  }).collect::<Vec<_>>()
}

