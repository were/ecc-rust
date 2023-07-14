use trinity::{ir::{
  module::{Module, namify},
  value::{
    function::FunctionRef,
    instruction::{InstructionRef, InstOpcode, BranchInst, Return, Call}, block::BlockRef
  },
  VoidType, Argument, Instruction, Block, Function
}, context::Reference};

fn emit_instruction(
  inst: &InstructionRef,
  res: &mut String,
  indent: &String,
  emit_cache: &mut Vec<Emission>,
  entry_block: &String) {
  res.push_str(format!("{};; {}\n", indent, inst.to_string(false)).as_str());
  match inst.get_opcode() {
    // InstOpcode::Call => {
    //   let call = inst.as_sub::<Call>().unwrap();
    //   if let Some(func) = call.get_callee().as_ref::<Function>(inst.ctx) {
    //     let fname = format!("${}", namify(&func.get_name()));
    //     for i in 0..call.get_num_args() {
    //       call.get_arg(i);
    //     }
    //   }
    // }
    InstOpcode::Branch => {
      let next_block = emit_cache[inst.get_parent().get_skey()].next_block;
      let br = inst.as_sub::<BranchInst>().unwrap();
      if let Some(_) = br.cond() {
        let raw_true = br.true_label().unwrap();
        let raw_false = br.false_label().unwrap();
        // TODO(@were): flip the condition.
        let (dest_label, flip) = if raw_true.get_skey() == next_block {
          (raw_false, true)
        } else {
          assert_eq!(raw_false.get_skey(), next_block);
          (raw_true, false)
        };
        res.push_str(
          format!("{}(br_if ${} (i32.const 0)) ;; direct block is block.{}, flip the cond? {}\n",
                  indent, namify(&dest_label.get_name()), next_block, flip).as_str()
        );
      } else {
        let raw_dest = br.dest_label().unwrap();
        if raw_dest.get_skey() == next_block {
          res.push_str(format!("{};; Next block is {}", indent, raw_dest.get_name()).as_str());
        }
        res.push_str(format!("{}(br ${} (i32.const 0))\n", indent, namify(&raw_dest.get_name())).as_str());
      }
    }
    InstOpcode::Return => {
      let ret = inst.as_sub::<Return>().unwrap();
      if let Some(_) = ret.get_ret_val() {
        res.push_str(format!("{}(br ${})\n", indent, entry_block).as_str());
      } else {
        res.push_str(format!("{}(br ${})\n", indent, entry_block).as_str());
      }
    }
    _ => res.push_str(format!("{};; Not supported yet\n", indent).as_str())
  }
}

struct Emission {
  next_block: usize,
  dump: String,
  register: String,
  stamp: usize,
}

fn emit_function(func: &FunctionRef, res: &mut String, emit_cache: &mut Vec<Emission>) {
  // emit the function signature.
  res.push_str(" (func $");
  res.push_str(namify(&func.get_name()).as_str());
  if func.get_num_args() > 0 {
    for i in 0..func.get_num_args() {
      let arg = func.get_arg(i);
      let arg = arg.as_ref::<Argument>(func.ctx).unwrap();
      res.push_str(format!(" (param ${} i32)", namify(&arg.get_name())).as_str());
      emit_cache[arg.get_skey()].dump = format!("(local.get ${})", namify(&arg.get_name()));
    }
  }
  let fty = func.get_type();
  if let None = fty.ret_ty().as_ref::<VoidType>(fty.ctx) {
    res.push_str(" (result i32)");
  }
  res.push_str("\n");
  // emit the function body.
  // sort a topological order of the function blocks.
  let topo = {
    let mut topo: Vec<BlockRef> = Vec::new();
    let mut stack = Vec::new();
    stack.push((func.get_block(0).unwrap(), 0 as usize));
    let mut cnt = 1;
    emit_cache[func.get_block(0).unwrap().get_skey()].stamp = cnt;
    while let Some((block, idx)) = stack.last().clone() {
      if let Some(inst) = block.last_inst() {
        if let Some(br) = inst.as_sub::<BranchInst>() {
          if let Some(succ) = br.get_successors().get(*idx) {
            let succ = succ.as_ref::<Block>(func.ctx).unwrap();
            if emit_cache[succ.get_skey()].stamp == 0 {
              cnt += 1;
              emit_cache[succ.get_skey()].stamp = cnt;
              stack.push((succ, 0 as usize));
              continue;
            }
          }
        }
      }
      if let Some(next) = topo.last() {
        let stack_top = stack.last().unwrap();
        emit_cache[stack_top.0.get_skey()].next_block = next.get_skey();
      }
      topo.push(stack.pop().unwrap().0);
      if let Some(stack_top) = stack.last_mut() {
        stack_top.1 += 1;
      }
    }
    topo
  };
  let entry_block = namify(&topo.get(0).unwrap().get_name());
  for (i, block) in topo.iter().rev().enumerate() {
    let indent = " ".repeat(i + 3);
    let block_type = "block";
    res.push_str(format!("{}({} ${}\n", indent, block_type, namify(&block.get_name())).as_str());
  }
  for (i, block) in topo.iter().enumerate() {
    let indent = " ".repeat(topo.len() - i + 3);
    res.push_str(format!("{};; Emitting block {}  pred: [", indent, block.get_name()).as_str());
    block.pred_iter().for_each(|pred| {
      res.push_str(" ");
      res.push_str(pred.get_parent().get_name().as_str())
    });
    res.push_str(" ]\n");
    for inst in block.inst_iter() {
      emit_instruction(&inst, res, &indent, emit_cache, &entry_block);
    }
    res.push_str(format!("{}) ;; End of {}\n", " ".repeat(topo.len() - i + 2), namify(&block.get_name())).as_str());
  }
  res.push_str(" )\n");
}

pub(super) fn emit(module: &Module) -> String {
  let mut emit_cache = Vec::new();
  (0..module.context.capacity()).for_each(|_| {
    emit_cache.push(Emission { next_block: 0, dump: String::new(), register: String::new(), stamp: 0 });
  });
  let mut res = String::new();
  res.push_str("(module\n");
  res.push_str(" (type (;0;) (func (param i32) (result i32)))\n"); // malloc
  res.push_str(" (type (;1;) (func (param i32 i32)))\n");
  res.push_str(" (import \"env\" \"__linear_memory\" (memory (;0;) 1))\n");
  res.push_str(" (import \"env\" \"malloc\" (func (;0;) (type 0)))\n");
  res.push_str(" (import \"env\" \"__print_str__\" (func (;1;) (type 1)))\n");
  for func in module.iter() {
    if func.get_num_blocks() == 0 {
      continue;
    }
    emit_function(&func, &mut res, &mut emit_cache);
  }
  res.push_str(")");
  res
}
