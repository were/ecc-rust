use either::Either;
use trinity::ir::{
  module::{Module, namify},
  value::{
    function::FunctionRef,
    instruction::{InstructionRef, InstOpcode, BranchInst, Return, Call},
    block::BlockRef
  },
  VoidType, Argument, Instruction, Block, Function
};

use crate::analysis::topo::{analyze_topology, LoopInfo};

use super::ir::{WASMFunc, WASMInst};


fn emit_instruction(
  inst: &InstructionRef,
  emit_cache: &mut Vec<Emission>) -> Vec<WASMInst> {
  let next_block = emit_cache[inst.get_parent().get_skey()].next_block;
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
    InstOpcode::Branch(_) => {
      let br = inst.as_sub::<BranchInst>().unwrap();
      if let Some(_) = br.cond() {
        let raw_true = br.true_label().unwrap();
        let raw_false = br.false_label().unwrap();
        vec![
          WASMInst::br_if(inst.get_skey(), namify(&raw_false.get_name()), WASMInst::plain("(i32.const 0)".to_string())),
          WASMInst::br(inst.get_skey(), namify(&raw_true.get_name()))]
      } else {
        let raw_dest = br.dest_label().unwrap();
        if raw_dest.get_skey() == next_block {
          vec![WASMInst::plain(format!(";; Linearly, next block is already {}", namify(&raw_dest.get_name())))]
        } else {
          vec![WASMInst::br(inst.get_skey(), namify(&raw_dest.get_name()))]
        }
      }
    }
    // InstOpcode::Return => {
    //   let ret = inst.as_sub::<Return>().unwrap();
    //   if let Some(_) = ret.get_ret_val() {
    //     // res.push_str(format!("{}(br ${})\n", indent, entry_block).as_str());
    //   } else {
    //     // res.push_str(format!("{}(br ${})\n", indent, entry_block).as_str());
    //   }
    // }
    _ => vec![WASMInst::plain(format!(";; Not supported yet: {}", inst.to_string(false)))]
  }
}

struct Emission {
  next_block: usize,
  dump: String,
  register: String,
}

fn emit_loop_or_block<'ctx>(
  loop_or_block: &Either<BlockRef<'ctx>, Box<LoopInfo<'ctx>>>,
  func: &mut WASMFunc,
  emit_label: bool,
  emit_cache: &mut Vec<Emission>) {
  match loop_or_block {
    Either::Left(block) => {
      if emit_label {
        func.insts.push(WASMInst::block_begin(block.get_skey(), namify(&block.get_name())));
      }
      for inst in block.inst_iter() {
        func.insts.extend(emit_instruction(&inst, emit_cache));
      }
      if emit_label {
        func.insts.push(WASMInst::block_end(block.get_skey()));
      }
    }
    Either::Right(li) => {
      func.insts.push(WASMInst::loop_begin(li.get_head().get_skey(),
                      namify(&li.get_head().get_name())));
      for elem in li.child_iter() {
        emit_loop_or_block(elem, func, true, emit_cache);
      }
      func.insts.push(WASMInst::block_end(li.get_head().get_skey()));
    }
  }
}

fn emit_function(func: &FunctionRef,
                 emit_cache: &mut Vec<Emission>) -> WASMFunc {
  let fty = func.get_type();
  let rty = if let None = fty.ret_ty().as_ref::<VoidType>(fty.ctx) { "i32" } else { "" };
  let args = (0..func.get_num_args()).map(|i| {
    let arg = func.get_arg(i);
    let arg = arg.as_ref::<Argument>(func.ctx).unwrap();
    namify(&arg.get_name())
  }).collect::<Vec<String>>();
  let mut res = WASMFunc::new(namify(&func.get_name()), args, rty.to_string());
  let mut visited = vec![false; func.ctx.capacity()];
  let topo = analyze_topology(func, &mut visited);
  for elem in topo.iter() {
    let (skey, label) = match elem {
      Either::Left(block) => {
        (block.get_skey(), namify(&block.get_name()))
      }
      Either::Right(li) => {
        let head = li.get_head();
        (head.get_skey(), namify(&head.get_name()))
      }
    };
    res.insts.push(WASMInst::block_begin(skey, label));
  }
  for elem in topo.iter().rev() {
    let (skey, label) = match elem {
      Either::Left(block) => {
        (block.get_skey(), namify(&block.get_name()))
      }
      Either::Right(li) => {
        let head = li.get_head();
        (head.get_skey(), namify(&head.get_name()))
      }
    };
    res.insts.push(WASMInst::block_end(skey));
    res.insts.last_mut().unwrap().comment = label.to_string();
    emit_loop_or_block(elem, &mut res, false, emit_cache);
  }
  res
}

pub fn emit(module: &Module) -> String {
  let mut emit_cache = Vec::new();
  (0..module.context.capacity()).for_each(|_| {
    emit_cache.push(Emission {
      next_block: 0,
      dump: String::new(),
      register: String::new(),
    });
  });
  let mut res = String::new();
  res.push_str("(module\n");
  res.push_str(" (type (;0;) (func (param i32) (result i32)))\n"); // malloc
  res.push_str(" (type (;1;) (func (param i32 i32)))\n");
  res.push_str(" (import \"env\" \"__linear_memory\" (memory (;0;) 1))\n");
  res.push_str(" (import \"env\" \"malloc\" (func (;0;) (type 0)))\n");
  res.push_str(" (import \"env\" \"__print_str__\" (func (;1;) (type 1)))\n");
  for func in module.func_iter() {
    if func.is_declaration() {
      continue;
    }
    res.push_str(emit_function(&func, &mut emit_cache).to_string().as_str());
  }
  res.push_str(")");
  res
}
