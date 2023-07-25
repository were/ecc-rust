mod ir;

use std::collections::HashSet;

use either::Either;
use trinity::{ir::{
  module::{Module, namify},
  value::{
    function::FunctionRef,
    instruction::{InstructionRef, InstOpcode, BranchInst, Return, Call},
    block::BlockRef
  },
  VoidType, Argument, Instruction, Block, Function
}, context::{Reference, Context}};

use crate::analysis::topo::analyze_topology;

use self::ir::{WASMFunc, WASMInst};


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

struct SCCEntry {
  visited: bool,
  latch: usize,
  sub_topo: Vec<usize>,
}

fn emit_scc(ctx: &Context, scc: &Vec<usize>, emit_cache: &mut Vec<Emission>) -> Vec<WASMInst> {
  let mut res = Vec::new();
  if scc.len() == 1 {
    let block = scc.get(0).unwrap();
    let block = Block::from_skey(*block);
    let block = block.as_ref::<Block>(ctx).unwrap();
    for inst in block.inst_iter() {
      res.extend(emit_instruction(&inst, emit_cache));
    }
  } else {
    for block in scc.iter().map(|x| { let block = Block::from_skey(*x); block.as_ref::<Block>(ctx).unwrap() }) {
      res.push(WASMInst::plain(format!(";; Block {}", namify(&block.get_name()))));
    }
  }
  res
}

fn emit_function(func: &FunctionRef,
                 emit_cache: &mut Vec<Emission>,
                 workspace: &mut Vec<SCCEntry>) -> WASMFunc {
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
        let block = Block::from_skey(*block).as_ref::<Block>(func.ctx).unwrap();
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
        let block = Block::from_skey(*block).as_ref::<Block>(func.ctx).unwrap();
        (block.get_skey(), namify(&block.get_name()))
      }
      Either::Right(li) => {
        let head = li.get_head();
        (head.get_skey(), namify(&head.get_name()))
      }
    };
    res.insts.push(WASMInst::block_end(skey));
    res.insts.last_mut().unwrap().comment = label.to_string();
  }
  res
}

pub(super) fn emit(module: &Module) -> String {
  let mut emit_cache = Vec::new();
  let mut workspace = Vec::new();
  (0..module.context.capacity()).for_each(|_| {
    emit_cache.push(Emission {
      next_block: 0,
      dump: String::new(),
      register: String::new(),
    });
    workspace.push(SCCEntry {
      visited: false,
      latch: 0,
      sub_topo: Vec::new(),
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
    if func.get_num_blocks() == 0 {
      continue;
    }
    res.push_str(emit_function(&func, &mut emit_cache, &mut workspace).to_string().as_str());
  }
  res.push_str(")");
  res
}
