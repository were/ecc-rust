use either::Either;
use trinity::{ir::{
  module::{Module, namify},
  value::{
    function::FunctionRef,
    instruction::{InstructionRef, InstOpcode, BranchInst, Return, Call, CompareInst, CmpPred, SubInst},
    block::BlockRef
  },
  VoidType, Argument, Instruction, Block, Function, ValueRef
}, context::Context};

use crate::analysis::topo::{analyze_topology, LoopInfo};

use super::ir::{WASMFunc, WASMInst};


fn emit_value(ctx: &Context, value: ValueRef, emit_cache: &mut Vec<Emission>) -> Vec<WASMInst> {
  if let Some(inst) = value.as_ref::<Instruction>(ctx) {
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
      // InstOpcode::Branch(_) => {
      //   let br = inst.as_sub::<BranchInst>().unwrap();
      //   if let Some(_) = br.cond() {
      //     let raw_true = br.true_label().unwrap();
      //     let raw_false = br.false_label().unwrap();
      //     let mut res = vec![
      //       WASMInst::br_if(inst.get_skey(), namify(&raw_false.get_name()), WASMInst::plain("(i32.const 0)".to_string())),
      //       WASMInst::br(inst.get_skey(), namify(&raw_true.get_name()))];
      //     res[0].comment = br.to_string();
      //     res
      //   } else {
      //     let raw_dest = br.dest_label().unwrap();
      //     if raw_dest.get_skey() == next_block {
      //       vec![WASMInst::plain(format!(";; Linearly, next block is already {}", namify(&raw_dest.get_name())))]
      //     } else {
      //       vec![WASMInst::br(inst.get_skey(), namify(&raw_dest.get_name()))]
      //     }
      //   }
      // }
      // InstOpcode::ICompare(_) => {
      //   let cmp = inst.as_sub::<CompareInst>().unwrap();
      //   let mut lhs = emit_value(ctx, inst.get_operand(0).unwrap().clone(), emit_cache);
      //   let mut rhs = emit_value(ctx, inst.get_operand(1).unwrap().clone(), emit_cache);
      //   match cmp.get_pred() {
      //     CmpPred::SLT | CmpPred::SGT | CmpPred::SLE | CmpPred::SGE | CmpPred::EQ => {
      //       return vec![WASMInst::cmp(inst.get_skey(), cmp.get_pred().clone(), lhs.remove(0), rhs.remove(0))];
      //     }
      //   }
      // }
      InstOpcode::Return => {
        let ret = inst.as_sub::<Return>().unwrap();
        if let Some(val) = ret.get_ret_val() {
          // let mut ret_val = emit_value(ctx, val.clone(), emit_cache);
          // vec![WASMInst::ret(inst.get_skey(), Some(ret_val.remove(0)))]
          vec![WASMInst::ret(inst.get_skey(), Some(WASMInst::iconst(val.skey, 0)))]
        } else {
          vec![WASMInst::plain(format!(";; ret void as a noop: {}", inst.to_string(false)))]
        }
      }
      _ => {
        vec![]
        // let mut res = vec![WASMInst::iconst(value.skey, 0)];
        // res.last_mut().unwrap().comment = format!("Inst not supported yet: {}", inst.to_string(false));
        // res
      }
    }
  } else {
    vec![]
    // let mut res = vec![WASMInst::iconst(value.skey, 0)];
    // res.last_mut().unwrap().comment = format!("Value not supported yet: {}", value.to_string(ctx, false));
    // res
  }
}

struct Emission {
  next_block: usize,
  dump: String,
  register: String,
}

fn emit_loop_or_block<'ctx>(
  blocks: &Vec<Either<BlockRef<'ctx>, Box<LoopInfo<'ctx>>>>,
  func: &mut WASMFunc,
  emit_cache: &mut Vec<Emission>) {

  for elem in blocks.iter() {
    match elem {
      Either::Left(block) => {
        func.insts.push(WASMInst::block_begin(block.get_skey(), namify(&block.get_name())));
        func.insts.last_mut().unwrap().comment = block.get_name();
        // for inst in block.inst_iter() {
        //   let value = inst.as_super();
        //   func.insts.extend(emit_value(inst.ctx, value, emit_cache));
        // }
      }
      Either::Right(li) => {
        let head = li.get_head();
        func.insts.push(WASMInst::block_begin(head.get_skey(), namify(&head.get_name())));
      }
    }
  }

  for elem in blocks.iter().rev() {
    match elem {
      Either::Left(block) => {
        func.insts.push(WASMInst::block_end(block.get_skey()));
        for inst in block.inst_iter() {
          let value = inst.as_super();
          func.insts.extend(emit_value(inst.ctx, value, emit_cache));
        }
      }
      Either::Right(li) => {
        let head = li.get_head();
        func.insts.push(WASMInst::block_end(head.get_skey()));
        func.insts.push(WASMInst::loop_begin(head.get_skey(), namify(&head.get_name())));
        emit_loop_or_block(li.children(), func, emit_cache);
        func.insts.push(WASMInst::block_end(head.get_skey()));
      }
    }
  }

}

fn emit_function(func: &FunctionRef,
                 emit_cache: &mut Vec<Emission>,
                 visited: &mut Vec<bool>) -> WASMFunc {
  let fty = func.get_type();
  let rty = if let None = fty.ret_ty().as_ref::<VoidType>(fty.ctx) { "i32" } else { "" };
  let args = (0..func.get_num_args()).map(|i| {
    let arg = func.get_arg(i);
    let arg = arg.as_ref::<Argument>(func.ctx).unwrap();
    namify(&arg.get_name())
  }).collect::<Vec<String>>();
  let mut res = WASMFunc::new(namify(&func.get_name()), args, rty.to_string());
  let topo = analyze_topology(func, visited);

  emit_loop_or_block(&topo, &mut res, emit_cache);

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
  let mut visited = vec![false; module.context.capacity()];
  for func in module.func_iter() {
    if func.is_declaration() {
      continue;
    }
    res.push_str(emit_function(&func, &mut emit_cache, &mut visited).to_string().as_str());
  }
  res.push_str(")");
  res
}
