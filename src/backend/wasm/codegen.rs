use std::collections::HashMap;

use either::Either;
use trinity::ir::{
  module::{Module, namify},
  value::{
    function::FunctionRef,
    instruction::{InstOpcode, BranchInst, Return, Call, CompareInst, CmpPred, SubInst, CastOp, BinaryOp},
    block::BlockRef
  },
  VoidType, Argument, Instruction, ValueRef, ConstScalar, VKindCode
};

use crate::analysis::topo::{analyze_topology, LoopInfo};

use super::{ir::{WASMFunc, WASMInst}, analysis::gather_block_downstreams};

pub struct Codegen<'ctx> {
  module: &'ctx Module,
  locals: HashMap<usize, String>,
}

impl <'ctx>Codegen<'ctx> {

  pub fn new(module: &'ctx Module) -> Self {
    Codegen {
      module,
      locals: HashMap::new(),
    }
  }

  fn emit_value(&self, value: &ValueRef, define: bool) -> Vec<WASMInst> {
    let locals = &self.locals;
    if !define {
      if let Some(var_name) = locals.get(&value.skey) {
        return vec![WASMInst::local_get(value.skey, var_name.clone())];
      }
    }
    if let Some(inst) = value.as_ref::<Instruction>(&self.module.context) {
      let mut res = match inst.get_opcode() {
        InstOpcode::Call => {
          let call = inst.as_sub::<Call>().unwrap();
          let callee = call.get_callee().get_name();
          let operands = call.arg_iter().map(|x| self.emit_value(x, false).remove(0));
          vec![WASMInst::call(inst.get_skey(), namify(&callee), operands.collect())]
        }
        InstOpcode::Branch(_) => {
          let br = inst.as_sub::<BranchInst>().unwrap();
          if let Some(cond) = br.cond() {
            let raw_true = br.true_label().unwrap();
            let raw_false = br.false_label().unwrap();
            let mut cond = self.emit_value(cond, false);
            let mut res = vec![
              WASMInst::br_if(inst.get_skey(), namify(&raw_false.get_name()), cond.remove(0)),
              WASMInst::br(inst.get_skey(), namify(&raw_true.get_name()))];
            res[0].comment = br.to_string();
            res
          } else {
            let raw_dest = br.dest_label().unwrap();
            // if raw_dest.get_skey() == next_block {
            //   vec![WASMInst::plain(format!(";; Linearly, next block is already {}", namify(&raw_dest.get_name())))]
            // } else {
            // }
            vec![WASMInst::br(inst.get_skey(), namify(&raw_dest.get_name()))]
          }
        }
        InstOpcode::ICompare(_) => {
          let cmp = inst.as_sub::<CompareInst>().unwrap();
          let mut lhs = self.emit_value(inst.get_operand(0).unwrap(), false);
          let mut rhs = self.emit_value(inst.get_operand(1).unwrap(), false);
          let mut res = match cmp.get_pred() {
            CmpPred::SLT | CmpPred::SGT | CmpPred::SLE | CmpPred::SGE | CmpPred::EQ => {
              vec![WASMInst::cmp(inst.get_skey(), cmp.get_pred().clone(), lhs.remove(0), rhs.remove(0))]
            }
          };
          res.last_mut().unwrap().comment = inst.to_string(false);
          res
        }
        InstOpcode::BinaryOp(op) => {
          let mut lhs = self.emit_value(inst.get_operand(0).unwrap(), false);
          let mut rhs = self.emit_value(inst.get_operand(1).unwrap(), false);
          vec![WASMInst::binop(inst.get_skey(), op, lhs.remove(0), rhs.remove(0))]
        }
        InstOpcode::CastInst(op) => {
          match op {
            CastOp::Bitcast => {
              let mut src = self.emit_value(inst.get_operand(0).unwrap(), false);
              src.last_mut().unwrap().comment = "Bitcast is a noop".to_string();
              vec![src.remove(0)]
            }
            _ => {
              let mut res = vec![WASMInst::iconst(value.skey, 0)];
              res.last_mut().unwrap().comment = format!("Inst not supported yet: {}", inst.to_string(false));
              res
            }
          }
        }
        InstOpcode::Return => {
          let ret = inst.as_sub::<Return>().unwrap();
          if let Some(val) = ret.get_ret_val() {
            let mut ret_val = self.emit_value(val, false);
            vec![WASMInst::ret(inst.get_skey(), Some(ret_val.remove(0)))]
          } else {
            vec![WASMInst::plain(format!(";; ret void as a noop: {}", inst.to_string(false)))]
          }
        }
        InstOpcode::Phi => {
          vec![WASMInst::local_get(inst.get_skey(), namify(&inst.get_name()))]
        }
        InstOpcode::GetElementPtr(_) => {
          let array = inst.get_operand(0).unwrap();
          let array = self.emit_value(array, false).remove(0);
          let idx = inst.get_operand(1).unwrap();
          let idx = self.emit_value(idx, false).remove(0);
          let scalar_size = inst.get_type().get_scalar_size_in_bits(self.module) / 8; // bits / 8 to get byte size
          let scalar_size = WASMInst::iconst(value.skey, scalar_size as u64);
          let offset = WASMInst::binop(value.skey, &BinaryOp::Mul, idx, scalar_size);
          let a_idx = WASMInst::binop(value.skey, &BinaryOp::Add, array, offset);
          let mut res = vec![a_idx];
          res.last_mut().unwrap().comment = format!("gep: {}", inst.to_string(false));
          res
        }
        _ => {
          let mut res = vec![WASMInst::iconst(value.skey, 0)];
          res.last_mut().unwrap().comment = format!("Inst not supported yet: {}", inst.to_string(false));
          res
        }
      };
      if let Some(var_name) = locals.get(&inst.get_skey()) {
        if define {
          let value = res.remove(0);
          res.push(WASMInst::local_set(inst.get_skey(), var_name.clone(), value));
          res.last_mut().unwrap().comment = "Defined here! Write to local variable allocated".to_string();
        } else {
          res.last_mut().unwrap().comment += "; not defined here?";
        }
      }
      res
    } else {
      let res = match value.kind {
        VKindCode::Argument => {
          let arg = value.as_ref::<Argument>(&self.module.context).unwrap();
          vec![WASMInst::local_get(value.skey, namify(&arg.get_name()))]
        }
        VKindCode::ConstScalar => {
          let scalar = value.as_ref::<ConstScalar>(&self.module.context).unwrap();
          vec![WASMInst::iconst(value.skey, scalar.get_value())]
        }
        _ => {
          let mut res = vec![WASMInst::iconst(value.skey, 0)];
          let ctx = &self.module.context;
          res.last_mut().unwrap().comment = format!("Value not supported yet: {}", value.to_string(ctx, false));
          res
        }
      };
      res
    }
  }

  fn emit_loop_or_block(&self, func: &mut WASMFunc, blocks: &Vec<Either<BlockRef<'ctx>, Box<LoopInfo<'ctx>>>>) {

    for elem in blocks.iter() {
      match elem {
        Either::Left(block) => {
          func.insts.push(WASMInst::block_begin(block.get_skey(), namify(&block.get_name())));
          func.insts.last_mut().unwrap().comment = block.get_name();
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
          func.insts.last_mut().unwrap().comment = block.get_name();
          // Gather the constant changes in this block.
          let downstreams = gather_block_downstreams(block);
          for (phi, raw_value) in downstreams {
            let var_name = namify(&phi.get_name());
            let mut value = self.emit_value(&raw_value, false);
            let mut inst = WASMInst::local_set(phi.get_skey(), var_name.clone(), value.remove(0));
            inst.comment = format!("{} of {}", block.get_name(), phi.to_string(false));
            func.insts.push(inst);
          }
          for inst in block.inst_iter() {
            let emit = match inst.get_opcode() {
              InstOpcode::Branch(_) | InstOpcode::Return => {
                // These are side-effect ones.
                true
              }
              InstOpcode::Call => {
                // Call should not be emitted until use.
                let call = inst.as_sub::<Call>().unwrap();
                let callee = call.get_callee();
                let fty = callee.get_type();
                let rty = fty.ret_ty();
                rty.as_ref::<VoidType>(inst.ctx).is_some()
              }
              InstOpcode::Phi => {
                // Phi should not be emitted until use.
                false
              }
              _ => {
                self.locals.contains_key(&inst.get_skey())
              }
            };
            if emit {
              let value = inst.as_super();
              func.insts.extend(self.emit_value(&value, true));
            } else {
              func.insts.push(WASMInst::plain(format!(";; Skip for now: {}", inst.to_string(false))));
            }

          }
        }
        Either::Right(li) => {
          let head = li.get_head();
          func.insts.push(WASMInst::block_end(head.get_skey()));
          func.insts.push(WASMInst::loop_begin(head.get_skey(), namify(&head.get_name())));
          self.emit_loop_or_block(func, li.children());
          func.insts.push(WASMInst::block_end(head.get_skey()));
        }
      }
    }

  }

  fn emit_function(&mut self, func: &FunctionRef, visited: &mut Vec<bool>) -> WASMFunc {
    let fty = func.get_type();
    let rty = if let None = fty.ret_ty().as_ref::<VoidType>(fty.ctx) { "i32" } else { "" };
    let args = (0..func.get_num_args()).map(|i| {
      let arg = func.get_arg(i);
      let arg = arg.as_ref::<Argument>(func.ctx).unwrap();
      namify(&arg.get_name())
    }).collect::<Vec<String>>();
    let mut emit_func = WASMFunc::new(namify(&func.get_name()), args, rty.to_string());
    self.locals = super::analysis::gather_locals(func);
    let blocks = analyze_topology(&func, visited);
    // Clear the locals, and put it in this finalized function.
    self.emit_loop_or_block(&mut emit_func, &blocks);
    std::mem::swap(&mut self.locals, &mut emit_func.locals);
    return emit_func;
  }

  pub fn emit(&mut self) -> String {
    let module = self.module;
    let mut res = String::new();
    let mut visited = vec![false; module.context.capacity()];
    res.push_str("(module\n");
    res.push_str(" (type (;0;) (func (param i32) (result i32)))\n"); // malloc
    res.push_str(" (type (;1;) (func (param i32 i32)))\n");
    res.push_str(" (import \"env\" \"__linear_memory\" (memory (;0;) 1))\n");
    res.push_str(" (import \"env\" \"malloc\" (func $malloc (type 0)))\n");
    res.push_str(" (import \"env\" \"__print_str__\" (func $__print_str__ (type 1)))\n");
    for i in 0..module.get_num_functions() {
      let func = module.get_function(i).unwrap();
      if func.is_declaration() {
        continue;
      }
      res.push_str(self.emit_function(&func, &mut visited).to_string().as_str());
    }
    res.push_str(")");
    res
  }

}

