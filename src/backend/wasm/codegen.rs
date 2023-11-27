use std::collections::HashMap;

use trinity::{ir::{
  module::{Module, namify},
  value::{
    function::FunctionRef,
    instruction::{
      InstOpcode, BranchInst, Return, Call, CompareInst, CmpPred, SubInst,
      CastOp, BinaryOp, Store, CastInst, Load
    },
    consts::ConstObject
  },
  VoidType, Argument, Instruction, ValueRef, ConstScalar, VKindCode,
  ConstArray, StructType, ConstExpr, PointerType
}, context::Reference};

use crate::analysis::topo::{analyze_topology, Node, ChildTraverse, FuncTopoInfo};

use super::{ir::{WASMFunc, WASMInst}, analysis::gather_block_downstreams};

pub struct Codegen<'ctx> {
  module: &'ctx Module,
  locals: HashMap<usize, String>,
  global_offset: usize,
  global_buffer: Vec<Vec<u8>>,
  allocated_globals: HashMap<usize, usize>,
}

impl <'ctx>Codegen<'ctx> {

  pub fn new(module: &'ctx Module) -> Self {
    Codegen {
      module,
      locals: HashMap::new(),
      global_offset: 0,
      global_buffer: vec![],
      allocated_globals: HashMap::new(),
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
              WASMInst::br_if(inst.get_skey(), namify(&raw_true.get_name()), cond.remove(0)),
              WASMInst::br(inst.get_skey(), namify(&raw_false.get_name()))];
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
            CmpPred::SLT | CmpPred::SGT | CmpPred::SLE | CmpPred::SGE | CmpPred::EQ | CmpPred::NE => {
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
              src
            }
            CastOp::SignExt | CastOp::ZeroExt => {
              let mut src = self.emit_value(inst.get_operand(0).unwrap(), false);
              src.last_mut().unwrap().comment = "SignExt&ZeroExt is a noop".to_string();
              src
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
            vec![WASMInst::ret(inst.get_skey(), None)]
            // vec![WASMInst::plain(format!(";; ret void as a noop: {}", inst.to_string(false)))]
          }
        }
        InstOpcode::Phi => {
          vec![WASMInst::local_get(inst.get_skey(), namify(&inst.get_name()))]
        }
        InstOpcode::GetElementPtr(_) => {
          let array_ptr = inst.get_operand(0).unwrap();
          let mut array = self.emit_value(array_ptr, false).remove(0);
          array.comment = "array".to_string();
          let operand_idx = inst.get_operand(1).unwrap();
          let idx = self.emit_value(operand_idx, false).remove(0);
          let ptr_ty = array_ptr.get_type(&self.module.context).as_ref::<PointerType>(&self.module.context).unwrap();
          let scalar_size = ptr_ty.get_pointee_ty().get_scalar_size_in_bits(self.module) / 8; // bits / 8 to get byte size
          let mut scalar_size = WASMInst::iconst(value.skey, scalar_size as u64);
          scalar_size.comment = format!("scalar size of {}", ptr_ty.get_pointee_ty().to_string(&self.module.context));
          let mut idx = WASMInst::binop(value.skey, &BinaryOp::Mul, idx, scalar_size);
          idx.comment = format!("idx: {}", operand_idx.to_string(&self.module.context, true));
          let mut addr = WASMInst::binop(value.skey, &BinaryOp::Add, array, idx);
          if let Some(sty) = ptr_ty.get_pointee_ty().as_ref::<StructType>(&self.module.context) {
            if let Some(offset) = inst.get_operand(2) {
              let ci = offset.as_ref::<ConstScalar>(&self.module.context).unwrap();
              let offset = sty.get_offset_in_bytes(&self.module, ci.get_value() as usize);
              let mut offset = WASMInst::iconst(value.skey, offset as u64);
              offset.comment = format!("offset: {}", ci.to_string());
              addr = WASMInst::binop(value.skey, &BinaryOp::Add, addr, offset);
            }
          }
          let mut res = addr;
          res.comment = format!("gep: {}", inst.to_string(false));
          vec![res]
        }
        InstOpcode::Store(_) => {
          let store = inst.as_sub::<Store>().unwrap();
          let mut ptr = self.emit_value(store.get_ptr(), false);
          let mut value = store.get_value().clone();
          let module = self.module;
          let ctx = &self.module.context;
          let mut bits = value.get_type(ctx).get_scalar_size_in_bits(module);
          let mut comment = format!("store: {}", inst.to_string(false));
          if let Some(inst) = value.as_ref::<Instruction>(ctx) {
            if let Some(_) = inst.as_sub::<CastInst>() {
              value = inst.get_operand(0).unwrap().clone();
              bits = inst.get_type().get_align_in_bits(module);
              comment = format!("{} ; cast ({}) fused in this store", comment, value.to_string(ctx, false));
            }
          }
          let mut value = self.emit_value(&value, false);
          let mut res = WASMInst::store(inst.get_skey(), bits, value.remove(0), ptr.remove(0));
          res.comment = comment;
          return vec![res];
        }
        InstOpcode::Load(_) => {
          let load = inst.as_sub::<Load>().unwrap();
          let mut value = self.emit_value(load.get_ptr(), false);
          let bits = inst.get_type().get_scalar_size_in_bits(self.module);
          let mut res = WASMInst::load(inst.get_skey(), bits, value.remove(0));
          res.comment = format!("load: {}", inst.to_string(false));
          if bits == 8 {
            vec![WASMInst::binop(inst.get_skey(), &BinaryOp::And, res, WASMInst::iconst(0, 255))]
          } else {
            vec![res]
          }
        }
        InstOpcode::Select => {
          let cond = self.emit_value(inst.get_operand(0).unwrap(), false).remove(0);
          let tv = self.emit_value(inst.get_operand(1).unwrap(), false).remove(0);
          let fv = self.emit_value(inst.get_operand(2).unwrap(), false).remove(0);
          vec![WASMInst::select(inst.get_skey(), cond, tv, fv)]
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
        VKindCode::ConstObject => {
          let co = value.as_ref::<ConstObject>(&self.module.context).unwrap();
          let addr = self.allocated_globals.get(&co.get_skey()).unwrap();
          let mut res = vec![WASMInst::plain(format!("(i32.const {})", addr))];
          res.last_mut().unwrap().comment = format!("ConstObject: {}", co.to_string());
          res
        }
        VKindCode::ConstExpr => {
          let expr = value.as_ref::<ConstExpr>(&self.module.context).unwrap();
          let inst = expr.get_inst();
          let inst = Reference::new(&self.module.context, inst);
          match inst.get_opcode() {
            InstOpcode::GetElementPtr(_) => {
              let ptr = inst.get_operand(0).unwrap();
              let value = self.allocated_globals.get(&ptr.skey).unwrap();
              let mut res = vec![WASMInst::iconst(expr.get_skey(), *value as u64)];
              res.last_mut().unwrap().comment = format!("ConstExpr: {}", expr.to_string());
              res
            }
            _ => {
              panic!("ConstExpr::to_string: not a constant opcode {:?}",
                     inst.get_opcode().to_string());
            }
          }
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

  fn emit_loop_or_block(&self, func: &mut WASMFunc, iter: impl Iterator<Item = Node<'ctx>>) {
    let blocks = iter.collect::<Vec<_>>();

    for elem in blocks.iter() {
      match elem {
        Node::Block(block) => {
          let mut label = WASMInst::block_begin(block.get_skey(), namify(&block.get_name()));
          label.comment = block.get_name();
          func.push(label);
        }
        Node::Loop(li) => {
          let head = li.get_head();
          let mut label = WASMInst::block_begin(head.get_skey(), namify(&head.get_name()));
          label.comment = head.get_name();
          func.push(label);
        }
      }
    }

    for elem in blocks.iter().rev() {
      match elem {
        Node::Block(block) => {
          let mut block_end = WASMInst::block_end(block.get_skey());
          block_end.comment = block.get_name();
          func.push(block_end);

          // Gather the constant changes in this block.
          let downstreams = gather_block_downstreams(&block);

          for inst in block.inst_iter() {
            let emit = match inst.get_opcode() {
              // These are side-effect ones.
              InstOpcode::Branch(_) => {
                for (phi, raw_value) in downstreams.iter() {
                  let var_name = namify(&phi.get_name());
                  let mut value = self.emit_value(&raw_value, false);
                  let mut inst =
                    WASMInst::local_set(phi.get_skey(), var_name.clone(), value.remove(0));
                  inst.comment = format!("{} of {}", block.get_name(), phi.to_string(false));
                  func.push(inst);
                }
                true
              }
              // These are side-effect ones.
              InstOpcode::Return | InstOpcode::Store(_) => {
                true
              }
              InstOpcode::Call => {
                if self.locals.contains_key(&inst.get_skey()) {
                  true
                } else {
                  // Call should not be emitted until use.
                  let call = inst.as_sub::<Call>().unwrap();
                  let callee = call.get_callee();
                  let fty = callee.get_type();
                  let rty = fty.ret_ty();
                  rty.as_ref::<VoidType>(inst.ctx()).is_some()
                }
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
              func.extend(self.emit_value(&value, true));
            } else {
              func.push(WASMInst::plain(format!(";; Skip for now: {}", inst.to_string(false))));
            }
          }

        }
        Node::Loop(li) => {
          let head = li.get_head();
          func.push(WASMInst::block_end(head.get_skey()));
          func.push(WASMInst::loop_begin(head.get_skey(), namify(&head.get_name())));
          self.emit_loop_or_block(func, li.child_iter());
          func.push(WASMInst::block_end(head.get_skey()));
        }
      }
    }

  }

  fn emit_function(&mut self, func: &FunctionRef, func_topo: &FuncTopoInfo) -> WASMFunc {
    let fty = func.get_type();
    let rty = if let None = fty.ret_ty().as_ref::<VoidType>(fty.ctx()) { "i32" } else { "" };
    let args = (0..func.get_num_args()).map(|i| {
      let arg = func.get_arg(i);
      let arg = arg.as_ref::<Argument>(func.ctx()).unwrap();
      namify(&arg.get_name())
    }).collect::<Vec<String>>();
    let mut emit_func = WASMFunc::new(namify(&func.get_name()), args, rty.to_string());
    self.locals = super::analysis::gather_locals(func);
    // Clear the locals, and put it in this finalized function.
    self.emit_loop_or_block(&mut emit_func, func_topo.child_iter());
    std::mem::swap(&mut self.locals, &mut emit_func.locals);
    return emit_func;
  }

  pub fn to_linear_buffer(&mut self, gv: &ValueRef) -> Vec<u8> {
    match gv.kind {
      VKindCode::ConstExpr => {
        let gv = gv.as_ref::<ConstExpr>(&self.module.context).unwrap();
        let inst = gv.get_inst();
        let inst = Reference::new(&self.module.context, inst);
        match inst.get_opcode() {
          InstOpcode::GetElementPtr(_) => {
            let ptr = inst.get_operand(0).unwrap();
            let value = self.allocated_globals.get(&ptr.skey).unwrap();
            let mut res = value.to_le_bytes().to_vec();
            res.resize(self.module.tm.get_pointer_size_in_bits() / 8, 0);
            res
            // let ptr_ty = ptr_ty.as_ref::<PointerType>(&self.module.context).unwrap();
            // let ptr_scalar = ptr_ty.get_pointee_ty();
            // let opcode = inst.get_opcode();
          }
          _ => {
            panic!("ConstExpr::to_string: not a constant opcode {:?}", inst.get_opcode().to_string());
          }
        }
      },
      VKindCode::ConstArray => {
        let ca = gv.as_ref::<ConstArray>(&self.module.context).unwrap();
        let mut res = vec![];
        ca.get_value().iter().for_each(|x| {
          res.extend(self.to_linear_buffer(x))
        });
        res
      }
      VKindCode::ConstObject => {
        let co = gv.as_ref::<ConstObject>(&self.module.context).unwrap();
        let ptr_ty = co.get_type().as_ref::<PointerType>(&self.module.context).unwrap();
        let sty = ptr_ty.get_pointee_ty().as_ref::<StructType>(&self.module.context).unwrap();
        let mut res = vec![];
        co.get_value().iter().enumerate().for_each(|(i, x)| {
          let n = sty.get_offset_in_bytes(&self.module, i);
          assert!(res.len() <= n);
          while res.len() < n {
            res.push(0);
          }
          if let Some(obj_init) = x.as_ref::<ConstObject>(&self.module.context) {
            let value = self.allocated_globals.get(&obj_init.get_skey()).unwrap();
            res.extend(value.to_le_bytes().to_vec());
          } else {
            res.extend(self.to_linear_buffer(x))
          }
        });
        res
      }
      VKindCode::ConstScalar => {
        let cs = gv.as_ref::<ConstScalar>(&self.module.context).unwrap();
        let mut res = cs.get_value().to_le_bytes().to_vec();
        res.resize(cs.get_type().get_scalar_size_in_bits(self.module) / 8, 0);
        res
      }
      _ => { panic!("Unknown const variable kind"); }
    }
  }

  pub fn initialize_global_values(&mut self) {
    let module = self.module;
    for i in 0..module.get_num_gvs() {
      let gv = module.get_gv(i);
      {
        let offset = &mut self.global_offset;
        let rem = *offset % (gv.get_type(&module.context).get_align_in_bits(&module) / 8);
        if rem != 0 {
          *offset += gv.get_type(&module.context).get_align_in_bits(&module) / 8 - rem;
        }
        self.allocated_globals.insert(gv.skey, *offset);
      }
      let buffer = self.to_linear_buffer(&gv);
      self.global_buffer.push(buffer);
      {
        let offset = &mut self.global_offset;
        let ty = gv.get_type(&module.context);
        let ty = ty.as_ref::<PointerType>(&module.context).unwrap();
        // eprintln!("ty: {}", ty.get_pointee_ty().to_string(&module.context));
        *offset += ty.get_pointee_ty().get_scalar_size_in_bits(&module) / 8;
        // eprintln!("Global {}'s size is: {} byte(s)", i, ty.get_pointee_ty().get_scalar_size_in_bits(&module) / 8)
      }
    }
  }

  pub fn emit(&mut self) -> String {
    let module = self.module;
    let mut res = String::new();
    let topo = analyze_topology(self.module);
    res.push_str("(module\n");
    res.push_str(" (type (;0;) (func (param i32) (result i32)))\n"); // malloc
    res.push_str(" (type (;1;) (func (param i32)))\n");
    res.push_str(" (type (;2;) (func (result i32)))\n");
    res.push_str(" (import \"env\" \"__linear_memory\" (memory (;0;) 1))\n");
    res.push_str(" (import \"env\" \"malloc\" (func $malloc (type 0)))\n");
    res.push_str(" (import \"env\" \"__print_str__\" (func $__print_str__ (type 1)))\n");
    res.push_str(" (import \"env\" \"__print_int__\" (func $__print_int__ (type 1)))\n");
    res.push_str(" (import \"env\" \"nextInt\" (func $nextInt (type 2)))\n");
    self.initialize_global_values();
    for i in 0..module.get_num_functions() {
      let func = module.get_function(i).unwrap();
      if func.is_declaration() {
        continue;
      }
      let func_topo = topo.get_function(func.get_skey());
      res.push_str(self.emit_function(&func, &func_topo).to_string().as_str());
    }
    for i in 0..module.get_num_gvs() {
      let gv = module.get_gv(i);
      let name = match gv.kind {
        VKindCode::ConstExpr => {
          "".to_string()
        },
        VKindCode::ConstArray => {
          let ca = gv.as_ref::<ConstArray>(&module.context).unwrap();
          ca.get_name()
        }
        VKindCode::ConstObject => {
          let co = gv.as_ref::<ConstObject>(&module.context).unwrap();
          co.get_name()
        }
        VKindCode::ConstScalar => {
          "".to_string()
        }
        _ => { panic!("Unknown const variable kind"); }
      };
      res.push_str(format!(" (data ${} (i32.const {})", name, *self.allocated_globals.get(&gv.skey).unwrap()).as_str());
      res.push_str(" \"");
      let init = self.global_buffer.get(i).unwrap();
      init.iter().for_each(|x| {
        res.push_str(&format!("\\{:02x}", x));
      });
      res.push_str("\")\n");
    }

    res.push_str(")");
    res
  }

}

