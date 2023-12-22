mod typegen;

use std::rc::Rc;
use std::collections::HashMap;

use trinity::ir::{
  self,
  value::{
    ValueRef, Function,
    instruction::{InstOpcode, Alloca, BranchMetadata}
  },
  types::TypeRef,
  Block, Instruction, module::Module, TKindCode, StructType,
};
use trinity::builder::Builder;
use self::typegen::{TypeGen, CGType};

use super::{
  ast::{self, ForStmt, WhileStmt, IfStmt, ReturnStmt},
  lexer::TokenType
};

struct ValueCache {
  cache: HashMap<String, ValueRef>,
}

impl ValueCache {

  fn insert(&mut self, key: String, value: ValueRef) {
    self.cache.insert(key, value);
  }

  fn get(&self, key: &String) -> Option<ValueRef> {
    if let Some(x) = self.cache.get(key) {
      Some((*x).clone())
    } else {
      None
    }
  }

}

struct CacheStack {
  pub stack: Vec<ValueCache>,
}

impl CacheStack {

  fn new() -> Self {
    Self { stack: vec![], }
  }

  fn push(&mut self) {
    self.stack.push(ValueCache { cache: HashMap::new() });
  }

  fn insert(&mut self, key: String, value: ValueRef) {
    self.stack.last_mut().unwrap().insert(key, value);
  }

  fn get(&self, key: &String) -> Option<ValueRef> {
    for cache in self.stack.iter().rev() {
      if let Some(value) = cache.get(key) {
        return Some(value);
      }
    }
    None
  }

}

struct CodeGen {
  tg: typegen::TypeGen,
  cache_stack: CacheStack,
  loop_cond_or_end: Option<(ValueRef, ValueRef)>,
  pointer_cache: HashMap<usize, CGType>,
}
 
impl CodeGen {

  fn pop_scope(&mut self) {
    let mut ends = vec![];
    for (_, value) in self.cache_stack.stack.last().unwrap().cache.iter() {
      if let Some(inst) = value.as_ref::<Instruction>(&self.tg.builder.module.context) {
        if let Some(_) = inst.as_sub::<Alloca>() {
          ends.push(inst.as_super());
        }
      }
    }

    let block = self.tg.builder.get_current_block().unwrap();
    let block = block.as_ref::<Block>(&self.tg.builder.module.context).unwrap();
    let restore_insert_before = self.tg.builder.get_insert_before();

    if let None = self.tg.builder.get_insert_before() {
      if let Some(inst) = block.last_inst() {
        match inst.get_opcode() {
          InstOpcode::Branch(_) | InstOpcode::Return => {
            self.tg.builder.set_insert_before(inst.as_super());
          }
          _ => {}
        }
      }
    }

    for elem in ends {
      self.generate_lifetime_annot(elem, "end");
    }
    self.cache_stack.stack.pop();

    if let Some(inst) = restore_insert_before {
      self.tg.builder.set_insert_before(inst);
    } else {
      let block = self.tg.builder.get_current_block().unwrap();
      self.tg.builder.set_current_block(block);
    }
  }

  fn generate_linkage(&mut self, ast: &Rc<ast::Linkage>) {
    assert_eq!(ast.tus.len(), 2);
    self.cache_stack.push();
    let void_ty = self.tg.builder.context().void_type();
    let ptr_ty = self.tg.builder.context().pointer_type(void_ty.clone());
    let i64_ty = self.tg.builder.context().int_type(64);
    let fty = self.tg.builder.context().function_type(void_ty, vec![i64_ty, ptr_ty]);
    {
      let start = self.tg.builder.create_function(&"llvm.lifetime.start".to_string(), &fty);
      self.cache_stack.insert("llvm.lifetime.start".to_string(), start);
      let end = self.tg.builder.create_function(&"llvm.lifetime.end".to_string(), &fty);
      self.cache_stack.insert("llvm.lifetime.end".to_string(), end);
    }
    for tu in &ast.tus {
      self.generate_translation_unit(&tu);
    }
  }

  fn builder_mut(&mut self) -> &mut Builder {
    &mut self.tg.builder
  }

  fn generate_translation_unit(&mut self, tu: &Rc<ast::TranslateUnit>) {
    for decl in &tu.decls {
      if let ast::Decl::Func(func) = decl {
        let ret_ty = if func.id.literal == "malloc".to_string() {
          // If it is "malloc", return a raw pointer.
          let vty = self.tg.builder.context().void_type();
          self.tg.builder.context().pointer_type(vty)
        } else {
          // If not, respect the compiler.
          let cgty = self.tg.generate_type(&func.ty);
          cgty.to_llvm(self.builder_mut().context())
        };
        let args_ty :Vec<TypeRef> = func.args.iter().map(|arg| {
          let res = self.tg.generate_type(&arg.ty);
          res.to_llvm(self.builder_mut().context())
        }).collect();
        let fty = ret_ty.fn_type(self.tg.builder.context(), args_ty);
        let func_ref = self.tg.builder.create_function(&func.id.literal, &fty);
        self.cache_stack.insert(func.id.literal.clone(), func_ref.clone());
      }
    }
    for decl in tu.decls.iter() {
      if let ast::Decl::Func(func) = decl {
        if func.body.stmts.len() == 0 {
          continue;
        }
        let func_ref = self.cache_stack.get(&func.id.literal).unwrap();
        self.builder_mut().set_current_function(func_ref);
        let block_ref = self.builder_mut().create_block("entry".to_string());
        self.builder_mut().set_current_block(block_ref);
        self.generate_function(func);
      }
    }
  }

  fn generate_function(&mut self, func: &Rc<ast::FuncDecl>) {
    {
      self.cache_stack.push();
      let func_ref = self.cache_stack.get(&func.id.literal).unwrap();
      let args = {
        let llvm_func = func_ref.as_ref::<Function>(&self.tg.builder.module.context).unwrap();
        (0..llvm_func.get_num_args()).map(|i| {
          llvm_func.get_arg(i)
        }).collect::<Vec<_>>()
      };
      for (i, arg) in args.iter().enumerate() {
        let ty = self.tg.generate_type(&func.args[i].ty);
        self.declare_variable(ty, func.args[i].id.literal.clone(), Some(arg.clone()));
      }
    }
    // push arguments
    self.generate_compound_stmt(&func.body, false);
    self.pop_scope();
  }

  fn generate_lifetime_annot(&mut self, alloca: ValueRef, suffix: &str) {
    let intrin = self.cache_stack.get(&format!("llvm.lifetime.{}", suffix)).unwrap().clone();
    let i64_ty = self.tg.builder.context().int_type(64);
    let minus_one = self.tg.builder.context().const_value(i64_ty, u64::MAX);
    self.tg.builder.create_func_call(intrin, vec![minus_one, alloca.clone()]);
  }

  /// Return the pointer to the struct field.
  pub fn get_struct_field(
    &mut self,
    sty: TypeRef,
    value: ValueRef,
    idx: usize,
    name: &str) -> ValueRef {
    let attr_ty = self.tg.get_struct_field(sty.clone(), idx);
    let i32ty = self.builder_mut().context().int_type(32);
    let zero = self.builder_mut().context().const_value(i32ty.clone(), 0);
    let idx = self.builder_mut().context().const_value(i32ty, idx as u64);
    let res = self.builder_mut().create_gep(sty, value, vec![zero, idx], true, name.into());
    self.pointer_cache.insert(res.skey, CGType::Pointer(attr_ty.into()));
    res
  }

  fn declare_variable(&mut self, ty: CGType, id: String, init: Option<ValueRef>) -> ValueRef {
    // Save block and instruciton for restoring later
    let block = self.tg.builder.get_current_block().unwrap();
    let insert_before = self.tg.builder.get_insert_before();
    let func = self.tg.builder
      .get_current_function()
      .unwrap()
      .as_ref::<Function>(&self.tg.builder.module.context)
      .unwrap();
    let entry_block = func.get_block(0).unwrap();
    let entry_block = entry_block.as_super();
    // Insert to the 1st entry block.
    self.tg.builder.set_current_block(entry_block.clone());
    let entry_block = entry_block
      .as_ref::<Block>(&self.tg.builder.module.context)
      .unwrap();
    if let Some(first_inst) = entry_block.get_inst(0) {
      self.tg.builder.set_insert_before(first_inst.as_super());
    }
    let raw_ty = ty.to_llvm(self.builder_mut().context());
    let alloca = self.tg.builder.create_alloca(raw_ty.clone(), id.clone());
    // Restore block and instruction
    self.tg.builder.set_current_block(block);
    if let Some(insert_before) = insert_before {
      self.tg.builder.set_insert_before(insert_before);
    }
    self.generate_lifetime_annot(alloca.clone(), "start");
    if let Some(expr) = init {
      self.tg.builder.create_store(expr, alloca.clone()).unwrap();
    }
    self.pointer_cache.insert(alloca.skey, CGType::Pointer(ty.into()));
    self.cache_stack.insert(id.clone(), alloca.clone());
    alloca
  }

  fn generate_var_decl(&mut self, var: &Rc<ast::VarDecl>) {
    let ty = self.tg.generate_type(&var.ty);
    let id = var.id.literal.clone();
    let init = var.init.as_ref().map(|init| self.generate_expr(&init, false));
    self.declare_variable(ty, id, init);
  }

  fn generate_compound_stmt(&mut self, stmt: &Rc<ast::CompoundStmt>, new_scope: bool) {
    if new_scope {
      self.cache_stack.push();
    }
    for stmt in &stmt.stmts {
      self.generate_stmt(&stmt);
    }
    if new_scope {
      self.pop_scope();
    }
  }

  fn generate_return_stmt(&mut self, ret: &ReturnStmt) {
    let func = self.tg.builder.get_current_function().unwrap();
    let func = func.as_ref::<Function>(&self.tg.builder.module.context).unwrap();
    let func_ret_ty = func.get_ret_ty();
    let expr_ty = if let Some(expr) = &ret.value {
      let val = self.generate_expr(&expr, false);
      let expr_ty = val.get_type(&self.tg.builder.module.context);
      self.tg.builder.create_return(Some(val));
      expr_ty
    } else {
      self.tg.builder.create_return(None);
      self.tg.builder.module.context.void_type()
    };
    if expr_ty != func_ret_ty {
      let func = self.tg.builder.get_current_function().unwrap();
      let func = func.as_ref::<Function>(&self.tg.builder.module.context).unwrap();
      panic!("return type mismatch {} while function @{} returns {}",
        ret, func.get_name(), func_ret_ty.to_string(&self.tg.builder.module.context));
    }
  }

  fn generate_stmt(&mut self, stmt: &ast::Stmt) {
    match stmt {
      ast::Stmt::Ret(ret) => {
        self.generate_return_stmt(&ret);
      }
      ast::Stmt::Evaluate(expr) => {
        self.generate_expr(&expr, false);
      }
      ast::Stmt::VarDecl(decl) => {
        self.generate_var_decl(&decl);
      }
      ast::Stmt::InlineAsm(asm) => {
        self.generate_inline_asm(&asm);
      }
      ast::Stmt::ForStmt(for_loop) => {
        self.generate_for_stmt(&for_loop);
      }
      ast::Stmt::CompoundStmt(stmt) => {
        self.generate_compound_stmt(&stmt, true)
      }
      ast::Stmt::IfStmt(if_stmt) => {
        self.generate_if_stmt(&if_stmt);
      }
      ast::Stmt::WhileStmt(while_stmt) => {
        self.generate_while_stmt(&while_stmt);
      }
      ast::Stmt::LoopJump(jump) => {
        self.generate_loop_jump(&jump);
      }
    }
  }

  fn generate_loop_jump(&mut self, jump: &ast::LoopJump) {
    if let Some((cond, end)) = &self.loop_cond_or_end {
      match &jump.loc.value {
        TokenType::KeywordBreak => {
          self.tg.builder.create_unconditional_branch(end.clone(), BranchMetadata::None);
        }
        TokenType::KeywordContinue => {
          self.tg.builder.create_unconditional_branch(cond.clone(), BranchMetadata::None);
        }
        _ => {}
      }
    } else {
      panic!("Loop jump continue/break outside of loop");
    }
  }

  fn generate_if_stmt(&mut self, if_stmt: &IfStmt) {
    let cond = self.generate_expr(&if_stmt.cond, false);
    let then_block = self.builder_mut().create_block(format!("then.{}", cond.skey));
    let else_block = self.builder_mut().create_block(format!("else.{}", cond.skey));
    let converge = self.builder_mut().create_block(format!("converge.{}", cond.skey));
    self
      .builder_mut()
      .create_conditional_branch(cond, then_block.clone(), else_block.clone(), false);
    self.builder_mut().set_current_block(then_block.clone());
    self.generate_compound_stmt(&if_stmt.then_body, true);
    self.builder_mut().create_unconditional_branch(converge.clone(), BranchMetadata::None);
    self.builder_mut().set_current_block(else_block.clone());
    if let Some(else_body) = &if_stmt.else_body {
      self.generate_compound_stmt(&else_body, true);
    }
    self.builder_mut().create_unconditional_branch(converge.clone(), BranchMetadata::None);
    self.builder_mut().set_current_block(converge.clone());
  }

  fn generate_while_stmt(&mut self, while_stmt: &WhileStmt) {
    // Save the nested condition and end blocks.
    let old = self.loop_cond_or_end.clone();
    self.cache_stack.push();
    let prehead = self.builder_mut().create_block("while.prehead".to_string());
    let cond_block = self.builder_mut().create_block("while.cond".to_string());
    let body_block = self.builder_mut().create_block("while.body".to_string());
    let end_block = self.builder_mut().create_block("while.end".to_string());
    let cond = self.generate_expr(&while_stmt.cond, false);
    self.builder_mut().create_conditional_branch(cond, prehead.clone(), end_block.clone(), false);
    self.builder_mut().set_current_block(prehead);
    self.builder_mut().create_unconditional_branch(body_block.clone(), BranchMetadata::None);
    // Set it to the inner most loop.
    self.loop_cond_or_end = (cond_block.clone(), end_block.clone()).into();
    self.builder_mut().set_current_block(cond_block.clone());
    let cond = self.generate_expr(&while_stmt.cond, false);
    self.builder_mut().create_conditional_branch(cond, body_block.clone(), end_block.clone(), true);
    self.builder_mut().set_current_block(body_block);
    self.generate_compound_stmt(&while_stmt.body, false);
    self.builder_mut().create_unconditional_branch(cond_block.clone(), BranchMetadata::None);
    self.pop_scope();
    // Restore the nested condition and end blocks.
    self.loop_cond_or_end = old;
    self.builder_mut().set_current_block(end_block)
  }

  fn generate_for_stmt(&mut self, for_stmt: &ForStmt) {
    // Save the nested condition and end blocks.
    let old = self.loop_cond_or_end.clone();
    self.cache_stack.push();
    self.generate_var_decl(&for_stmt.var);
    let prehead = self.builder_mut().create_block("for.prehead".to_string());
    let body_block = self.builder_mut().create_block("for.body".to_string());
    let cond_block = self.builder_mut().create_block("for.cond".to_string());
    let end_block = self.builder_mut().create_block("for.end".to_string());
    // Set it to the inner most loop.
    self.loop_cond_or_end = (cond_block.clone(), end_block.clone()).into();
    let extent = self.generate_expr(&for_stmt.end, false);
    let i32ty = self.tg.builder.context().int_type(32);
    // If extent <= loop start, just skip the loop.
    let init = {
      let init = self.cache_stack.get(&for_stmt.var.id.literal).unwrap();
      let init = self.builder_mut().create_load(i32ty.clone(), init);
      init
    };
    let precond = self.builder_mut().create_sle(extent.clone(), init, "precond".to_string());
    self
      .builder_mut()
      .create_conditional_branch(precond, end_block.clone(), prehead.clone(), false);
    self.builder_mut().set_current_block(prehead);
    self.builder_mut().create_unconditional_branch(body_block.clone(), BranchMetadata::None);
    // Generate the loop conditions.
    self.builder_mut().set_current_block(cond_block.clone());
    let loop_var_addr = self.cache_stack.get(&for_stmt.var.id.literal).unwrap();
    let loop_var_value = self.builder_mut().create_load(i32ty.clone(), loop_var_addr.clone());
    let cond = self.builder_mut().create_slt(loop_var_value, extent, "cond".to_string());
    self.builder_mut().create_conditional_branch(cond, body_block.clone(), end_block.clone(), true);
    self.builder_mut().set_current_block(body_block);
    self.generate_compound_stmt(&for_stmt.body, false);
    let loop_var_value = self.builder_mut().create_load(i32ty.clone(), loop_var_addr.clone());
    let one = self.tg.builder.context().const_value(i32ty.clone(), 1);
    let added = self.builder_mut().create_add(loop_var_value, one);
    self.builder_mut().create_store(added, loop_var_addr).unwrap();
    self.builder_mut().create_unconditional_branch(cond_block.clone(), BranchMetadata::None);
    self.pop_scope();
    // Restore the nested condition and end blocks.
    self.loop_cond_or_end = old;
    self.builder_mut().set_current_block(end_block)
  }

  fn generate_expr(&mut self, expr: &ast::Expr, is_lval: bool) -> ValueRef {
    match expr {
      ast::Expr::FuncCall(call) => {
        self.generate_func_call(&call)
      }
      ast::Expr::IntImm(value) => {
        match value.token.value {
          TokenType::IntLiteral => {
            let i32ty = self.tg.builder.context().int_type(32);
            self.tg.builder.context().const_value(i32ty, value.value as u64)
          },
          TokenType::KeywordTrue => {
            let i32ty = self.tg.builder.context().int_type(1);
            self.tg.builder.context().const_value(i32ty, 1)
          }
          TokenType::KeywordFalse => {
            let i32ty = self.tg.builder.context().int_type(1);
            self.tg.builder.context().const_value(i32ty, 0)
          }
          _ => unreachable!()
        }
      }
      ast::Expr::StrImm(value) => {
        let i8ty = self.tg.builder.context().int_type(8);
        let ai8ty = self.tg.generate_array_runtime(i8ty.into(), 1);
        let str_value = self.tg.builder.create_string(value.value.clone());
        let len = value.value.len();
        let i32ty = self.tg.builder.context().int_type(32);
        let len = self.tg.builder.context().const_value(i32ty.clone(), len as u64);
        let raw_ai8ty = ai8ty.get_pointee_ty().unwrap();
        let i8array_obj = self.tg.builder.create_global_struct(raw_ai8ty, vec![len, str_value]);
        let string_ty = self.tg.get_class(&"string".into()).unwrap();
        // let str_ptr = self.tg.builder.get_struct_field(i8array, 0).unwrap();
        self.tg.builder.create_global_struct(string_ty, vec![i8array_obj])
      }
      ast::Expr::Variable(var) => {
        let value = self.cache_stack.get(&var.id());
        let value = if let Some(value) = value {
          value.clone()
        } else {
          let ptr_addr = &self.cache_stack.stack.last() as *const _ as usize;
          panic!("{} is not defined! 0x{:x}", var.id(), ptr_addr);
        };
        if is_lval {
          value
        } else {
          let ty = self.pointer_cache.get(&value.skey).unwrap().clone();
          let llvm_ty = ty.get_pointee_ty().to_llvm(self.builder_mut().context());
          let kind = llvm_ty.kind().clone();
          // TODO(@were): Check if it is a pointer.
          let res = self.tg.builder.create_load(llvm_ty, value);
          if kind == TKindCode::PointerType {
            self.pointer_cache.insert(res.skey, ty.get_pointee_ty());
          }
          res
        }
        // panic!("{} is not a pointer!", value.print_to_string().to_string());
      }
      ast::Expr::AttrAccess(aa) => {
        // "This" is expected to be a pointer to a struct.
        let this = self.generate_expr(&aa.this, false);
        eprintln!("{}", aa.this);
        let sty = self.pointer_cache.get(&this.skey).unwrap();
        dbg!(sty.clone());
        let sty = sty.get_pointee_ty().unwrap();
        dbg!(sty.clone());
        assert!(sty.kind() == &TKindCode::StructType);
        // Get the pointer's underlying struct type.
        let res = self.get_struct_field(sty.clone(), this, aa.idx, "");
        let attr_ty = self.tg.get_struct_field(sty, aa.idx);
        if is_lval {
          self.pointer_cache.insert(res.skey, CGType::Pointer(attr_ty.into()));
          res
        } else {
          let llvm_ty = attr_ty.to_llvm(self.builder_mut().context());
          let res = self.tg.builder.create_load(llvm_ty, res.clone());
          self.pointer_cache.insert(res.skey, attr_ty.clone());
          res
        }
      }
      ast::Expr::BinaryOp(binop) => {
        let is_lval = binop.op.value == TokenType::AssignEq;
        let lhs = self.generate_expr(&binop.lhs, is_lval);
        let logic = match &binop.op.value {
          TokenType::LogicAnd | TokenType::LogicOr => {
            let func = self.tg.builder
              .get_current_function().unwrap()
              .as_ref::<Function>(&self.tg.builder.module.context).unwrap();
            // Restore the current block.
            let current = self.tg.builder.get_current_block().unwrap();
            // Create a local variable for the result.
            let entry = func.get_block(0).unwrap().as_super();
            self.tg.builder.set_current_block(entry.clone());
            {
              let entry = entry.as_ref::<Block>(&self.tg.builder.module.context).unwrap();
              if let Some(first_inst) = entry.get_inst(0) {
                self.tg.builder.set_insert_before(first_inst.as_super());
              }
            }
            let i1ty = self.tg.builder.context().int_type(1);
            let alloca = self.declare_variable(i1ty.clone().into(), "sc.result".into(), None);
            // Prepare the values
            let one = self.tg.builder.context().const_value(i1ty.clone(), 1);
            let zero = self.tg.builder.context().const_value(i1ty.clone(), 0);
            // Create two blocks for the short-circuit evaluation.
            let true_block = self.tg.builder.create_block(format!("sc.true.{}", alloca.skey));
            let false_block = self.tg.builder.create_block(format!("sc.false.{}", alloca.skey));
            self.tg.builder.set_current_block(current);
            self.generate_lifetime_annot(alloca.clone(), "start");
            self.tg.builder.create_conditional_branch(lhs.clone(), true_block.clone(), false_block.clone(), false);
            let (finalize, finalized_value, compute) = if let &TokenType::LogicAnd = &binop.op.value {
              (false_block, zero, true_block)
            } else {
              (true_block, one, false_block)
            };
            self.tg.builder.set_current_block(compute);
            let rhs = self.generate_expr(&binop.rhs, false);
            self.tg.builder.create_store(rhs, alloca.clone()).unwrap();
            let converge = self.tg.builder.create_block(format!("sc.converge.{}", alloca.skey));
            self.tg.builder.create_unconditional_branch(converge.clone(), BranchMetadata::None);
            // Create the false block.
            self.tg.builder.set_current_block(finalize);
            self.tg.builder.create_store(finalized_value, alloca.clone()).unwrap();
            self.tg.builder.create_unconditional_branch(converge.clone(), BranchMetadata::None);
            // Create the converge block.
            self.tg.builder.set_current_block(converge);
            let res = Some(self.tg.builder.create_load(i1ty.clone(), alloca.clone()));
            self.generate_lifetime_annot(alloca, "end");
            res
          }
          _ => None
        };
        if let Some(res) = logic {
          res
        } else {
          let rhs = self.generate_expr(&binop.rhs, false);
          match &binop.op.value {
            TokenType::Add => {
              self.tg.builder.create_add(lhs, rhs)
            }
            TokenType::Sub => {
              self.tg.builder.create_sub(lhs, rhs)
            }
            TokenType::Mul => {
              self.tg.builder.create_mul(lhs, rhs)
            }
            TokenType::Mod => {
              self.tg.builder.create_srem(lhs, rhs)
            }
            TokenType::Div => {
              self.tg.builder.create_sdiv(lhs, rhs)
            }
            TokenType::AssignEq => {
              match self.tg.builder.create_store(rhs.clone(), lhs.clone()) {
                Ok(res) => { res }
                Err(msg) => {
                  let lhs = lhs.to_string(&self.tg.builder.module.context, true);
                  let rhs = rhs.to_string(&self.tg.builder.module.context, true);
                  eprintln!("Failed to store:\n{}", lhs);
                  eprintln!("Failed to store:\n{}", rhs);
                  panic!("Failed to cg:\n{}, msg: {}", expr, msg);
                }
              }
            }
            TokenType::LT => self.tg.builder.create_slt(lhs, rhs, "lt".to_string()),
            TokenType::LE => self.tg.builder.create_sle(lhs, rhs, "le".to_string()),
            TokenType::GT => self.tg.builder.create_sgt(lhs, rhs, "gt".to_string()),
            TokenType::GE => self.tg.builder.create_sge(lhs, rhs, "ge".to_string()),
            TokenType::EQ => self.tg.builder.create_eq(lhs, rhs, "eq".to_string()),
            TokenType::NE => self.tg.builder.create_ne(lhs, rhs, "ne".to_string()),
            TokenType::BitwiseAnd => self.tg.builder.create_and(lhs, rhs),
            TokenType::BitwiseOr => self.tg.builder.create_or(lhs, rhs),
            TokenType::BitwiseXor => self.tg.builder.create_xor(lhs, rhs),
            TokenType::BitwiseShl => self.tg.builder.create_shl(lhs, rhs),
            TokenType::BitwiseShr => self.tg.builder.create_ashr(lhs, rhs),
            _ => { panic!("Unknown binary op {}", binop.op); }
          }
        }
      }
      ast::Expr::UnaryOp(unary) => {
        let expr = self.generate_expr(&unary.expr, false);
        let res = match &unary.op.value {
          TokenType::Sub => {
            let i32ty = self.tg.builder.context().int_type(32);
            let zero = self.tg.builder.context().const_value(i32ty, 0);
            self.tg.builder.create_sub(zero, expr)
          }
          TokenType::LogicNot => {
            let i1ty = self.tg.builder.context().int_type(1);
            let one = self.tg.builder.context().const_value(i1ty, 1);
            self.tg.builder.create_xor(one, expr)
          }
          _ => { panic!("Unknown unary op {}", unary.op); }
        };
        res
      }
      ast::Expr::NewExpr(ne) => {
        let malloc = self.cache_stack.get(&"malloc".to_string()).unwrap().clone();
        let i32ty = self.tg.builder.context().int_type(32);
        let ty = self.tg.generate_type(&ne.dtype);
        let size = {
          let ty = ty.to_llvm(self.builder_mut().context());
          ty.get_scalar_size_in_bits(&self.tg.builder.module) / 8
        };
        let size = self.tg.builder.context().const_value(i32ty.clone(), size as u64);
        let obj = self.tg.builder.create_func_call(malloc.clone(), vec![size]);
        self.pointer_cache.insert(obj.skey, ty.clone());
        // eprintln!("mark {} as {}", obj.to_string(&self.tg.builder.module.context, true),
        //   ty.to_string(&self.tg.builder.module.context));

        // If it is an array, we set the length of the array first.
        // Then we allocate the array buffer.
        if let ast::Type::Array(array) = &ne.dtype {
          let array_ty = ty.clone();
          let raw_aty = array_ty.get_pointee_ty().unwrap();
          // Array length.
          let array_len = self.generate_expr(&array.dims[0], false);
          // Write array length to object's first field.
          let len_ptr = self.get_struct_field(
            raw_aty.clone(),
            obj.clone(), 0, "array.length");
          self.tg.builder.create_store(array_len.clone(), len_ptr).unwrap();
          // Array size.
          let elem_ty = self.tg
            .get_struct_field(raw_aty.clone(), 1)
            .to_llvm(self.builder_mut().context());
          // If it is an array of struct, we only need to allocate the pointer for these
          // structs.
          let obj_size = elem_ty.get_scalar_size_in_bits(&self.tg.builder.module) / 8;
          let obj_size = self.tg.builder.context().const_value(i32ty.clone(), obj_size as u64);
          // Allocate the array buffer.
          let bytes = self.tg.builder.create_mul(array_len, obj_size);
          let payload = self.tg.builder.create_func_call(malloc, vec![bytes]);
          // Write array buffer to object's second field.
          let payload_ptr = self.get_struct_field(raw_aty, obj.clone(), 1, "array.payload");
          self.tg.builder.create_store(payload, payload_ptr).unwrap();
        }
        obj
      }
      ast::Expr::ArrayIndex(array_idx) => {
        let mut array_obj = self.generate_expr(&array_idx.array, false);
        eprintln!("array: {}", array_idx.array);
        eprintln!("array obj: {}", array_obj.to_string(&self.tg.builder.module.context, true));
        let indices = array_idx
          .indices
          .iter()
          .map(|x| self.generate_expr(x, false))
          .collect::<Vec<_>>();
        let mut carried_ty = self.pointer_cache.get(&array_obj.skey).unwrap().clone();
        // for (i, elem) in indices.iter().enumerate() {
        //   eprintln!("idx_{}: {}", i, elem.to_string(&self.tg.builder.module.context, true));
        // }
        for (i, idx) in indices.iter().enumerate() {
          // array_struct = struct { length=i32, payload=ptr }
          // ptr is the pointer to the array, and the type is scalar_ty*.
          let array_struct = carried_ty.get_pointee_ty().unwrap();
          dbg!(array_struct.as_ref::<StructType>(&self.tg.builder.module.context).unwrap().to_string());
          let field_ty = self.tg.get_struct_field(array_struct.clone(), 1); // scalar*
          dbg!(field_ty.clone());
          let scalar_ty = field_ty.get_pointee_ty(); // scalar
          dbg!(scalar_ty.clone());
          // array_struct.payload is scalar_ty*, but the GEP instruction returns
          // scalar_ty** for further load this field.
          let payload_ptr = self.get_struct_field(array_struct, array_obj, 1, "array.payload");
          // After LLVM-15, they deprecated typed pointers.
          // We need to keep this information in our codegen's mind,
          // i.e. the ``pointer_cache'' HashMap.
          self.pointer_cache.insert(payload_ptr.skey, CGType::Pointer(field_ty.clone().into()));
          // TODO(@were): Deprecate this later. We no longer care about the pointee type.
          let vty = self.tg.builder.context().void_type();
          let pty = self.tg.builder.context().pointer_type(vty);
          // Load scalar_ty* from scalar_ty**, but it is still a pointer type.
          let payload = self.tg.builder.create_load(pty, payload_ptr);
          // Record this load is scalar_ty* in our data structure.
          self.pointer_cache.insert(payload.skey, field_ty.clone());
          // &payload[idx]
          // gep scalar_ty, ptr payload, i
          array_obj = {
            let scalar_ty = scalar_ty.to_llvm(self.builder_mut().context());
            self.tg.builder.create_gep(
              scalar_ty, payload, vec![idx.clone()], true, "a.i".into())
          };
          // In case this scalar is still a pointer, record this.
          self.pointer_cache.insert(array_obj.skey, scalar_ty.clone());
          // If we have further indices, we need to use it as a right-value.
          // Dereference the address.
          if i != indices.len() - 1 {
            dbg!(scalar_ty.clone());
            let llvm_ty = scalar_ty.to_llvm(self.builder_mut().context());
            array_obj = self.tg.builder.create_load(llvm_ty, array_obj);
            if let CGType::Pointer(ty) = &scalar_ty {
              self.pointer_cache.insert(array_obj.skey, ty.as_ref().clone());
            }
            // eprintln!("load array[i]: {}",
            //   array_obj.as_ref::<Instruction>(&self.tg.builder.module.context)
            //     .unwrap().to_string(false));
          }
          carried_ty = scalar_ty;
        }
        if !is_lval {
          let ty = carried_ty.to_llvm(self.builder_mut().context());
          let res = self.tg.builder.create_load(ty, array_obj);
          // eprintln!("rval array[i]: {}",
          //   res.as_ref::<Instruction>(&self.tg.builder.module.context).unwrap().to_string(false));
          res
        } else {
          array_obj
        }
      }
      ast::Expr::Cast(cast) => {
        let value = self.generate_expr(&cast.expr, false);
        let ty = self.tg.generate_type(&cast.dtype);
        let ty = ty.to_llvm(self.builder_mut().context());
        self.tg.builder.create_cast(value, ty)
      }
      _ => {
        eprintln!("Not supported node (use 0 as a placeholder):\n{}", expr);
        let i32ty = self.tg.builder.context().int_type(32);
        self.tg.builder.context().const_value(i32ty, 0)
      }
      // _ => { panic!("Unknown expr {}", expr); }
    }
  }

  fn generate_inline_asm(&mut self, asm: &ast::InlineAsm) -> ValueRef {
    let params : Vec<ValueRef> = asm.args.iter().map(|arg| {
      self.generate_expr(&arg, false).into()
    }).collect();
    let vty = self.tg.builder.context().void_type();
    let code = asm.code.value.clone();
    let operands = asm.operands.value.clone();
    let callee = self.tg.builder.create_inline_asm(vty.clone(), code, operands, true);
    self.tg.builder.create_typed_call(vty.clone(), callee, params)
  }

  fn generate_func_call(&mut self, call: &Rc<ast::FuncCall>) -> ValueRef {
    let params : Vec<ValueRef> = call.params.iter().map(|arg| {
      let expr = self.generate_expr(&arg, false);
      expr
    }).collect();

    // Manually inline array::length.
    if call.fname.literal == "array::length".to_string() {
      let array = params.get(0).unwrap().clone();
      let pointee_ty = self.pointer_cache.get(&array.skey).unwrap().clone();
      let pointee_ty = pointee_ty.get_pointee_ty().unwrap();
      let len_ptr = self.get_struct_field(pointee_ty, array, 0, "array.length");
      let i32ty = self.tg.builder.context().int_type(32);
      return self.tg.builder.create_load(i32ty, len_ptr);
    }

    let callee = self.cache_stack.get(&call.fname.literal).unwrap().clone();
    self.tg.builder.create_func_call(callee, params)
  }

}

pub fn codegen(ast: &Rc<ast::Linkage>, tt: String, layout: String) -> ir::module::Module {
  let fname = ast.tus[ast.tus.len() - 1].fname.clone();
  let module = Module::new(fname.clone(), fname.clone(), tt, layout);

  let mut tg = TypeGen::new(ast, module);
  tg.enter_linkage(ast);

  let mut cg = CodeGen{
    tg,
    cache_stack: CacheStack::new(),
    loop_cond_or_end: None,
    pointer_cache: HashMap::new()
  };

  cg.generate_linkage(ast);

  return cg.tg.builder.module;
}

