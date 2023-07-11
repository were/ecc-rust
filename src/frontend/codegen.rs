use std::rc::Rc;
use std::collections::HashMap;

use trinity::{ir::{
  self,
  value::ValueRef,
  types::{StructType, TypeRef},
  value::Function, PointerType, Block
}, context::{Reference, component::GetSlabKey}};
use trinity::builder::Builder;
use super::ast::{self, ForStmt, WhileStmt, IfStmt, ReturnStmt};

struct TypeGen {
  pub builder: Builder,
  pub class_cache: HashMap<String, TypeRef>,
}

impl TypeGen {

  fn enter_linkage(&mut self, ast: &Rc<ast::Linkage>) {
    assert_eq!(ast.tus.len(), 2);
    for tu in &ast.tus {
      self.generate_translation_unit(&tu);
    }
  }

  fn generate_translation_unit(&mut self, tu: &Rc<ast::TranslateUnit>) {
    for decl in &tu.decls {
      match decl {
        ast::Decl::Class(class) => {
          self.class_to_struct(&class);
        }
        _ => {}
      }
    }
  }

  fn type_to_llvm(&mut self, ty: &ast::Type) -> ir::types::TypeRef {
    match ty {
      ast::Type::Class(class) => {
        let res = self.class_cache.get(&class.id.literal).unwrap();
        return res.ptr_type(self.builder.context());
      }
      ast::Type::Builtin(builtin) => self.builtin_to_llvm(builtin.as_ref()),
      ast::Type::Array(array) => self.array_to_llvm(array),
    }
  }

  fn class_to_struct(&mut self, class: &Rc<ast::ClassDecl>) {
    let attrs : Vec<ir::types::TypeRef> = class.attrs.iter().map(
      |attr| { self.type_to_llvm(&attr.ty) }).collect();
    if let Some(sty) = self.class_cache.get(&class.id.literal) {
      let sty_mut = sty.as_mut::<StructType>(self.builder.context()).unwrap();
      sty_mut.set_body(attrs);
    }
  }

  fn builtin_to_llvm(&mut self, builtin : &ast::BuiltinType) -> ir::types::TypeRef {
    match builtin.code {
      ast::BuiltinTypeCode::Int => self.builder.context().int_type(32),
      ast::BuiltinTypeCode::Char => self.builder.context().int_type(8),
      ast::BuiltinTypeCode::Void => self.builder.context().void_type(),
      ast::BuiltinTypeCode::Bool => self.builder.context().int_type(1),
      _ => { panic!("Unknown builtin type {}", builtin.token.literal); }
    }
  }

  fn array_to_llvm(&mut self, array: &ast::ArrayType) -> ir::types::TypeRef {
    let mut res = self.type_to_llvm(&array.scalar_ty);
    for _ in 0..array.dims.len() {
      res = res.ptr_type(self.builder.context());
    }
    return res
  }

}

struct ValueCache {
  pub cache: HashMap<String, ValueRef>,
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

  fn push(&mut self) {
    self.stack.push(ValueCache { cache: HashMap::new() });
  }

  fn insert(&mut self, key: String, value: ValueRef) {
    self.stack.last_mut().unwrap().insert(key, value);
  }

  fn pop(&mut self) -> ValueCache {
    self.stack.pop().unwrap()
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
  tg: TypeGen,
  cache_stack: CacheStack,
  loop_cond_and_end: Option<(ValueRef, ValueRef)>,
}
 
impl CodeGen {

  fn generate_linkage(&mut self, ast: &Rc<ast::Linkage>) {
    assert_eq!(ast.tus.len(), 2);
    self.cache_stack.push();
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
        let ret_ty = self.tg.type_to_llvm(&func.ty);
        let args_ty :Vec<TypeRef> = func.args.iter().map(|arg| {
          let res = self.tg.type_to_llvm(&arg.ty);
          if *res.kind() == ir::types::TKindCode::StructType {
            res.ptr_type(self.tg.builder.context())
          } else {
            res
          }
        }).collect();
        let fty = ret_ty.fn_type(self.tg.builder.context(), args_ty);
        let func_ref = self.tg.builder.create_function(func.id.literal.clone(), fty);
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
        let block_ref = self.builder_mut().add_block("entry".to_string());
        self.builder_mut().set_current_block(block_ref);
        self.generate_func(func);
      }
    }
  }

  fn generate_func(&mut self, func: &Rc<ast::FuncDecl>) {
    {
      self.cache_stack.push();
      let func_ref = self.cache_stack.get(&func.id.literal).unwrap();
      let args = {
        let llvm_func = func_ref.as_ref::<Function>(&self.tg.builder.module.context).unwrap();
        let llvm_func = Reference::new(llvm_func.get_skey(), &self.tg.builder.module.context, llvm_func);
        (0..llvm_func.get_num_args()).map(|i| {
          llvm_func.get_arg(i)
        }).collect::<Vec<ValueRef>>()
      };
      for (i, arg) in args.iter().enumerate() {
        let ty = arg.get_type(self.tg.builder.context());
        let ptr = self.builder_mut().create_alloca(ty);
        self.builder_mut().create_store(arg.clone(), ptr.clone()).unwrap();
        self.cache_stack.insert(
          func.args[i].id.literal.clone(),
          ptr
        );
      }
    }
    // push arguments
    self.generate_compound_stmt(&func.body, false);
    self.cache_stack.pop();
  }

  fn generate_var_decl(&mut self, var: &Rc<ast::VarDecl>) {
    let ty = self.tg.type_to_llvm(&var.ty);
    // Save block and instruciton for restoring later
    let block = self.tg.builder.get_current_block().unwrap();
    let insert_before = self.tg.builder.get_insert_before();
    let func = self.tg.builder
      .get_current_function()
      .unwrap()
      .as_ref::<Function>(&self.tg.builder.module.context)
      .unwrap();
    let func = Reference::new(func.get_skey(), &self.tg.builder.module.context, func);
    let entry_block = func.get_block(0).unwrap();
    let entry_block = Block::from_skey(entry_block.get_skey());
    // Insert to the 1st entry block.
    self.tg.builder.set_current_block(entry_block.clone());
    if let Some(first_inst) = entry_block
      .as_ref::<Block>(&self.tg.builder.module.context)
      .unwrap()
      .get_inst(0) {
      self.tg.builder.set_insert_before(first_inst);
    }
    let alloca = self.tg.builder.create_alloca(ty);
    // Restore block and instruction
    self.tg.builder.set_current_block(block);
    if let Some(insert_before) = insert_before {
      self.tg.builder.set_insert_before(insert_before);
    }
    if let Some(init) = &var.init {
      let init = self.generate_expr(&init, false);
      self.tg.builder.create_store(init, alloca.clone()).unwrap();
    }
    self.cache_stack.insert(var.id().clone(), alloca);
  }

  fn generate_compound_stmt(&mut self, stmt: &Rc<ast::CompoundStmt>, new_scope: bool) {
    if new_scope {
      self.cache_stack.push();
    }
    for stmt in &stmt.stmts {
      self.generate_stmt(&stmt);
    }
    if new_scope {
      self.cache_stack.pop();
    }
  }

  fn generate_return_stmt(&mut self, ret: &ReturnStmt) {
    let func = self.tg.builder.get_current_function().unwrap();
    let func = func.as_ref::<Function>(&self.tg.builder.module.context).unwrap();
    let func = Reference::new(func.get_skey(), &self.tg.builder.module.context, func);
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
    assert!(expr_ty == func_ret_ty);
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
    if let Some((cond, end)) = &self.loop_cond_and_end {
      match &jump.loc.value {
        super::lexer::TokenType::KeywordBreak => {
          self.tg.builder.create_unconditional_branch(end.clone());
        }
        super::lexer::TokenType::KeywordContinue => {
          self.tg.builder.create_unconditional_branch(cond.clone());
        }
        _ => {}
      }
    } else {
      panic!("Loop jump continue/break outside of loop");
    }
  }

  fn generate_if_stmt(&mut self, if_stmt: &IfStmt) {
    let cond = self.generate_expr(&if_stmt.cond, false);
    let then_block = self.builder_mut().add_block(format!("then.{}", cond.skey));
    let else_block = self.builder_mut().add_block(format!("else.{}", cond.skey));
    let converge = self.builder_mut().add_block(format!("converge.{}", cond.skey));
    self.builder_mut().create_conditional_branch(cond, then_block.clone(), else_block.clone());
    self.builder_mut().set_current_block(then_block.clone());
    self.generate_compound_stmt(&if_stmt.then_body, true);
    self.builder_mut().create_unconditional_branch(converge.clone());
    self.builder_mut().set_current_block(else_block.clone());
    if let Some(else_body) = &if_stmt.else_body {
      self.generate_compound_stmt(&else_body, true);
    }
    self.builder_mut().create_unconditional_branch(converge.clone());
    self.builder_mut().set_current_block(converge.clone());
  }

  fn generate_while_stmt(&mut self, while_stmt: &WhileStmt) {
    // Save the nested condition and end blocks.
    let old = self.loop_cond_and_end.clone();
    self.cache_stack.push();
    let cond_block = self.builder_mut().add_block("while.cond".to_string());
    let body_block = self.builder_mut().add_block("while.body".to_string());
    let end_block = self.builder_mut().add_block("while.end".to_string());
    // Set it to the inner most loop.
    self.loop_cond_and_end = (cond_block.clone(), end_block.clone()).into();
    self.builder_mut().create_unconditional_branch(cond_block.clone());
    self.builder_mut().set_current_block(cond_block.clone());
    let cond = self.generate_expr(&while_stmt.cond, false);
    self.builder_mut().create_conditional_branch(cond, body_block.clone(), end_block.clone());
    self.builder_mut().set_current_block(body_block);
    self.generate_compound_stmt(&while_stmt.body, false);
    self.builder_mut().create_unconditional_branch(cond_block.clone());
    self.cache_stack.pop();
    // Restore the nested condition and end blocks.
    self.loop_cond_and_end = old;
    self.builder_mut().set_current_block(end_block)
  }

  fn generate_for_stmt(&mut self, for_stmt: &ForStmt) {
    // Save the nested condition and end blocks.
    let old = self.loop_cond_and_end.clone();
    self.cache_stack.push();
    self.generate_var_decl(&for_stmt.var);
    let cond_block = self.builder_mut().add_block("for.cond".to_string());
    let body_block = self.builder_mut().add_block("for.body".to_string());
    let end_block = self.builder_mut().add_block("for.end".to_string());
    // Set it to the inner most loop.
    self.loop_cond_and_end = (cond_block.clone(), end_block.clone()).into();
    let extent = self.generate_expr(&for_stmt.end, false);
    self.builder_mut().create_unconditional_branch(cond_block.clone());
    self.builder_mut().set_current_block(cond_block.clone());
    let loop_var_addr = self.cache_stack.get(&for_stmt.var.id.literal).unwrap();
    let loop_var_value = self.builder_mut().create_load(loop_var_addr.clone());
    let cond = self.builder_mut().create_slt(loop_var_value, extent);
    self.builder_mut().create_conditional_branch(cond, body_block.clone(), end_block.clone());
    self.builder_mut().set_current_block(body_block);
    self.generate_compound_stmt(&for_stmt.body, false);
    let loop_var_value = self.builder_mut().create_load(loop_var_addr.clone());
    let i32ty = self.tg.builder.context().int_type(32);
    let one = self.tg.builder.context().const_value(i32ty.clone(), 1);
    let added = self.builder_mut().create_add(loop_var_value, one);
    self.builder_mut().create_store(added, loop_var_addr).unwrap();
    self.builder_mut().create_unconditional_branch(cond_block.clone());
    self.cache_stack.pop();
    // Restore the nested condition and end blocks.
    self.loop_cond_and_end = old;
    self.builder_mut().set_current_block(end_block)
  }

  fn generate_expr(&mut self, expr: &ast::Expr, is_lval: bool) -> ValueRef {
    match expr {
      ast::Expr::FuncCall(call) => {
        self.generate_func_call(&call)
      }
      ast::Expr::IntImm(value) => {
        let i32ty = self.tg.builder.context().int_type(32);
        self.tg.builder.context().const_value(i32ty, value.value as u64)
      }
      ast::Expr::StrImm(value) => {
        let len = value.value.len();
        let str_value = self.tg.builder.create_string(value.value.clone());
        let str_ref = self.tg.class_cache.get("string").unwrap().clone();
        let i32ty = self.tg.builder.context().int_type(32);
        let zero = self.tg.builder.context().const_value(i32ty.clone(), 0);
        let str_len = self.tg.builder.context().const_value(i32ty.clone(), len as u64);
        let i8ty = self.tg.builder.context().int_type(8);
        let i8ptr = i8ty.ptr_type(self.tg.builder.context());
        let str_ptr = self.tg.builder.create_gep(i8ptr, str_value, vec![zero.clone(), zero.clone()], true);
        self.tg.builder.create_global_struct(str_ref, vec![str_len, str_ptr])
      }
      ast::Expr::Variable(var) => {
        let value = self.cache_stack.get(&var.id());
        let value = if let Some(value) = value {
          value.clone()
        } else {
          panic!("{} is not defined! 0x{:x}", var.id(), &self.cache_stack.stack.last() as *const _ as usize);
        };
        if is_lval {
          value
        } else {
          // TODO(@were): Check if it is a pointer.
          self.tg.builder.create_load(value)
        }
        // panic!("{} is not a pointer!", value.print_to_string().to_string());
      }
      ast::Expr::AttrAccess(aa) => {
        // "This" is expected to be a pointer to a struct.
        let this = self.generate_expr(&aa.this, false);
        // Get the pointer's underlying struct type.
        let ptr_ty_ref = this.get_type(self.tg.builder.context());
        let ptr_ty = ptr_ty_ref.as_ref::<PointerType>(self.tg.builder.context()).unwrap();
        let sty_ref = ptr_ty.get_pointee_ty();
        let sty = sty_ref.as_ref::<StructType>(self.tg.builder.context()).unwrap();
        // Get the struct type for the corresponding struct attr.
        let attr_ty = sty.get_attr(aa.idx);
        let attr_ptr = attr_ty.ptr_type(self.tg.builder.context());
        // Compute the pointer.
        let i32ty = self.tg.builder.context().int_type(32);
        let zero = self.tg.builder.context().const_value(i32ty.clone(), 0);
        let idx = self.tg.builder.context().const_value(i32ty.clone(), aa.idx as u64);
        let res = self.tg.builder.create_gep(attr_ptr, this, vec![zero, idx], true);
        if is_lval {
          res
        } else {
          self.tg.builder.create_load(res)
        }
      }
      ast::Expr::BinaryOp(binop) => {
        let is_lval = binop.op.value == super::lexer::TokenType::AssignEq;
        let lhs = self.generate_expr(&binop.lhs, is_lval);
        let rhs = self.generate_expr(&binop.rhs, false);
        match &binop.op.value {
          super::lexer::TokenType::Add => {
            self.tg.builder.create_add(lhs, rhs)
          }
          super::lexer::TokenType::Sub => {
            self.tg.builder.create_sub(lhs, rhs)
          }
          super::lexer::TokenType::Mul => {
            self.tg.builder.create_mul(lhs, rhs)
          }
          super::lexer::TokenType::Mod => {
            self.tg.builder.create_srem(lhs, rhs)
          }
          super::lexer::TokenType::Div => {
            self.tg.builder.create_sdiv(lhs, rhs)
          }
          super::lexer::TokenType::AssignEq => {
            self.tg.builder.create_store(rhs, lhs).unwrap()
          }
          super::lexer::TokenType::LT => self.tg.builder.create_slt(lhs, rhs),
          super::lexer::TokenType::GT => self.tg.builder.create_sgt(lhs, rhs),
          super::lexer::TokenType::EQ => self.tg.builder.create_eq(lhs, rhs),
          _ => { panic!("Unknown binary op {}", binop.op); }
        }
      }
      ast::Expr::NewExpr(ne) => {
        let malloc = self.cache_stack.get(&"malloc".to_string()).unwrap().clone();
        let i32ty = self.tg.builder.context().int_type(32);
        // TODO(@were): Support the size of malloc.
        let dest = self.tg.type_to_llvm(&ne.dtype);
        let params = vec![self.tg.builder.context().const_value(i32ty, 8)];
        let call = self.tg.builder.create_func_call(malloc, params);
        self.tg.builder.create_bitcast(call, dest)
      }
      ast::Expr::ArrayIndex(idx) => {
        let array = self.generate_expr(&idx.array, false);
        let indices = idx.indices.iter().map(|x| self.generate_expr(x, false)).collect::<Vec<_>>();
        let array_ty = array.get_type(self.tg.builder.context());
        // let ptr_ref = array_ty.as_ref::<PointerType>(self.tg.builder.context()).unwrap();
        // let elem_ty = ptr_ref.get_pointee_ty();
        self.tg.builder.create_gep(array_ty, array, indices, true)
      }
      ast::Expr::Cast(cast) => {
        let value = self.generate_expr(&cast.expr, false);
        let ty = self.tg.type_to_llvm(&cast.dtype);
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
    let callee = self.tg.builder.create_inline_asm(vty.clone(), asm.code.value.clone(), asm.operands.value.clone(), true);
    self.tg.builder.create_typed_call(vty.clone(), callee, params)
  }

  fn generate_func_call(&mut self, call: &Rc<ast::FuncCall>) -> ValueRef {
    let params : Vec<ValueRef> = call.params.iter().map(|arg| {
      let expr = self.generate_expr(&arg, false);
      expr
    }).collect();
    let callee = self.cache_stack.get(&call.fname.literal).unwrap().clone();
    self.tg.builder.create_func_call(callee, params)
  }

}

pub fn codegen(ast: &Rc<ast::Linkage>, tt: String, layout: String) -> ir::module::Module {
  let fname = ast.tus[ast.tus.len() - 1].fname.clone();
  let module = ir::module::Module::new(fname.clone(), fname.clone(), tt, layout);
  let mut builder = Builder::new(module);
  let mut class_cache: HashMap<String, TypeRef> = HashMap::new();

  // Declare all classes for potential cross references.
  for tu in ast.tus.iter() {
    for decl in &tu.decls {
      if let ast::Decl::Class(class) = decl {
        let sty_ref = builder.create_struct(class.id().clone());
        class_cache.insert(class.id().clone(), sty_ref);
      }
    }
  }

  let mut tg = TypeGen{ builder, class_cache };
  tg.enter_linkage(ast);

  let mut cg = CodeGen{
    tg,
    cache_stack: CacheStack {
      stack: Vec::new(),
    },
    loop_cond_and_end: None
  };

  cg.generate_linkage(ast);

  return cg.tg.builder.module;
}

