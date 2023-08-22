use std::rc::Rc;
use std::collections::HashMap;

use trinity::ir::{
  self,
  value::{ValueRef, instruction::InstOpcode},
  types::{StructType, TypeRef},
  value::{Function, instruction::Alloca}, PointerType, Block, TKindCode, Instruction,
};
use trinity::builder::Builder;
use super::{
  ast::{self, ForStmt, WhileStmt, IfStmt, ReturnStmt},
  lexer::TokenType
};

struct TypeGen {
  pub builder: Builder,
  pub class_cache: HashMap<String, TypeRef>,
  array_types: HashMap<TypeRef, TypeRef>,
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
        // NOTE: Here we generate a pointer type for class type
        let res = self.class_cache.get(&class.id.literal).unwrap();
        return res.ptr_type(self.builder.context());
      }
      ast::Type::Builtin(builtin) => {
        let ty = self.builtin_to_llvm(builtin.as_ref());
        ty
      }
      ast::Type::Array(array) => {
        self.array_to_llvm(array)
      }
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
    self.generate_array_runtime(res)
  }

  fn extract_array_type_name(&self, scalar_ty: &TypeRef) -> String {
    return scalar_ty
      .to_string(&self.builder.module.context)
      .chars()
      .filter(|c| *c != ' ')
      .map(|c| if c == '*' { "ptr".to_string() } else { c.to_string() })
      .collect::<Vec<String>>()
      .join("");
  }


  fn generate_array_runtime(&mut self, ty: TypeRef) -> TypeRef {
    if let Some(res) = self.array_types.get(&ty) {
      return res.clone()
    }
    if let Some(ptr_ty) = ty.as_ref::<PointerType>(&self.builder.module.context) {
      if let &TKindCode::StructType = ptr_ty.get_pointee_ty().kind() {
      } else {
        eprintln!("origin ty: {}", ty.to_string(&self.builder.module.context));
        let raw = ptr_ty.get_pointee_ty();
        let scalar_ty = self.generate_array_runtime(raw.clone());
        let type_name = self.extract_array_type_name(&scalar_ty);
        let type_name = format!("array.{}", type_name.replace("%", "_"));
        // Declare the array type.
        let array_ty = self.builder.create_struct(type_name.clone());
        let length = self.builder.context().int_type(32);
        let array_ptr = self.builder.context().pointer_type(scalar_ty.clone());
        array_ty
          .as_mut::<StructType>(self.builder.context())
          .unwrap()
          .set_body(vec![length, array_ptr]);
        self.class_cache.insert(type_name, array_ty.clone());
        let res = self.builder.module.context.pointer_type(array_ty.clone());
        self.array_types.insert(ty.clone(), res.clone());
        eprintln!("mapped to res: {}", array_ty.as_ref::<StructType>(&self.builder.module.context).unwrap().to_string());
        return res;
      }
    }
    return ty;
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
  tg: TypeGen,
  cache_stack: CacheStack,
  loop_cond_or_end: Option<(ValueRef, ValueRef)>,
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
      let start = self.tg.builder.create_function("llvm.lifetime.start".to_string(), fty.clone());
      self.cache_stack.insert("llvm.lifetime.start".to_string(), start);
      let end = self.tg.builder.create_function("llvm.lifetime.end".to_string(), fty);
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
          let i8ty = self.tg.builder.context().int_type(8);
          self.tg.builder.context().pointer_type(i8ty)
        } else {
          // If not, respect the compiler.
          self.tg.type_to_llvm(&func.ty)
        };
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
        (0..llvm_func.get_num_args()).map(|i| {
          llvm_func.get_arg(i)
        }).collect::<Vec<ValueRef>>()
      };
      for (i, arg) in args.iter().enumerate() {
        let ty = arg.get_type(self.tg.builder.context());
        let ptr = self.builder_mut().create_alloca(ty, format!("arg_{}", i));
        self.generate_lifetime_annot(ptr.clone(), "start");
        self.builder_mut().create_store(arg.clone(), ptr.clone()).unwrap();
        self.cache_stack.insert(
          func.args[i].id.literal.clone(),
          ptr
        );
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
    let alloca = self.tg.builder.create_alloca(ty, var.id.literal.clone());
    // Restore block and instruction
    self.tg.builder.set_current_block(block);
    if let Some(insert_before) = insert_before {
      self.tg.builder.set_insert_before(insert_before);
    }
    self.generate_lifetime_annot(alloca.clone(), "start");
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
          self.tg.builder.create_unconditional_branch(end.clone());
        }
        TokenType::KeywordContinue => {
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
    self.builder_mut().create_conditional_branch(cond, then_block.clone(), else_block.clone(), false);
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
    let old = self.loop_cond_or_end.clone();
    self.cache_stack.push();
    let cond_block = self.builder_mut().add_block("while.cond".to_string());
    let body_block = self.builder_mut().add_block("while.body".to_string());
    let end_block = self.builder_mut().add_block("while.end".to_string());
    let cond = self.generate_expr(&while_stmt.cond, false);
    self.builder_mut().create_conditional_branch(cond, body_block.clone(), end_block.clone(), true);
    // Set it to the inner most loop.
    self.loop_cond_or_end = (cond_block.clone(), end_block.clone()).into();
    self.builder_mut().set_current_block(cond_block.clone());
    let cond = self.generate_expr(&while_stmt.cond, false);
    self.builder_mut().create_conditional_branch(cond, body_block.clone(), end_block.clone(), true);
    self.builder_mut().set_current_block(body_block);
    self.generate_compound_stmt(&while_stmt.body, false);
    self.builder_mut().create_unconditional_branch(cond_block.clone());
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
    let body_block = self.builder_mut().add_block("for.body".to_string());
    let cond_block = self.builder_mut().add_block("for.cond".to_string());
    let end_block = self.builder_mut().add_block("for.end".to_string());
    // Set it to the inner most loop.
    self.loop_cond_or_end = (cond_block.clone(), end_block.clone()).into();
    let extent = self.generate_expr(&for_stmt.end, false);
    let i32ty = self.tg.builder.context().int_type(32);
    // If extent <= loop start, just skip the loop.
    let init = {
      let init = self.cache_stack.get(&for_stmt.var.id.literal).unwrap();
      let init = self.builder_mut().create_load(init);
      init
    };
    let precond = self.builder_mut().create_sle(extent.clone(), init);
    self.builder_mut().create_conditional_branch(precond, end_block.clone(), body_block.clone(), false);
    // Generate the loop conditions.
    self.builder_mut().set_current_block(cond_block.clone());
    let loop_var_addr = self.cache_stack.get(&for_stmt.var.id.literal).unwrap();
    let loop_var_value = self.builder_mut().create_load(loop_var_addr.clone());
    let cond = self.builder_mut().create_slt(loop_var_value, extent);
    self.builder_mut().create_conditional_branch(cond, body_block.clone(), end_block.clone(), true);
    self.builder_mut().set_current_block(body_block);
    self.generate_compound_stmt(&for_stmt.body, false);
    let loop_var_value = self.builder_mut().create_load(loop_var_addr.clone());
    let one = self.tg.builder.context().const_value(i32ty.clone(), 1);
    let added = self.builder_mut().create_add(loop_var_value, one);
    self.builder_mut().create_store(added, loop_var_addr).unwrap();
    self.builder_mut().create_unconditional_branch(cond_block.clone());
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
        let i8ptr = i8ty.ptr_type(self.tg.builder.context());
        let str_value = self.tg.builder.create_string(value.value.clone());
        let i8array = self.tg.generate_array_runtime(i8ptr.clone());
        let i8array = {
          let ptr = i8array.as_ref::<PointerType>(&self.tg.builder.module.context).unwrap();
          ptr.get_pointee_ty()
        };
        let len = value.value.len();
        let i32ty = self.tg.builder.context().int_type(32);
        let len = self.tg.builder.context().const_value(i32ty.clone(), len as u64);
        let zero = self.tg.builder.context().const_value(i32ty.clone(), 0);
        let i8array_ptr = self.tg.builder.create_gep(i8ptr, str_value, vec![zero.clone(), zero.clone()], true, "arraycast".to_string());
        let i8array_obj = self.tg.builder.create_global_struct(i8array, vec![len, i8array_ptr]);
        let string_ty = self.tg.class_cache.get("string").unwrap().clone();
        // let str_ptr = self.tg.builder.get_struct_field(i8array, 0).unwrap();
        self.tg.builder.create_global_struct(string_ty, vec![i8array_obj])
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
        let res = self.tg.builder.get_struct_field(this, aa.idx, "").unwrap();
        if is_lval {
          res
        } else {
          self.tg.builder.create_load(res)
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
            let alloca = self.tg.builder.create_alloca(i1ty.clone(), "sc.result".to_string());
            // Prepare the values
            let one = self.tg.builder.context().const_value(i1ty.clone(), 1);
            let zero = self.tg.builder.context().const_value(i1ty.clone(), 0);
            // Create two blocks for the short-circuit evaluation.
            let true_block = self.tg.builder.add_block(format!("sc.true.{}", alloca.skey));
            let false_block = self.tg.builder.add_block(format!("sc.false.{}", alloca.skey));
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
            let converge = self.tg.builder.add_block(format!("sc.converge.{}", alloca.skey));
            self.tg.builder.create_unconditional_branch(converge.clone());
            // Create the false block.
            self.tg.builder.set_current_block(finalize);
            self.tg.builder.create_store(finalized_value, alloca.clone()).unwrap();
            self.tg.builder.create_unconditional_branch(converge.clone());
            // Create the converge block.
            self.tg.builder.set_current_block(converge);
            let res = Some(self.tg.builder.create_load(alloca.clone()));
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
                  eprintln!("Failed to store:\n{}", lhs.to_string(&self.tg.builder.module.context, true));
                  eprintln!("Failed to store:\n{}", rhs.to_string(&self.tg.builder.module.context, true));
                  panic!("Failed to cg:\n{}, msg: {}", expr, msg);
                }
              }
            }
            TokenType::LT => self.tg.builder.create_slt(lhs, rhs),
            TokenType::LE => self.tg.builder.create_sle(lhs, rhs),
            TokenType::GT => self.tg.builder.create_sgt(lhs, rhs),
            TokenType::GE => self.tg.builder.create_sge(lhs, rhs),
            TokenType::EQ => self.tg.builder.create_eq(lhs, rhs),
            TokenType::NE => self.tg.builder.create_ne(lhs, rhs),
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
        let ty = self.tg.type_to_llvm(&ne.dtype);
        let size = {
          let scalar_ty = ty
            .as_ref::<PointerType>(&self.tg.builder.module.context).unwrap()
            .get_pointee_ty();
          let res = scalar_ty.get_scalar_size_in_bits(&self.tg.builder.module);
          // eprintln!("{}: ty: {}, size: {}", line!(), scalar_ty.to_string(&self.tg.builder.module.context), res);
          res / 8
        };
        let size = self.tg.builder.context().const_value(i32ty.clone(), size as u64);
        let obj = self.tg.builder.create_func_call(malloc.clone(), vec![size]);
        let obj = self.tg.builder.create_bitcast(obj, ty.clone());

        if let ast::Type::Array(array) = &ne.dtype {
          // Array type.
          let ptr_ty = ty.as_ref::<PointerType>(&self.tg.builder.module.context).unwrap();
          let array_ty = ptr_ty.get_pointee_ty();
          let obj_ptr_ty = array_ty
            .as_ref::<StructType>(&self.tg.builder.module.context)
            .unwrap()
            .get_attr(1);
          // Array length.
          let array_len = self.generate_expr(&array.dims[0], false);
          // Write array length to object's first field.
          let obj_len = self.tg.builder.get_struct_field(obj.clone(), 0, "array.length").unwrap();
          self.tg.builder.create_store(array_len.clone(), obj_len).unwrap();
          // Array size.
          let elem_ty = obj_ptr_ty
            .as_ref::<PointerType>(&self.tg.builder.module.context)
            .unwrap()
            .get_pointee_ty();
          let obj_size = elem_ty.get_scalar_size_in_bits(&self.tg.builder.module) / 8;
          let obj_size = self.tg.builder.context().const_value(i32ty.clone(), obj_size as u64);
          // Allocate array buffer.
          let array_size = self.tg.builder.create_mul(array_len, obj_size);
          let payload = self.tg.builder.create_func_call(malloc, vec![array_size]);
          // Write array buffer to object's second field.
          let payload = self.tg.builder.create_bitcast(payload, obj_ptr_ty.clone());
          let payload_ptr = self.tg.builder.get_struct_field(obj.clone(), 1, "array.payload").unwrap();
          self.tg.builder.create_store(payload, payload_ptr).unwrap();
        }

        self.tg.builder.create_bitcast(obj, ty)
      }
      ast::Expr::ArrayIndex(array_idx) => {
        let mut array_obj = self.generate_expr(&array_idx.array, false);
        // eprintln!("array obj: {}", array_obj.to_string(&self.tg.builder.module.context, true));
        let indices = array_idx.indices.iter().map(|x| self.generate_expr(x, false)).collect::<Vec<_>>();
        // for (i, elem) in indices.iter().enumerate() {
        //   eprintln!("idx_{}: {}", i, elem.to_string(&self.tg.builder.module.context, true));
        // }
        for (i, idx) in indices.iter().enumerate() {
          // array_obj = struct { length=i32, payload=ptr } where ptr is the pointer to the array.
          let payload_ptr = self.tg.builder.get_struct_field(array_obj.clone(), 1, "array.payload").unwrap();
          // Dereference &array_obj.payload where payload = array_obj.payload
          let payload = self.tg.builder.create_load(payload_ptr);
          // &array_ptr[idx]
          array_obj = self.tg.builder.index_array(payload, idx.clone()).unwrap();
          // If we have further indices, we need to use it as a right-value.
          // Dereference the address.
          if i != indices.len() - 1 {
            array_obj = self.tg.builder.create_load(array_obj);
            eprintln!("load array[i]: {}", array_obj.as_ref::<Instruction>(&self.tg.builder.module.context).unwrap().to_string(false));
          }
        }
        if !is_lval {
          let res = self.tg.builder.create_load(array_obj);
          eprintln!("rval array[i]: {}", res.as_ref::<Instruction>(&self.tg.builder.module.context).unwrap().to_string(false));
          res
        } else {
          array_obj
        }
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

    // Manually inline array::length.
    if call.fname.literal == "array::length".to_string() {
      let array = params.get(0).unwrap().clone();
      let len_ptr = self.tg.builder.get_struct_field(array, 0, "array.length").unwrap();
      return self.tg.builder.create_load(len_ptr);
    }

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
        // Note: We insert the struct type into the cache, NOT the pointer of the struct type.
        class_cache.insert(class.id().clone(), sty_ref);
      }
    }
  }

  let mut tg = TypeGen{
    builder,
    class_cache,
    array_types: HashMap::new()
  };
  tg.enter_linkage(ast);

  let mut cg = CodeGen{
    tg,
    cache_stack: CacheStack::new(),
    loop_cond_or_end: None,
  };

  cg.generate_linkage(ast);

  return cg.tg.builder.module;
}

