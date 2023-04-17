use std::rc::Rc;
use std::collections::HashMap;

use inkwell::values::{BasicValueEnum, BasicMetadataValueEnum, AnyValue, AnyValueEnum};

use inkwell::{
  context::{Context, ContextRef},
  module::Module,
  builder::Builder,
  types::{AnyTypeEnum, BasicTypeEnum, BasicMetadataTypeEnum},
  AddressSpace
};

use super::ast::*;

struct TypeGen<'ctx> {
  pub ty_cache: HashMap<String, AnyTypeEnum<'ctx>>,
  context: &'ctx Context,
  pregen: bool,
}

impl<'ctx> TypeGen<'ctx> {

  fn enter_linkage(&mut self, ast: &Rc<Linkage>) {
    assert_eq!(ast.tus.len(), 2);
    for tu in &ast.tus {
      self.generate_translation_unit(&tu);
    }
  }

  fn generate_translation_unit(&mut self, tu: &Rc<TranslateUnit>) {
    for decl in &tu.decls {
      match decl {
        Decl::Class(class) => {
          self.class_to_struct(&class);
        }
        _ => {}
      }
    }
  }

  fn type_to_llvm(&mut self, ty: &Type) -> AnyTypeEnum<'ctx> {
    match ty {
      Type::Class(class) => {
        if self.pregen {
          self.context.i8_type().ptr_type(AddressSpace::from(0)).into()
        } else {
          let llvm_struct = self.ty_cache.get(&class.id.literal).unwrap();
          match llvm_struct {
            AnyTypeEnum::StructType(t) => {
              let res : AnyTypeEnum = t.ptr_type(AddressSpace::from(0)).into();
              return res;
            }
            _ => { panic!("Unsupported type {}", ty); }
          }
        }
      }
      Type::Builtin(builtin) => self.builtin_to_llvm(builtin.as_ref()),
      Type::Array(array) => self.array_to_llvm(array),
    }
  }

  fn class_to_struct(&mut self, class: &Rc<ClassDecl>) {
    if self.pregen {
      self.ty_cache.insert(class.id.literal.clone(),
                           self.context.opaque_struct_type(&class.id.literal).into());
    } else {
      let attrs : Vec<BasicTypeEnum> = class.attrs.iter().map(
        |attr| {
          match self.type_to_llvm(&attr.ty) {
            AnyTypeEnum::IntType(t) => t.into(),
            AnyTypeEnum::PointerType(t) => t.into(),
            _ => { panic!("Unknown type {}", attr.ty); }
          }
        }).collect();
      match self.ty_cache.get_mut(&class.id.literal).unwrap() {
        AnyTypeEnum::StructType(t) => {
          t.set_body(&attrs, false);
        }
        _ => { panic!("Unknown type {}", class.id.literal); }
      }
    }
  }

  fn builtin_to_llvm(&mut self, builtin : &BuiltinType) -> AnyTypeEnum<'ctx> {
    match builtin.code {
      BuiltinTypeCode::Int => self.context.i32_type().into(),
      BuiltinTypeCode::Char => self.context.i8_type().into(),
      BuiltinTypeCode::Void => self.context.void_type().into(),
      BuiltinTypeCode::Bool => self.context.bool_type().into(),
      _ => { panic!("Unknown builtin type {}", builtin.token.literal); }
    }
  }

  fn array_to_llvm(&mut self, array: &ArrayType) -> AnyTypeEnum<'ctx> {
    let mut res = self.type_to_llvm(&array.scalar_ty);
    for _ in 0..array.dims {
      res = match res {
        AnyTypeEnum::IntType(t) => t.ptr_type(AddressSpace::from(0)).into(),
        AnyTypeEnum::PointerType(t) => t.ptr_type(AddressSpace::from(0)).into(),
        AnyTypeEnum::StructType(t) => t.ptr_type(AddressSpace::from(0)).into(),
        AnyTypeEnum::VectorType(t) => t.ptr_type(AddressSpace::from(0)).into(),
        _ => { panic!("Unknown type"); }
      }
    }
    res
  }

}

struct ValueCache<'ctx> {
  pub cache: HashMap<String, BasicValueEnum<'ctx>>,
}

impl<'ctx> ValueCache<'ctx> {

  fn insert(&mut self, key: String, value: BasicValueEnum<'ctx>) {
    self.cache.insert(key, value);
  }

  fn get(&self, key: &String) -> Option<&BasicValueEnum<'ctx>> {
    self.cache.get(key)
  }

}

struct CacheStack<'ctx> {
  pub stack: Vec<ValueCache<'ctx>>,
}

impl<'ctx> CacheStack<'ctx> {

  fn push(&mut self) {
    self.stack.push(ValueCache { cache: HashMap::new() });
  }

  fn insert(&mut self, key: String, value: BasicValueEnum<'ctx>) {
    self.stack.last_mut().unwrap().insert(key, value);
  }

  fn pop(&mut self) -> ValueCache<'ctx> {
    self.stack.pop().unwrap()
  }

  fn get(&self, key: &String) -> Option<&BasicValueEnum<'ctx>> {
    for cache in self.stack.iter().rev() {
      if let Some(value) = cache.get(key) {
        return Some(value);
      }
    }
    None
  }

}

struct CodeGen<'ctx> {
  module: Module<'ctx>,
  types: TypeGen<'ctx>,
  builder: Builder<'ctx>,
  cache_stack: CacheStack<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {

  fn context(&self) -> ContextRef<'ctx> {
    return self.module.get_context();
  }

  fn generate_linkage(&mut self, ast: &Rc<Linkage>) {
    assert_eq!(ast.tus.len(), 2);
    for tu in &ast.tus {
      self.generate_translation_unit(&tu);
    }
  }

  fn generate_var_decl(&mut self, var: &Rc<VarDecl>) {
    let ty = self.types.type_to_llvm(&var.ty);
    let res = match ty {
      AnyTypeEnum::IntType(t) => self.builder.build_alloca(t, &var.id.literal).into(),
      AnyTypeEnum::PointerType(t) => self.builder.build_alloca(t, &var.id.literal).into(),
      _ => { panic!("Unknown type {}", ty.print_to_string().to_str().unwrap()); }
    };
    self.cache_stack.insert(var.id.literal.clone(), res);
  }

  fn generate_translation_unit(&mut self, tu: &Rc<TranslateUnit>) {
    for decl in &tu.decls {
      if let Decl::Func(func) = decl {
        let ret_ty = self.types.type_to_llvm(&func.ty);
        let args_ty :Vec<BasicMetadataTypeEnum> = func.args.iter().map(
          |arg| {
            match self.types.type_to_llvm(&arg.ty) {
              AnyTypeEnum::IntType(t) => t.into(),
              AnyTypeEnum::PointerType(t) => t.into(),
              _ => { panic!("Unknown type"); }
            }
          }).collect();
        let fty = match ret_ty {
          AnyTypeEnum::IntType(t) => t.fn_type(&args_ty, false),
          AnyTypeEnum::VoidType(t) => t.fn_type(&args_ty, false),
          AnyTypeEnum::PointerType(t) => t.fn_type(&args_ty, false),
          _ => { panic!("Unknown type"); }
        };
        let llvm_func = self.module.add_function(&func.id.literal, fty, None);
        self.context().append_basic_block(llvm_func, "entry");
      }
    }
    for decl in &tu.decls {
      if let Decl::Func(func) = decl {
        self.generate_func(func);
      }
    }
  }

  fn generate_func(&mut self, func: &Rc<FuncDecl>) {
    let llvm_func = self.module.get_function(&func.id.literal).unwrap();
    self.builder.position_at_end(llvm_func.get_first_basic_block().unwrap());
    // push arguments
    self.cache_stack.push();
    func.args.iter().enumerate().for_each(|arg| {
      self.cache_stack.insert(arg.1.id.literal.clone(),
                              llvm_func.get_nth_param(arg.0 as u32).unwrap());
    });
    self.generate_compound_stmt(&func.body, false);
    // restore the scope
    self.cache_stack.pop();
  }

  fn generate_compound_stmt(&mut self, stmt: &Rc<CompoundStmt>, new_scope: bool) {
    if new_scope {
      self.cache_stack.push();
    }
    for stmt in &stmt.stmts {
      self.generate_stmt(&stmt);
    }
  }

  fn generate_stmt(&mut self, stmt: &Stmt) {
    match stmt {
      Stmt::Ret(ret) => {
        if let Some(expr) = &ret.value {
          let val = self.generate_expr(&expr, false);
          self.builder.build_return(Some(&val));
        } else {
          self.builder.build_return(None);
        }
      }
      Stmt::FuncCall(call) => {
        self.generate_func_call(&call);
      }
      _ => {}
    }
  }

  fn generate_expr(&mut self, expr: &Expr, is_lval: bool) -> BasicValueEnum<'ctx> {
    match expr {
      Expr::FuncCall(call) => {
        match self.generate_func_call(&call) {
          Some(x) => x,
          None => { panic!("No return value"); }
        }
      }
      Expr::IntImm(value) => {
        self.generate_int_imm(&value)
      }
      Expr::StrImm(value) => {
        let gv_str = unsafe { self.builder.build_global_string(&value.value.to_string(), "") };
        let ty = self.types.ty_cache.get(&"string".to_string()).unwrap();
        let alloca = self.builder.build_alloca(ty.into_struct_type(), "");
        let zero = self.context().i32_type().const_int(0, false);
        let one = self.context().i32_type().const_int(1, false);
        let len_gep = unsafe { self.builder.build_in_bounds_gep(alloca, &[zero.clone(), zero.clone()], "") };
        self.builder.build_store(len_gep, self.context().i32_type().const_int((value.value.len() - 1) as u64, false));
        let data_gep = unsafe { self.builder.build_in_bounds_gep(alloca, &[zero.clone(), one.clone()], "") };
        let gv_gep = unsafe { self.builder.build_in_bounds_gep(gv_str.as_pointer_value(), &[zero, zero], "") };
        self.builder.build_store(data_gep, gv_gep);
        return alloca.into()
      }
      Expr::Variable(var) => {
        let value = self.cache_stack.get(&var.id.literal).unwrap();
        if let Type::Class(_) = &var.decl.ty {
          return *value;
        }
        if is_lval {
          return *value;
        } else {
          return if let BasicValueEnum::PointerValue(ptr) = value {
            self.builder.build_load(*ptr, "").into()
          } else {
            *value
          }
        }
        // panic!("{} is not a pointer!", value.print_to_string().to_string());
      }
      Expr::AttrAccess(aa) => {
        let this = self.generate_expr(&aa.this, false);
        if let BasicValueEnum::PointerValue(pv) = this {
          let zero = self.module.get_context().i32_type().const_int(0, false);
          let idx = self.context().i32_type().const_int(aa.idx as u64, false);
          let args = vec![zero, idx];
          let ptr = unsafe { self.builder.build_in_bounds_gep(pv, &args, "") };
          if is_lval {
            return ptr.into()
          } else {
            return self.builder.build_load(ptr, "").into()
          }
        } else {
          panic!("{} is not a pointer!", this.print_to_string().to_string());
        }
      }
      _ => { panic!("Unknown expr {}", expr); }
    }
  }

  fn generate_int_imm(&mut self, value: &IntImm) -> BasicValueEnum<'ctx> {
    self.module.get_context().i32_type().const_int(value.value as u64, false).into()
  }

  fn generate_func_call(&mut self, call: &Rc<FuncCall>) -> Option<BasicValueEnum<'ctx>> {
    let params : Vec<BasicMetadataValueEnum> = call.params.iter().map(|arg| {
      let expr = self.generate_expr(&arg, false);
      match expr {
        BasicValueEnum::IntValue(t) => t.into(),
        BasicValueEnum::PointerValue(t) => t.into(),
        _ => { panic!("Unknown type {}", expr.print_to_string().to_string()); }
      }
    }).collect();
    let llvm_call = self.builder.build_call(self.module.get_function(&call.fname.literal).unwrap(), &params, "calltmp");
    llvm_call.try_as_basic_value().left()
  }

}

pub fn codegen<'ctx>(ast: &Rc<Linkage>, ctx: &'ctx Context) -> Module<'ctx> {
  let fname = ast.tus[ast.tus.len() - 1].fname.clone();
  ctx.i8_type();
  let mut tg = TypeGen{
    ty_cache: HashMap::new(),
    context: &ctx,
    pregen: true
  };
  tg.enter_linkage(ast);
  tg.pregen = false;
  tg.enter_linkage(ast);
  // for elem in tg.ty_cache.iter() {
  //   println!("{}: {}", elem.0, elem.1.print_to_string().to_str().unwrap());
  // }

  let mut cg = CodeGen{
    module: ctx.create_module(&fname),
    types: tg,
    builder: ctx.create_builder(),
    cache_stack: CacheStack {
      stack: Vec::new(),
    }
  };
  cg.generate_linkage(ast);
  cg.module.verify().unwrap();

  return cg.module;
}

