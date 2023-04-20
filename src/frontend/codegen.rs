use std::rc::Rc;
use std::collections::HashMap;

use crate::ir::{
  self,
  value::{ValueRef, VKindCode},
  types::{StructType, StructTypeRef, Type}, function::Function,
};
use super::ast;

struct TypeGen {
  pub builder: ir::builder::Builder,
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

  fn type_to_llvm(&mut self, ty: &ast::Type) -> ir::types::Type {
    match ty {
      ast::Type::Class(class) => {
        let module = &self.builder.module;
        let sty = module.get_struct(&class.id.literal).unwrap();
        return ir::types::Type::StructTypeRef(Rc::new(StructTypeRef{ name: sty.name.clone() })).ptr_type();
      }
      ast::Type::Builtin(builtin) => self.builtin_to_llvm(builtin.as_ref()),
      ast::Type::Array(array) => self.array_to_llvm(array),
    }
  }

  fn class_to_struct(&mut self, class: &Rc<ast::ClassDecl>) {
    let attrs : Vec<ir::types::Type> = class.attrs.iter().map(
      |attr| { self.type_to_llvm(&attr.ty) }).collect();
    let sty = self.builder.module.get_struct_mut(class.id()).unwrap();
    sty.set_body(attrs);
  }

  fn builtin_to_llvm(&mut self, builtin : &ast::BuiltinType) -> ir::types::Type {
    match builtin.code {
      ast::BuiltinTypeCode::Int => self.builder.int_type(32),
      ast::BuiltinTypeCode::Char => self.builder.int_type(8),
      ast::BuiltinTypeCode::Void => self.builder.void_type(),
      ast::BuiltinTypeCode::Bool => self.builder.int_type(1),
      _ => { panic!("Unknown builtin type {}", builtin.token.literal); }
    }
  }

  fn array_to_llvm(&mut self, array: &ast::ArrayType) -> ir::types::Type {
    let mut res = self.type_to_llvm(&array.scalar_ty);
    for _ in 0..array.dims {
      res = res.ptr_type();
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
}
 
impl CodeGen {

  fn generate_linkage(&mut self, ast: &Rc<ast::Linkage>) {
    assert_eq!(ast.tus.len(), 2);
    self.cache_stack.push();
    for tu in &ast.tus {
      self.generate_translation_unit(&tu);
    }
  }

  fn module(&self) -> &ir::module::Module {
    &self.tg.builder.module
  }

  fn builder_mut(&mut self) -> &mut ir::builder::Builder {
    &mut self.tg.builder
  }

  fn generate_translation_unit(&mut self, tu: &Rc<ast::TranslateUnit>) {
    for decl in &tu.decls {
      if let ast::Decl::Func(func) = decl {
        let ret_ty = self.tg.type_to_llvm(&func.ty);
        let args_ty :Vec<Type> = func.args.iter().map(|arg|
          self.tg.type_to_llvm(&arg.ty)
        ).collect();
        let fty = ret_ty.fn_type(&args_ty);
        let func_ref = self.tg.builder.add_function(func.id.literal.clone(), fty);
        self.cache_stack.insert(func.id.literal.clone(), func_ref.clone());
      }
    }
    for decl in tu.decls.iter() {
      if let ast::Decl::Func(func) = decl {
        let func_ref = self.cache_stack.get(&func.id.literal).unwrap();
        self.builder_mut().set_current_function(func_ref);
        let block_ref = self.builder_mut().add_block("entry".to_string());
        self.cache_stack.push();
        func.args.iter().enumerate().for_each(|arg| {
          self.cache_stack.insert(
            arg.1.id.literal.clone(),
            ValueRef{ skey: arg.0 as usize, v_kind: VKindCode::Argument });
        });
        self.builder_mut().set_current_block(block_ref);
        self.generate_func(func);
      }
    }
  }

  fn generate_func(&mut self, func: &Rc<ast::FuncDecl>) {
    // push arguments
    // self.generate_compound_stmt(&func.body, false);
    // // restore the scope
    // self.cache_stack.pop();
  }

//   fn generate_var_decl(&mut self, var: &Rc<VarDecl>) {
//     let ty = self.types.type_to_llvm(&var.ty);
//     let res = match ty {
//       AnyTypeEnum::IntType(t) => self.builder.build_alloca(t, &var.id.literal).into(),
//       AnyTypeEnum::PointerType(t) => self.builder.build_alloca(t, &var.id.literal).into(),
//       _ => { panic!("Unknown type {}", ty.print_to_string().to_str().unwrap()); }
//     };
//     self.cache_stack.insert(var.id.literal.clone(), res);
//   }
// 
// 
// 
//   fn generate_compound_stmt(&mut self, stmt: &Rc<CompoundStmt>, new_scope: bool) {
//     if new_scope {
//       self.cache_stack.push();
//     }
//     for stmt in &stmt.stmts {
//       self.generate_stmt(&stmt);
//     }
//   }
// 
//   fn generate_stmt(&mut self, stmt: &Stmt) {
//     match stmt {
//       Stmt::Ret(ret) => {
//         if let Some(expr) = &ret.value {
//           let val = self.generate_expr(&expr, false);
//           self.builder.build_return(Some(&val));
//         } else {
//           self.builder.build_return(None);
//         }
//       }
//       Stmt::FuncCall(call) => {
//         self.generate_func_call(&call);
//       }
//       Stmt::InlineAsm(asm) => {
//         self.generate_inline_asm(&asm);
//       }
//       _ => {}
//     }
//   }
// 
//   fn generate_expr(&mut self, expr: &Expr, is_lval: bool) -> BasicValueEnum<'ctx> {
//     match expr {
//       Expr::FuncCall(call) => {
//         match self.generate_func_call(&call) {
//           Some(x) => x,
//           None => { panic!("No return value"); }
//         }
//       }
//       Expr::IntImm(value) => {
//         self.generate_int_imm(&value)
//       }
//       Expr::StrImm(value) => {
//         let gv_str = unsafe { self.builder.build_global_string(&value.value.to_string(), "") };
//         let ty = self.types.ty_cache.get(&"string".to_string()).unwrap();
//         let alloca = self.builder.build_alloca(ty.into_struct_type(), "");
//         let zero = self.context().i32_type().const_int(0, false);
//         let one = self.context().i32_type().const_int(1, false);
//         let len_gep = unsafe { self.builder.build_in_bounds_gep(alloca, &[zero.clone(), zero.clone()], "") };
//         self.builder.build_store(len_gep, self.context().i32_type().const_int(value.value.len() as u64, false));
//         let data_gep = unsafe { self.builder.build_in_bounds_gep(alloca, &[zero.clone(), one.clone()], "") };
//         let gv_gep = unsafe { self.builder.build_in_bounds_gep(gv_str.as_pointer_value(), &[zero, zero], "") };
//         self.builder.build_store(data_gep, gv_gep);
//         return alloca.into()
//       }
//       Expr::Variable(var) => {
//         let value = self.cache_stack.get(&var.id.literal).unwrap();
//         if let Type::Class(_) = &var.decl.ty {
//           return *value;
//         }
//         if is_lval {
//           return *value;
//         } else {
//           return if let BasicValueEnum::PointerValue(ptr) = value {
//             self.builder.build_load(*ptr, "").into()
//           } else {
//             *value
//           }
//         }
//         // panic!("{} is not a pointer!", value.print_to_string().to_string());
//       }
//       Expr::AttrAccess(aa) => {
//         let this = self.generate_expr(&aa.this, false);
//         if let BasicValueEnum::PointerValue(pv) = this {
//           let zero = self.module.get_context().i32_type().const_int(0, false);
//           let idx = self.context().i32_type().const_int(aa.idx as u64, false);
//           let args = vec![zero, idx];
//           let ptr = unsafe { self.builder.build_in_bounds_gep(pv, &args, "") };
//           if is_lval {
//             return ptr.into()
//           } else {
//             return self.builder.build_load(ptr, "").into()
//           }
//         } else {
//           panic!("{} is not a pointer!", this.print_to_string().to_string());
//         }
//       }
//       _ => { panic!("Unknown expr {}", expr); }
//     }
//   }
// 
//   fn generate_inline_asm(&mut self, asm: &InlineAsm) {
//     let params : Vec<BasicValueEnum> = asm.args.iter().map(|arg| {
//       self.generate_expr(&arg, false).into()
//     }).collect();
//     let types : Vec<BasicMetadataTypeEnum> = params.iter().map(|arg| {
//       arg.get_type().into()
//     }).collect();
//     let meta_params: Vec<BasicMetadataValueEnum> =
//       params.iter().map(|arg| BasicMetadataValueEnum::try_from(*arg).unwrap()).collect();
//     let asm_fty = self.context().void_type().fn_type(&types, false);
//     let asm_callee =
//       self.context().create_inline_asm(
//         asm_fty, asm.code.value.clone(), asm.operands.value.clone(), true, true, None);
//     let callable_value = CallableValue::try_from(asm_callee).unwrap();
//     self.builder.build_call(callable_value, &meta_params, "");
//   }
// 
//   fn generate_int_imm(&mut self, value: &IntImm) -> BasicValueEnum<'ctx> {
//     self.module.get_context().i32_type().const_int(value.value as u64, false).into()
//   }
// 
//   fn generate_func_call(&mut self, call: &Rc<FuncCall>) -> Option<BasicValueEnum<'ctx>> {
//     let params : Vec<BasicMetadataValueEnum> = call.params.iter().map(|arg| {
//       let expr = self.generate_expr(&arg, false);
//       match expr {
//         BasicValueEnum::IntValue(t) => t.into(),
//         BasicValueEnum::PointerValue(t) => t.into(),
//         _ => { panic!("Unknown type {}", expr.print_to_string().to_string()); }
//       }
//     }).collect();
//     let llvm_call = self.builder.build_call(self.module.get_function(&call.fname.literal).unwrap(), &params, "calltmp");
//     llvm_call.try_as_basic_value().left()
//   }
// 
}

pub fn codegen(ast: &Rc<ast::Linkage>) -> ir::module::Module {
  let fname = ast.tus[ast.tus.len() - 1].fname.clone();
  let mut module = ir::module::Module::new(fname.clone(), fname.clone());

  // Declare all classes
  for tu in ast.tus.iter() {
    for decl in &tu.decls {
      if let ast::Decl::Class(class) = decl {
        module.add_struct_decl(class.id().clone())
      }
    }
  }

  let builder = ir::builder::Builder::new(module);

  let mut tg = TypeGen{ builder };
  tg.enter_linkage(ast);

  let mut cg = CodeGen{
    tg,
    cache_stack: CacheStack {
      stack: Vec::new(),
    }
  };
  //   module: ctx.create_module(&fname),
  //   types: tg,
  //   builder: ctx.create_builder(),
  // };
  cg.generate_linkage(ast);

  return cg.tg.builder.module;
}

