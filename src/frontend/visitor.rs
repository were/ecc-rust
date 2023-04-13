use std::rc::Rc;

use super::ast::{
  Type, TranslateUnit, BuiltinType, Variable,
  FuncDecl, CompoundStmt, Stmt, ReturnStmt, IntImm,
  Decl, Expr, FuncCall, Linkage, VarDecl, ClassDecl,
  InlineAsm, BinaryOp, ArrayType, AttrAccess
};

use super::sema::SymbolTable;

#[macro_export]
macro_rules! mutated {
  ($a:expr, $b:expr) => {
    Iterator::zip($a.iter(), $b.iter()).fold(false, |acc, x| {
      acc || !Rc::ptr_eq(x.0, x.1)
    })
  };
  ($a:expr, $b:expr, $eq_cond:expr) => {
    Iterator::zip($a.iter(), $b.iter()).fold(false, |acc, x| {
      acc || !$eq_cond(x.0, x.1)
    })
  };
}

pub fn expr_eq(a:&Expr, b:&Expr) -> bool {
  match (a, b) {
    (Expr::StrImm(v0), Expr::StrImm(v1)) => Rc::ptr_eq(v0, v1),
    (Expr::IntImm(v0),Expr::IntImm(v1)) => Rc::ptr_eq(v0, v1),
    (Expr::FuncCall(v0),Expr::FuncCall(v1)) => Rc::ptr_eq(v0, v1),
    (Expr::Variable(v0),Expr::Variable(v1)) => Rc::ptr_eq(v0, v1),
    (Expr::BinaryOp(v0),Expr::BinaryOp(v1)) => Rc::ptr_eq(v0, v1),
    (Expr::AttrAccess(v0),Expr::AttrAccess(v1)) => Rc::ptr_eq(v0, v1),
    (Expr::UnknownRef(v0),Expr::UnknownRef(v1)) => v0.literal == v1.literal,
    _ => { false }
  }
}

pub fn stmt_eq(a:&Stmt, b:&Stmt) -> bool {
  match (a, b) {
    (Stmt::Ret(v0),Stmt::Ret(v1)) => Rc::ptr_eq(v0, v1),
    (Stmt::FuncCall(v0),Stmt::FuncCall(v1)) => Rc::ptr_eq(v0, v1),
    (Stmt::InlineAsm(v0),Stmt::InlineAsm(v1)) => Rc::ptr_eq(v0, v1),
    _ => { false }
  }
}

pub fn type_eq(a:&Type, b:&Type) -> bool {
  match (a, b) {
    (Type::Builtin(v0), Type::Builtin(v1)) => {
      v0.token.literal == v1.token.literal
    }
    (Type::Array(v0), Type::Array(v1)) => {
      v0.dims == v1.dims && type_eq(&v0.scalar_ty, &v1.scalar_ty)
    }
    (Type::Class(v0), Type::Class(v1)) => v0.id.literal == v1.id.literal,
    _ => { false }
  }
}

pub fn decl_eq(a:&Decl, b:&Decl) -> bool {
  match (a, b) {
    (Decl::Func(v0), Decl::Func(v1)) => Rc::ptr_eq(v0, v1),
    (Decl::Class(v0), Decl::Class(v1)) => Rc::ptr_eq(v0, v1),
    _ => false
  }
}

pub trait Visitor {

  fn visit_decl(&mut self, decl:&Decl) -> Decl {
    match decl {
      Decl::Func(func) => Decl::Func(self.visit_func(func)),
      Decl::Class(class) => Decl::Class(self.visit_class(class)),
    }
  }

  fn visit_expr(&mut self, expr: &Expr) -> Expr {
    match expr {
      Expr::IntImm(imm) => self.visit_intimm(imm),
      Expr::StrImm(s) => Expr::StrImm(s.clone()),
      Expr::FuncCall(call) => Expr::FuncCall(self.visit_func_call(call)),
      Expr::Variable(var) => self.visit_var(var),
      Expr::BinaryOp(op) => self.visit_binary_op(op),
      Expr::AttrAccess(access) => self.visit_attr_access(access),
      Expr::UnknownRef(x) => Expr::UnknownRef(x.clone()),
    }
  }

  fn visit_type(&mut self, ty: &Type) -> Type {
    match ty {
      Type::Builtin(builtin) => self.visit_builtin(builtin),
      Type::Array(array_ty) => self.visit_array_ty(array_ty),
      Type::Class(class) => Type::Class(class.clone()),
    }
  }

  fn visit_stmt(&mut self, stmt: &Stmt) -> Stmt {
    match stmt {
      Stmt::Ret(ret) => self.visit_return(&ret),
      Stmt::FuncCall(call) => Stmt::FuncCall(self.visit_func_call(call)),
      Stmt::InlineAsm(asm) => self.visit_inline_asm(asm)
    }
  }

  fn visit_linkage(&mut self, linkage:&Rc<Linkage>) -> Rc<Linkage> {
    let tus:Vec<Rc<TranslateUnit>> =
      linkage.tus.iter().map(|elem| self.visit_tu(elem)).collect();
    if mutated!(tus, linkage.tus) {
      return Rc::new(Linkage {
        tus,
        symbols: linkage.symbols.clone()
      })
    }
    linkage.clone()
  }

  fn visit_class(&mut self, class:&Rc<ClassDecl>) -> Rc<ClassDecl> {
    let methods:Vec<Rc<FuncDecl>> =
      class.methods.iter().map(|elem| self.visit_func(elem)).collect();
    let attrs:Vec<Rc<VarDecl>> =
      class.attrs.iter().map(|elem| self.visit_var_decl(elem)).collect();
    if mutated!(methods, class.methods) || mutated!(attrs, class.attrs) {
      return Rc::new(ClassDecl{ id: class.id.clone(), methods, attrs, })
    }
    class.clone()
  }

  fn visit_func_call(&mut self, call: &Rc<FuncCall>) -> Rc<FuncCall> {
    let params:Vec<Expr> = call.params.iter().map(|x| self.visit_expr(x)).collect();
    if mutated!(params, call.params, expr_eq) {
      return Rc::new(FuncCall{
        fname: call.fname.clone(),
        params,
      })
    }
    call.clone()
  }

  fn visit_tu(&mut self, tu: &Rc<TranslateUnit>) -> Rc<TranslateUnit> {
    let decls:Vec<Decl> = tu.decls.iter().map(|x| self.visit_decl(x)).collect();
    if mutated!(decls, tu.decls, decl_eq) {
      return Rc::new(TranslateUnit{ fname: tu.fname.clone(), decls })
    }
    return tu.clone()
  }

  fn visit_func(&mut self, func: &Rc<FuncDecl>) -> Rc<FuncDecl> {
    let body:Rc<CompoundStmt> = self.visit_compound_stmt(&func.body);
    let args:Vec<Rc<VarDecl>> = func.args.iter().map(|x| self.visit_var_decl(x)).collect();
    if !Rc::ptr_eq(&body, &func.body) || mutated!(args, func.args) {
      return Rc::new(FuncDecl{
        ty: func.ty.clone(),
        id: func.id.clone(),
        args, body,
      })
    }
    func.clone()
  }

  fn visit_var_decl(&mut self, var: &Rc<VarDecl>) -> Rc<VarDecl> {
    let ty = self.visit_type(&var.ty);
    if type_eq(&ty, &var.ty) {
      return var.clone()
    }
    Rc::new(VarDecl{ ty, id: var.id.clone() })
  }

  fn visit_compound_stmt(&mut self, block: &Rc<CompoundStmt>) -> Rc<CompoundStmt> {
    let stmts:Vec<Stmt> = block.stmts.iter().map(|x| self.visit_stmt(x)).collect();
    if mutated!(stmts, block.stmts, stmt_eq) {
      return Rc::new(CompoundStmt{
        left: block.left.clone(),
        right: block.right.clone(),
        stmts, symbols: Rc::new(SymbolTable::new())
      })
    }
    block.clone()
  }

  fn visit_inline_asm(&mut self, asm: &Rc<InlineAsm>) -> Stmt {
    let args:Vec<Expr> = asm.args.iter().map(|x| self.visit_expr(x)).collect();
    if mutated!(args, asm.args, expr_eq) {
      return Stmt::InlineAsm(Rc::new(InlineAsm{
        code : asm.code.clone(), args, operands: asm.operands.clone()
      }))
    }
    return Stmt::InlineAsm(asm.clone())
  }

  fn visit_binary_op(&mut self, op: &Rc<BinaryOp>) -> Expr {
    let lhs = self.visit_expr(&op.lhs);
    let rhs = self.visit_expr(&op.rhs);
    if expr_eq(&lhs, &op.lhs) && expr_eq(&rhs, &op.rhs) {
      return Expr::BinaryOp(op.clone())
    }
    Expr::BinaryOp(Rc::new(BinaryOp{ lhs, rhs, op: op.op.clone() }))
  }

  fn visit_return(&mut self, x: &Rc<ReturnStmt>) -> Stmt {
    if let Some(val) = &x.value {
      let new_val = self.visit_expr(&val);
      if expr_eq(&new_val, &val) {
        return Stmt::Ret(x.clone())
      }
      return Stmt::Ret(Rc::new(ReturnStmt{ token: x.token.clone(), value: Some(new_val) }))
    }
    Stmt::Ret(x.clone())
  }

  fn visit_builtin(&mut self, x: &Rc<BuiltinType>) -> Type {
    Type::Builtin(x.clone())
  }

  fn visit_var(&mut self, x: &Rc<Variable>) -> Expr {
    Expr::Variable(x.clone())
  }

  fn visit_intimm(&mut self, x: &Rc<IntImm>) -> Expr {
    Expr::IntImm(x.clone())
  }

  fn visit_array_ty(&mut self, x: &Rc<ArrayType>) -> Type {
    let ty = self.visit_type(&x.scalar_ty);
    if type_eq(&ty, &x.scalar_ty) {
      return Type::Array(x.clone())
    }
    Type::Array(Rc::new(ArrayType{ scalar_ty: ty, dims: x.dims }))
  }

  fn visit_attr_access(&mut self, x: &Rc<AttrAccess>) -> Expr {
    let this = self.visit_expr(&x.this);
    if expr_eq(&this, &x.this) {
      return Expr::AttrAccess(x.clone())
    }
    Expr::AttrAccess(Rc::new(AttrAccess{ this, attr: x.attr.clone(), idx: x.idx }))
  }

}

