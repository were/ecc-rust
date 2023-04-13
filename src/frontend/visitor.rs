use std::rc::Rc;

use super::ast::{
  Type, TranslateUnit, BuiltinType, Variable,
  FuncDecl, CompoundStmt, Stmt, ReturnStmt, IntImm,
  Decl, Expr, FuncCall, Linkage, VarDecl, ClassDecl,
  InlineAsm, BinaryOp, ArrayType, AttrAccess
};

use super::sema::SymbolTable;

pub trait Visitor {

  fn visit_linkage(&mut self, linkage: &Linkage) -> Linkage {
    let tus = linkage.tus.iter().map(|elem| self.visit_tu(elem)).collect();
    Linkage {
      tus,
      symbols: linkage.symbols.clone()
    }
  }

  fn visit_decl(&mut self, decl: &Decl) -> Decl {
    match decl {
      Decl::Func(func) => Decl::Func(self.visit_func(func)),
      Decl::Class(class) => Decl::Class(self.visit_class(class)),
    }
  }

  fn visit_class(&mut self, class: &Rc<ClassDecl>) -> Rc<ClassDecl> {
    let methods = class.methods.iter().map(|elem| self.visit_func(elem)).collect();
    let attrs = class.attrs.iter().map(|elem| self.visit_var_decl(elem)).collect();
    Rc::new(ClassDecl{ id: class.id.clone(), methods, attrs, })
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

  fn visit_func_call(&mut self, call: &Rc<FuncCall>) -> Rc<FuncCall> {
    let params : Vec<Expr> = call.params.iter().map(|x| self.visit_expr(x)).collect();
    Rc::new(FuncCall{
      fname: call.fname.clone(),
      params,
      func: call.func.clone()
    })
  }

  fn visit_tu(&mut self, tu: &Rc<TranslateUnit>) -> Rc<TranslateUnit> {
    let decls : Vec<Decl> = tu.decls.iter().map(|x| self.visit_decl(x)).collect();
    Rc::new(TranslateUnit{ fname: tu.fname.clone(), decls })
  }

  fn visit_func(&mut self, func: &Rc<FuncDecl>) -> Rc<FuncDecl> {
    let body = self.visit_compound_stmt(&func.body);
    let args = func.args.iter().map(|x| self.visit_var_decl(x)).collect();
    Rc::new(FuncDecl{
      ty: func.ty.clone(),
      id: func.id.clone(),
      args, body,
    })
  }

  fn visit_var_decl(&mut self, var: &Rc<VarDecl>) -> Rc<VarDecl> {
    let ty = self.visit_type(&var.ty);
    Rc::new(VarDecl{ ty, id: var.id.clone() })
  }

  fn visit_compound_stmt(&mut self, block: &Rc<CompoundStmt>) -> Rc<CompoundStmt> {
    let stmts = block.stmts.iter().map(|x| self.visit_stmt(x)).collect();
    Rc::new(CompoundStmt{
      left: block.left.clone(),
      right: block.right.clone(),
      stmts, symbols: Rc::new(SymbolTable::new())
    })
  }

  fn visit_inline_asm(&mut self, asm: &Rc<InlineAsm>) -> Stmt {
    // TODO(@were): Improve the performance later.
    let args : Vec<Expr> = asm.args.iter().map(|x| self.visit_expr(x)).collect();
    Stmt::InlineAsm(Rc::new(InlineAsm{ code : asm.code.clone(), args, operands: asm.operands.clone() }))
  }

  fn visit_binary_op(&mut self, op: &Rc<BinaryOp>) -> Expr {
    let lhs = self.visit_expr(&op.lhs);
    let rhs = self.visit_expr(&op.rhs);
    Expr::BinaryOp(Rc::new(BinaryOp{ lhs, rhs, op: op.op.clone() }))
  }

  fn visit_return(&mut self, x: &Rc<ReturnStmt>) -> Stmt {
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
    Type::Array(Rc::new(ArrayType{ scalar_ty: ty, dims: x.dims }))
  }

  fn visit_attr_access(&mut self, x: &Rc<AttrAccess>) -> Expr {
    let this = self.visit_expr(&x.this);
    Expr::AttrAccess(Rc::new(AttrAccess{ this, attr: x.attr.clone(), idx: x.idx }))
  }

}

