use std::rc::Rc;

use super::ast::{
  Type, TranslateUnit, BuiltinType, Variable,
  FuncDecl, CompoundStmt, Stmt, ReturnStmt, IntImm,
  Decl, Expr, FuncCall, Linkage, VarDecl, ClassDecl,
  InlineAsm, BinaryOp, ArrayType, AttrAccess, ArrayIndex,
  NewExpr, Cast, ForStmt, IfStmt, WhileStmt, UnaryOp
};

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
    (Expr::NewExpr(v0),Expr::NewExpr(v1)) => Rc::ptr_eq(v0, v1),
    (Expr::UnknownRef(v0),Expr::UnknownRef(v1)) => v0.literal == v1.literal,
    _ => { false }
  }
}

pub fn stmt_eq(a:&Stmt, b:&Stmt) -> bool {
  match (a, b) {
    (Stmt::Ret(v0),Stmt::Ret(v1)) => Rc::ptr_eq(v0, v1),
    (Stmt::Evaluate(v0),Stmt::Evaluate(v1)) => expr_eq(v0, v1),
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
      if v0.dims.len() == v1.dims.len() && type_eq(&v0.scalar_ty, &v1.scalar_ty) {
        v0.dims.iter().zip(v1.dims.iter()).fold(true, |acc, x| acc && expr_eq(&x.0, &x.1))
      } else {
        false
      }
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
      Expr::UnaryOp(op) => self.visit_unary_op(op),
      Expr::AttrAccess(access) => self.visit_attr_access(access),
      Expr::ArrayIndex(array_idx) => self.visit_array_index(array_idx),
      Expr::NewExpr(ne) => self.visit_new_expr(ne),
      Expr::Cast(cast) => self.visit_cast(cast),
      Expr::UnknownRef(x) => Expr::UnknownRef(x.clone()),
    }
  }

  fn visit_array_index(&mut self, array_idx: &Rc<ArrayIndex>) -> Expr {
    let array = self.visit_expr(&array_idx.array);
    let indices = array_idx.indices.iter().map(|idx| self.visit_expr(idx)).collect::<Vec<_>>();
    if expr_eq(&array, &array_idx.array) && !mutated!(indices, array_idx.indices, expr_eq) {
      return Expr::ArrayIndex(array_idx.clone()).clone();
    }
    Expr::ArrayIndex(Rc::new(ArrayIndex {
      array,
      indices
    }))
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
      Stmt::Evaluate(expr) => Stmt::Evaluate(self.visit_expr(expr)),
      Stmt::InlineAsm(asm) => self.visit_inline_asm(asm),
      Stmt::VarDecl(decl) => Stmt::VarDecl(self.visit_var_decl(decl)),
      Stmt::ForStmt(for_loop) => Stmt::ForStmt(self.visit_for_stmt(for_loop)),
      Stmt::CompoundStmt(stmt) => Stmt::CompoundStmt(self.visit_compound_stmt(stmt)),
      Stmt::IfStmt(if_stmt) => Stmt::IfStmt(self.visit_if_stmt(if_stmt)),
      Stmt::WhileStmt(while_stmt) => Stmt::WhileStmt(self.visit_while_stmt(while_stmt)),
      Stmt::LoopJump(jump) => Stmt::LoopJump(jump.clone()),
    }
  }

  fn visit_while_stmt(&mut self, while_stmt: &Rc<WhileStmt>) -> Rc<WhileStmt> {
    let cond = self.visit_expr(&while_stmt.cond);
    let body = self.visit_compound_stmt(&while_stmt.body);
    if expr_eq(&cond, &while_stmt.cond) && !Rc::ptr_eq(&body, &while_stmt.body) {
      while_stmt.clone()
    } else {
      Rc::new(WhileStmt {
        loc: while_stmt.loc.clone(),
        cond, body,
      })
    }
  }

  fn visit_if_stmt(&mut self, if_stmt: &Rc<IfStmt>) -> Rc<IfStmt> {
    let cond = self.visit_expr(&if_stmt.cond);
    let then_body = self.visit_compound_stmt(&if_stmt.then_body);
    let else_body = if let Some(else_body) = &if_stmt.else_body {
      Some(self.visit_compound_stmt(else_body))
    } else {
      None
    };
    Rc::new(IfStmt {
      cond,
      then_body,
      else_body,
    })
  }

  fn visit_linkage(&mut self, linkage:&Rc<Linkage>) -> Rc<Linkage> {
    let tus:Vec<Rc<TranslateUnit>> =
      linkage.tus.iter().map(|elem| self.visit_tu(elem)).collect();
    if mutated!(tus, linkage.tus) {
      return Rc::new(Linkage {
        tus,
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
        rewrite: call.rewrite,
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
    match &var.init {
      Some(init) => {
        let new_init = self.visit_expr(init);
        if expr_eq(&new_init, init) {
          var.clone()
        } else {
          Rc::new(VarDecl{ ty, id: var.id.clone(), init: Some(new_init) })
        }
      }
      None => {
        Rc::new(VarDecl{ ty, id: var.id.clone(), init: None })
      }
    }
  }

  fn visit_compound_stmt(&mut self, block: &Rc<CompoundStmt>) -> Rc<CompoundStmt> {
    let stmts:Vec<Stmt> = block.stmts.iter().map(|x| self.visit_stmt(x)).collect();
    if mutated!(stmts, block.stmts, stmt_eq) {
      return Rc::new(CompoundStmt{
        left: block.left.clone(),
        right: block.right.clone(),
        stmts,
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

  fn visit_unary_op(&mut self, op: &Rc<UnaryOp>) -> Expr {
    let expr = self.visit_expr(&op.expr);
    if expr_eq(&expr, &op.expr) {
      return Expr::UnaryOp(op.clone())
    }
    Expr::UnaryOp(Rc::new(UnaryOp{ op: op.op.clone(), expr }))
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
    let new_dims = x.dims.iter().map(|x| {
      if let Expr::UnknownRef(tok) = x {
        if tok.literal == "" {
          return x.clone();
        }
      }
      return self.visit_expr(x);
    }).collect::<Vec<Expr>>();
    Type::Array(Rc::new(ArrayType{ scalar_ty: ty, dims: new_dims }))
  }

  fn visit_attr_access(&mut self, x: &Rc<AttrAccess>) -> Expr {
    let this = self.visit_expr(&x.this);
    if expr_eq(&this, &x.this) {
      return Expr::AttrAccess(x.clone())
    }
    Expr::AttrAccess(Rc::new(AttrAccess{ this, attr: x.attr.clone(), idx: x.idx }))
  }

  fn visit_new_expr(&mut self, x: &Rc<NewExpr>) -> Expr {
    let ty = self.visit_type(&x.dtype);
    if type_eq(&ty, &x.dtype) {
      return Expr::NewExpr(x.clone());
    }
    return Expr::NewExpr(Rc::new(NewExpr { token: x.token.clone(), dtype: ty }))
  }

  fn visit_cast(&mut self, x: &Rc<Cast>) -> Expr {
    let expr = self.visit_expr(&x.expr);
    let dtype = self.visit_type(&x.dtype);
    if expr_eq(&expr, &x.expr) && type_eq(&dtype, &x.dtype) {
      return Expr::Cast(x.clone());
    }
    return Expr::Cast(Rc::new(Cast{token: x.token.clone(), expr, dtype}))
  }

  fn visit_for_stmt(&mut self, x: &Rc<ForStmt>) -> Rc<ForStmt> {
    let end = self.visit_expr(&x.end);
    let var = self.visit_var_decl(&x.var);
    let body = self.visit_compound_stmt(&x.body);
    if expr_eq(&end, &x.end) && Rc::ptr_eq(&var, &x.var) && Rc::ptr_eq(&body, &x.body) {
      x.clone()
    } else {
      Rc::new(ForStmt { var, end, body })
    }
  }

}

