use super::ast::{Type, TranslateUnit, BuiltinType, Variable,
                 FuncDecl, CompoundStmt, Stmt, ReturnStmt, IntImm, Decl, Expr};

pub trait Visitor {

  fn visit_decl(&self, decl: &Decl) {
    match decl {
      Decl::Func(func) => self.visit_func(func)
    }
  }

  fn visit_expr(&self, expr: &Expr) {
    match expr {
      Expr::IntImm(imm) => self.visit_intimm(imm)
    }
  }

  fn visit_type(&self, ty: &Type) {
    match ty {
      Type::Builtin(builtin) => self.visit_builtin(builtin)
    }
  }

  fn visit_stmt(&self, stmt: &Stmt) {
    match stmt {
      Stmt::Ret(ret) => self.visit_return(ret)
    }
  }

  fn visit_tu(&self, tu: &TranslateUnit) {
    for decl in tu.decls.iter() {
      self.visit_decl(decl)
    }
  }

  fn visit_var(&self, var: &Variable) {
    self.visit_type(&var.ty)
  }

  fn visit_func(&self, func: &FuncDecl) {
    self.visit_var(&func.var);
    self.visit_compound_stmt(&func.body)
  }

  fn visit_compound_stmt(&self, block: &CompoundStmt) {
    for elem in block.stmts.iter() {
      self.visit_stmt(elem)
    }
  }

  fn visit_return(&self, _: &ReturnStmt) {}
  fn visit_builtin(&self, _: &BuiltinType) {}
  fn visit_intimm(&self, _: &IntImm) {}

}

