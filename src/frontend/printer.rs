use std::fmt;
use super::ast::{Decl, FuncDecl, Variable, Type, BuiltinType, CompoundStmt, Stmt, ReturnStmt, Expr, TranslateUnit};

fn print_tu(tu: &TranslateUnit, f: &mut fmt::Formatter, indent: &String) -> fmt::Result {
  if tu.decls.len() != 0 {
    let mut res : fmt::Result = Ok(());
    for elem in tu.decls.iter() {
      res = print_decl(elem, f, indent)
    }
    return res
  }
  write!(f, "[Empty Program]")
}

impl fmt::Display for TranslateUnit {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    return print_tu(&self, f, &"".to_string());
  }
}

fn print_decl(decl: &Decl, f: &mut fmt::Formatter, indent: &String) -> fmt::Result {
  match decl {
    Decl::Func(func) => print_func(func, f, indent)
  }
}

impl fmt::Display for Decl {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    return print_decl(&self, f, &"".to_string());
  }
}

fn print_func(func: &FuncDecl, f: &mut fmt::Formatter, indent: &String) -> fmt::Result {
  write!(f, "{}Function\n", indent).unwrap();
  write!(f, "{}|->Var=", indent).unwrap();
  print_var(&func.var, f, &format!("{}|  ", indent)).unwrap();
  write!(f, "\n{}`->Body=", indent).unwrap();
  print_compound_stmt(&func.body, f, &format!("{}   ", indent))
}

fn print_compound_stmt(stmts: &CompoundStmt, f: &mut fmt::Formatter, indent: &String) -> fmt::Result {
  write!(f, "CompoundStmt\n").unwrap();
  if stmts.stmts.len() != 0 {
    let mut res : fmt::Result = Ok(());
    for (i, elem) in stmts.stmts.iter().enumerate() {
      if i == stmts.stmts.len() - 1 {
        write!(f, "{}`->", indent).unwrap();
         res = print_stmt(&elem, f, &format!("{}   ", indent))
      } else {
        write!(f, "{}|->", indent).unwrap();
         res = print_stmt(&elem, f, &format!("{}|  ", indent))
      }
    }
    res
  } else {
    write!(f, "[Empty Compound Statement]")
  }
}

fn print_stmt(stmt: &Stmt, f: &mut fmt::Formatter, indent: &String) -> fmt::Result {
  match stmt {
    Stmt::Ret(ret) => print_ret(&ret, f, &format!("{}  ", indent))
  }
}

fn print_ret(ret: &ReturnStmt, f: &mut fmt::Formatter, indent: &String) -> fmt::Result {
  write!(f, "ReturnStmt").unwrap();
  match &ret.value {
    Some(x) => {
      write!(f, "\n{}`->Value=", indent).unwrap();
      print_expr(&x, f, indent)
    }
    None => {
      write!(f, " [No Value]")
    }
  }
}

fn print_expr(expr: &Expr, f: &mut fmt::Formatter, _indent: &String) -> fmt::Result {
  match &expr {
    Expr::IntImm(x) => {
      write!(f, "IntImm {}", x.token.literal)
    }
  }
}

fn print_var(var: &Variable, f: &mut fmt::Formatter, indent: &String) -> fmt::Result {
  write!(f, "Variable {}\n", var.token).unwrap();
  write!(f, "{}|->Type=", indent).unwrap();
  print_type(&var.ty, f, &format!("{} {}", indent, "| ")).unwrap();
  write!(f, "\n{}`->Name={}", indent, var.id())
}

fn print_type(ty: &Type, f: &mut fmt::Formatter, indent: &String) -> fmt::Result {
  match ty {
    Type::Builtin(underlying) => { print_builtin_type(underlying, f, indent) }
  }
}

fn print_builtin_type(ty: &BuiltinType, f: &mut fmt::Formatter, _: &String) -> fmt::Result{
  write!(f, "BuiltinType{}", ty.token)
}

