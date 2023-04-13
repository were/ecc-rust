use std::fmt;
use super::ast::{
  Decl, FuncDecl, Variable, Type, BuiltinType, CompoundStmt, Stmt,
  ReturnStmt, Expr, TranslateUnit, Linkage, FuncCall, VarDecl,
  ClassDecl, ArrayType, InlineAsm, StrImm, BinaryOp, AttrAccess,
  BuiltinTypeCode
};

fn print_linkage(linkage: &Linkage, f: &mut fmt::Formatter, indent: &String) -> fmt::Result {
  for (i, tu) in linkage.tus.iter().enumerate() {
    print_tu(tu, f, indent).unwrap();
    if i != linkage.tus.len() - 1 {
      write!(f, "\n").unwrap();
    }
  }
  Ok(())
}

impl fmt::Display for Linkage {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    return print_linkage(&self, f, &"".to_string());
  }
}

fn print_tu(tu: &TranslateUnit, f: &mut fmt::Formatter, indent: &String) -> fmt::Result {
  if tu.decls.len() != 0 {
    write!(f, "File={} 0x{:x}\n", tu.fname, tu as *const TranslateUnit as usize).unwrap();
    for (i, elem) in tu.decls.iter().enumerate() {
      if i == tu.decls.len() - 1 {
        write!(f, "{}`->", indent).unwrap();
        print_decl(elem, f, &format!("{}   ", indent)).unwrap();
      } else {
        write!(f, "{}|->", indent).unwrap();
        print_decl(elem, f, &format!("{}|  ", indent)).unwrap();
        write!(f, "\n").unwrap();
      }
    }
    Ok(())
  } else {
    write!(f, "[Empty Program]")
  }
}

impl fmt::Display for TranslateUnit {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    return print_tu(&self, f, &"".to_string());
  }
}

fn print_decl(decl: &Decl, f: &mut fmt::Formatter, indent: &String) -> fmt::Result {
  match decl {
    Decl::Func(func) => print_func(func, f, indent),
    Decl::Class(class) => print_class(class, f, indent),
  }
}

fn print_class(class: &ClassDecl, f: &mut fmt::Formatter, indent: &String) -> fmt::Result {
  write!(f, "ClassDecl 0x{:x}\n", class as *const ClassDecl as usize).unwrap();
  write!(f, "{}|->Name={}\n", indent, class.id).unwrap();
  let total = class.methods.len() + class.attrs.len();
  for (i, elem) in class.methods.iter().enumerate() {
    if i == total - 1 {
      write!(f, "{}`->Method_{}=", indent, i).unwrap();
      print_func(elem, f, &format!("{}   ", indent)).unwrap();
    } else {
      write!(f, "{}|->Method_{}=", indent, i).unwrap();
      print_func(elem, f, &format!("{}|  ", indent)).unwrap();
      write!(f, "\n").unwrap();
    }
  }
  for (i, elem) in class.attrs.iter().enumerate() {
    if i + class.methods.len() == total - 1 {
      write!(f, "{}`->Attr_{}=", indent, i).unwrap();
      print_var_decl(elem, f, &format!("{}   ", indent)).unwrap();
    } else {
      write!(f, "{}|->Attr_{}=", indent, i).unwrap();
      print_var_decl(elem, f, &format!("{}|  ", indent)).unwrap();
      write!(f, "\n").unwrap();
    }
  }
  Ok(())
}


impl fmt::Display for Decl {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    return print_decl(&self, f, &"".to_string());
  }
}

fn print_var_decl(var: &VarDecl, f: &mut fmt::Formatter, indent: &String) -> fmt::Result {
  write!(f, "VarDecl 0x{:x}", var as *const VarDecl as usize).unwrap();
  write!(f, "\n{}|->Name={}", indent, var.id).unwrap();
  write!(f, "\n{}`->Type=", indent).unwrap();
  print_type(&var.ty, f, &format!("{}   ", indent))
}

fn print_func(func: &FuncDecl, f: &mut fmt::Formatter, indent: &String) -> fmt::Result {
  write!(f, "Function 0x{:x}\n", func as *const FuncDecl as usize).unwrap();
  write!(f, "{}|->Name={}", indent, func.id.literal).unwrap();
  write!(f, "\n{}|->Args", indent).unwrap();
  for (i, elem) in func.args.iter().enumerate() {
    let new_indent = format!("{}|  ", indent);
    if i != func.args.len() - 1 {
      write!(f, "\n{}|->Arg_{}=", new_indent, i).unwrap();
    } else {
      write!(f, "\n{}`->Arg_{}=", new_indent, i).unwrap();
    }
    print_var_decl(elem, f, &format!("{}   ", new_indent)).unwrap();
  }
  write!(f, "\n{}`->Body=", indent).unwrap();
  print_compound_stmt(&func.body, f, &format!("{}   ", indent))
}

fn print_compound_stmt(stmts: &CompoundStmt, f: &mut fmt::Formatter, indent: &String) -> fmt::Result {
  write!(f, "CompoundStmt").unwrap();
  if stmts.stmts.len() != 0 {
    let mut res : fmt::Result = Ok(());
    for (i, elem) in stmts.stmts.iter().enumerate() {
      if i == stmts.stmts.len() - 1 {
        write!(f, "\n{}`->", indent).unwrap();
         res = print_stmt(&elem, f, &format!("{}   ", indent))
      } else {
        write!(f, "\n{}|->", indent).unwrap();
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
    Stmt::Ret(ret) => print_ret(&ret, f, &format!("{}", indent)),
    Stmt::FuncCall(call) => print_func_call(&call, f, &format!("{}", indent)),
    Stmt::InlineAsm(asm) => print_inline_asm(&asm, f, &format!("{}", indent)),
  }
}

fn print_inline_asm(asm: &InlineAsm, f: &mut fmt::Formatter, indent: &String) -> fmt::Result {
  write!(f, "InlineAsm").unwrap();
  write!(f, "\n{}|->Code=", indent).unwrap();
  print_str_imm(&asm.code, f, indent).unwrap();
  write!(f, "\n{}`->Args", indent).unwrap();
  let new_indent = format!("{}   ", indent);
  for (i, arg) in asm.args.iter().enumerate() {
    if i == asm.args.len() - 1 {
      write!(f, "\n{}`->Arg_{}=", new_indent, i).unwrap();
      print_expr(&arg, f, &format!("{}   ", new_indent)).unwrap();
    } else {
      write!(f, "\n{}|->Arg_{}=", new_indent, i).unwrap();
      print_expr(&arg, f, &format!("{}|  ", new_indent)).unwrap();
    }
  }
  Ok(())
}

fn print_func_call(call: &FuncCall, f: &mut fmt::Formatter, indent: &String) -> fmt::Result {
  write!(f, "Call={}\n{}`->Params\n", call.fname.literal, indent).unwrap();
  let new_indent = format!("{}   ", indent);
  for (i, elem) in call.params.iter().enumerate() {
    if i != call.params.len() - 1 {
      write!(f, "{}|->Param_{}=", new_indent, i).unwrap();
      write!(f, "\n").unwrap();
    } else {
      write!(f, "{}`->Param_{}=", new_indent, i).unwrap();
    }
    print_expr(&elem, f, &format!("{}   ", new_indent)).unwrap();
  }
  Ok(())
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

fn print_expr(expr: &Expr, f: &mut fmt::Formatter, indent: &String) -> fmt::Result {
  match &expr {
    Expr::IntImm(x) => {
      write!(f, "IntImm {}", x.token.literal)
    }
    Expr::StrImm(s) => {
      print_str_imm(s, f, indent)
    }
    Expr::FuncCall(call) => {
      print_func_call(call, f, indent)
    }
    Expr::UnknownRef(tok) => {
      write!(f, "UnknownRef {}", tok.literal)
    }
    Expr::Variable(var) => {
      print_var(var, f, indent)
    }
    Expr::BinaryOp(op) => {
      print_binary_op(op, f, indent)
    }
    Expr::AttrAccess(access) => {
      print_attr_access(access, f, indent)
    }
  }
}

impl fmt::Display for Expr {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    return print_expr(&self, f, &"".to_string());
  }
}

fn print_attr_access(access: &AttrAccess, f: &mut fmt::Formatter, indent: &String) -> fmt::Result {
  write!(f, "AttrAccess").unwrap();
  write!(f, "\n{}|->This=", indent).unwrap();
  print_expr(&access.this, f, &format!("{}|  ", indent)).unwrap();
  write!(f, "\n{}`->Attr={} ({})", indent, access.attr, access.idx)
}

fn print_binary_op(op: &BinaryOp, f: &mut fmt::Formatter, indent: &String) -> fmt::Result {
  write!(f, "BinaryOp {}", op.op).unwrap();
  write!(f, "\n{}|->A=", indent).unwrap();
  print_expr(&op.lhs, f, &format!("{}|  ", indent)).unwrap();
  write!(f, "\n{}`->B=", indent).unwrap();
  print_expr(&op.rhs, f, &format!("{}   ", indent))
}

fn print_str_imm(s: &StrImm, f: &mut fmt::Formatter, _indent: &String) -> fmt::Result {
  write!(f, "StringImm {}", s.token.literal)
}

fn print_var(var: &Variable, f: &mut fmt::Formatter, indent: &String) -> fmt::Result {
  write!(f, "Variable {} 0x{:x}\n", var.id, (&*var.decl) as *const VarDecl as usize).unwrap();
  write!(f, "{}`->Name={}", indent, var.id())
}

fn print_type(ty: &Type, f: &mut fmt::Formatter, indent: &String) -> fmt::Result {
  match ty {
    Type::Builtin(underlying) => { print_builtin_type(underlying, f, indent) }
    Type::Array(underlying) => { print_array_type(underlying, f, indent) }
    Type::Class(class) => { write!(f, "Class {}", class.id) }
  }
}

impl fmt::Display for Type {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    return print_type(&self, f, &"".to_string());
  }
}

fn print_array_type(ty: &ArrayType, f: &mut fmt::Formatter, indent: &String) -> fmt::Result {
  write!(f, "ArrayType").unwrap();
  write!(f, "\n{}|->Scalar=", indent).unwrap();
  print_type(&ty.scalar_ty, f, &format!("{}|  ", indent)).unwrap();
  write!(f, "\n{}`->Dimension={}", indent, "[]".repeat(ty.dims as usize))
}

fn print_builtin_type(ty: &BuiltinType, f: &mut fmt::Formatter, _: &String) -> fmt::Result{
  if let BuiltinTypeCode::Unknown = ty.code {
    write!(f, "UnknownType{}", ty.token)
  } else {
    write!(f, "BuiltinType{}", ty.token)
  }
}

