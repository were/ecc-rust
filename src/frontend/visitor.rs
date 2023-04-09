use super::ast::{Type, TranslateUnit, BuiltinType, Variable,
                 FuncDecl, CompoundStmt, Stmt, ReturnStmt, IntImm,
                 Decl, Expr, FuncCall, Linkage, VarDecl, ClassDecl, InlineAsm, BinaryOp, ArrayType};

pub trait Visitor {

  fn visit_linkage(&mut self, linkage: &mut Linkage) {
    for elem in linkage.tus.iter_mut() {
      self.visit_tu(elem);
    }
  }

  fn visit_decl(&mut self, decl: &Decl) -> Decl {
    match decl {
      Decl::Func(func) => Decl::Func(self.visit_func(func)),
      Decl::Class(class) => Decl::Class(self.visit_class(class)),
    }
  }

  fn visit_class(&mut self, class: &Box<ClassDecl>) -> Box<ClassDecl> {
    let methods = class.methods.iter().map(|elem| self.visit_func(elem)).collect();
    let attrs = class.attrs.iter().map(|elem| self.visit_var_decl(elem)).collect();
    Box::new(ClassDecl{ id: class.id.clone(), methods, attrs, })
  }

  fn visit_expr(&mut self, expr: &Expr) -> Expr {
    match expr {
      Expr::IntImm(imm) => self.visit_intimm(imm),
      Expr::StrImm(s) => Expr::StrImm(s.clone()),
      Expr::FuncCall(call) => Expr::FuncCall(self.visit_func_call(call)),
      Expr::Variable(var) => self.visit_var(var),
      Expr::BinaryOp(op) => self.visit_binary_op(op),
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

  fn visit_func_call(&mut self, call: &Box<FuncCall>) -> Box<FuncCall> {
    let params : Vec<Expr> = call.params.iter().map(|x| self.visit_expr(x)).collect();
    Box::new(FuncCall{
      fname: call.fname.clone(),
      params,
      func: call.func.clone()
    })
  }

  fn visit_tu(&mut self, tu: &Box<TranslateUnit>) -> Box<TranslateUnit> {
    let decls : Vec<Decl> = tu.decls.iter().map(|x| self.visit_decl(x)).collect();
    Box::new(TranslateUnit{ fname: tu.fname.clone(), decls })
  }

  fn visit_func(&mut self, func: &Box<FuncDecl>) -> Box<FuncDecl> {
    let body = self.visit_compound_stmt(&func.body);
    let args = func.args.iter().map(|x| self.visit_var_decl(x)).collect();
    Box::new(FuncDecl{ id: func.id.clone(), args, body, ty: func.ty.clone() })
  }

  fn visit_var_decl(&mut self, var: &Box<VarDecl>) -> Box<VarDecl> {
    let ty = self.visit_type(&var.ty);
    Box::new(VarDecl{ ty, id: var.id.clone() })
  }

  fn visit_compound_stmt(&mut self, block: &Box<CompoundStmt>) -> Box<CompoundStmt> {
    let stmts = block.stmts.iter().map(|x| self.visit_stmt(x)).collect();
    Box::new(CompoundStmt{
      left: block.left.clone(),
      right: block.right.clone(),
      stmts,
      symbols: block.symbols.clone() })
  }

  fn visit_inline_asm(&mut self, asm: &Box<InlineAsm>) -> Stmt {
    // TODO(@were): Improve the performance later.
    let args : Vec<Expr> = asm.args.iter().map(|x| self.visit_expr(x)).collect();
    Stmt::InlineAsm(Box::new(InlineAsm{ code : asm.code.clone(), args, operands: asm.operands.clone() }))
  }

  fn visit_binary_op(&mut self, op: &Box<BinaryOp>) -> Expr {
    let lhs = self.visit_expr(&op.lhs);
    let rhs = self.visit_expr(&op.rhs);
    Expr::BinaryOp(Box::new(BinaryOp{ lhs, rhs, op: op.op.clone() }))
  }

  fn visit_return(&mut self, x: &Box<ReturnStmt>) -> Stmt {
    Stmt::Ret(x.clone())
  }

  fn visit_builtin(&mut self, x: &Box<BuiltinType>) -> Type {
    Type::Builtin(x.clone())
  }

  fn visit_var(&mut self, x: &Box<Variable>) -> Expr {
    Expr::Variable(x.clone())
  }

  fn visit_intimm(&mut self, x: &Box<IntImm>) -> Expr {
    Expr::IntImm(x.clone()
      )
  }

  fn visit_array_ty(&mut self, x: &Box<ArrayType>) -> Type {
    let ty = self.visit_type(&x.scalar_ty);
    Type::Array(Box::new(ArrayType{ scalar_ty: ty, dims: x.dims }))
  }

}

