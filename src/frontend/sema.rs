use std::rc::Rc;
use std::collections::HashMap;

use crate::mutated;

use super::ast::{
  FuncDecl, VarDecl, Linkage, TranslateUnit, Decl, ClassDecl, Type, Stmt, CompoundStmt,
  Expr, Variable, BinaryOp, ClassRef, AttrAccess, BuiltinTypeCode, FuncCall, ForStmt
};
use super::visitor::{Visitor, expr_eq, stmt_eq, type_eq};
use super::lexer::{Token, TokenType};

#[derive(Clone)]
pub enum WithID {
  Variable(Rc<VarDecl>),
  Function(Rc<FuncDecl>),
  Class(Rc<ClassDecl>),
}

#[derive(Clone)]
pub struct SymbolTable {
  symbols: HashMap<String, WithID>,
}

impl SymbolTable {

  pub fn insert(&mut self, id: String, instance: WithID) {
    if let None = self.get(&id) {
      self.symbols.insert(id, instance);
    } else {
      panic!("Symbol {} already defined", id);
    }
  }

  pub fn get(&self, id: &String) -> Option<&WithID> {
    self.symbols.get(id)
  }

  pub fn new() -> SymbolTable {
    SymbolTable{symbols: HashMap::new()}
  }

}

pub struct ScopeStack {
  scopes: Vec<SymbolTable>,
}

impl ScopeStack {

  pub fn new() -> Self {
    ScopeStack{ scopes: Vec::new() }
  }

  pub fn push(&mut self, scope: SymbolTable) {
    self.scopes.push(scope);
  }

  pub fn insert(&mut self, id: String, instance: WithID) {
    if let Some(last) = self.scopes.last_mut() {
      last.insert(id, instance);
    }
  }

  pub fn pop(&mut self) -> Option<SymbolTable> {
    self.scopes.pop()
  }

  pub fn find(&self, id: &String) -> Option<&WithID> {
    for scope in self.scopes.iter().rev() {
      if let Some(instance) = scope.get(id) {
        return Some(instance);
      }
    }
    None
  }

}

#[macro_export]
macro_rules! find_in_scope {
  ($scope:expr, $id:expr, $ty:pat => $result:expr) => {
    $scope.find($id).and_then(|instance| {
      match instance {
        $ty => { $result }
        _ => None
      }
    })
  };
}

struct MethodHoister {
  to_add : Vec<Decl>,
  under_class: Option<Rc<ClassDecl>>
}

impl Visitor for MethodHoister {

  fn visit_tu(&mut self, tu: &Rc<TranslateUnit>) -> Rc<TranslateUnit> {
    let mut new_decls = Vec::new();
    for decl in tu.decls.iter() {
      match decl {
        Decl::Class(class) => {
          new_decls.push(Decl::Class(self.visit_class(class)));
        }
        Decl::Func(func) => {
          new_decls.push(Decl::Func(func.clone()));
        }
      }
    }
    new_decls.extend(self.to_add.clone());
    self.to_add.clear();
    Rc::new(TranslateUnit{
      fname: tu.fname.clone(),
      decls: new_decls,
    })
  }

  fn visit_class(&mut self, class: &Rc<ClassDecl>) -> Rc<ClassDecl> {
    self.under_class = Some(class.clone());
    for method in class.methods.iter() {
      let hoist = Decl::Func(self.visit_func(method));
      self.to_add.push(hoist);
    }
    self.under_class = None;
    Rc::new(ClassDecl{
      id: class.id.clone(),
      methods: Vec::new(),
      attrs: class.attrs.clone(),
    })
  }

  fn visit_func(&mut self, method: &Rc<FuncDecl>) -> Rc<FuncDecl> {
    let mut new_args = method.args.clone();
    if let Some(ty) = self.under_class.clone() {
      let ty_ref = ClassRef{
        id: ty.id.clone(),
        class: None
      };
      new_args.insert(0, Rc::new(VarDecl{
        ty: Type::Class(Rc::new(ty_ref)),
        id: Token{
          row: ty.id.row,
          col: ty.id.col,
          literal: "self".to_string(),
          value: TokenType::Identifier,
        },
        init: None,
      }));
      let func = FuncDecl{
        ty: method.ty.clone(),
        id: Token{
          row: method.id.row,
          col: method.id.col,
          literal: format!("{}::{}", ty.id.literal, method.id.literal).to_string(),
          value: method.id.value.clone(),
        },
        args: new_args,
        body: method.body.clone(),
      };
      Rc::new(func)
    } else {
      panic!("Method {} outside a class", method.id.literal)
    }
  }

}

/// Hoist methods, and gather global symbols
pub fn hoist_methods(ast: &Rc<Linkage>) -> Rc<Linkage> {
  MethodHoister {
    to_add: Vec::new(),
    under_class: None
  }.visit_linkage(ast)
}

struct SymbolResolver {
  pub(super) scopes: ScopeStack,
  new_scope: bool,
  check_func_sig: bool,
  main: bool,
}


impl Visitor for SymbolResolver {

  fn visit_linkage(&mut self, linkage: &Rc<Linkage>) -> Rc<Linkage> {
    self.scopes.push(SymbolTable::new());
    for tu in linkage.tus.iter() {
      tu.decls.iter().for_each(|decl| {
        match decl {
          Decl::Class(class) => {
            self.scopes.insert(class.id.literal.clone(), WithID::Class(class.clone()));
          }
          Decl::Func(func) => {
            self.scopes.insert(func.id.literal.clone(), WithID::Function(func.clone()));
          }
        }
      });
    }
    let tus:Vec<Rc<TranslateUnit>> = linkage.tus.iter().map(|tu| self.visit_tu(tu)).collect();
    self.scopes.pop().unwrap();
    if mutated!(tus, linkage.tus) {
      return Rc::new(Linkage {
        tus,
      })
    }
    return linkage.clone();
  }

  fn visit_class(&mut self, class:&Rc<ClassDecl>) -> Rc<ClassDecl> {
    assert!(class.methods.len() == 0);
    class.clone()
  }

  fn visit_func(&mut self, func: &Rc<FuncDecl>) -> Rc<FuncDecl> {
    self.main = self.main || func.id.literal == "main";
    self.scopes.push(SymbolTable::new());
    self.new_scope = false;
    let var_decls : Vec<Rc<VarDecl>> = func.args.iter().map(|arg| self.visit_var_decl(arg)).collect();
    let body = self.visit_compound_stmt(&func.body);
    let new_ty = self.visit_type(&func.ty);
    self.scopes.pop();
    if Rc::ptr_eq(&body, &func.body) && !mutated!(var_decls, func.args) && type_eq(&new_ty, &func.ty) {
      return func.clone();
    }
    Rc::new(FuncDecl{
      ty: new_ty,
      id: func.id.clone(),
      args: var_decls.clone(),
      body,
    })
  }

  fn visit_for_stmt(&mut self, x: &Rc<super::ast::ForStmt>) -> Rc<super::ast::ForStmt> {
    self.scopes.push(SymbolTable::new());
    let var = self.visit_var_decl(&x.var);
    let end = self.visit_expr(&x.end);
    let body = self.visit_compound_stmt(&x.body);
    self.scopes.pop();
    Rc::new(ForStmt{ var, end, body })
  }

  fn visit_var_decl(&mut self, var: &Rc<VarDecl>) -> Rc<VarDecl> {
    let ty = self.visit_type(&var.ty);
    let init = match &var.init {
      Some(init) => {
        let new_init = self.visit_expr(init);
        let eq = expr_eq(&init, &new_init);
        (Some(new_init), eq)
      }
      None => {
        (None, true)
      }
    };
    let res = if type_eq(&ty, &var.ty) && init.1 {
      var.clone()
    } else {
      Rc::new(VarDecl{
        ty,
        id: var.id.clone(),
        init: init.0,
      })
    };
    self.scopes.insert(var.id().clone(), WithID::Variable(res.clone()));
    return res;
  }

  fn visit_compound_stmt(&mut self, block: &Rc<super::ast::CompoundStmt>) -> Rc<super::ast::CompoundStmt> {
    let pushed = if self.new_scope {
      self.scopes.push(SymbolTable::new());
      self.new_scope = false;
      true
    } else {
      false
    };
    let stmts:Vec<Stmt> = block.stmts.iter().map(|stmt| { self.visit_stmt(stmt) }).collect();
    if !mutated!(stmts, block.stmts, stmt_eq) {
      return block.clone();
    }
    if pushed {
      self.scopes.pop().unwrap();
    }
    Rc::new(CompoundStmt {
      left: block.left.clone(),
      right: block.right.clone(),
      stmts
    })
  }

  fn visit_expr(&mut self, expr: &super::ast::Expr) -> super::ast::Expr {
    match expr {
      Expr::UnknownRef(x) => {
        if let Some(with_id) = self.scopes.find(&x.literal) {
          match with_id {
            WithID::Variable(decl) => {
              return Expr::Variable(Rc::new(Variable{ id: x.clone(), decl: decl.clone() }));
            }
            _ => {
              panic!("Expect {} to be a variable", x);
            }
          }
        } else {
          panic!("Unknown reference to {}", x);
        }
      }
      Expr::IntImm(imm) => self.visit_intimm(imm),
      Expr::StrImm(s) => Expr::StrImm(s.clone()),
      Expr::FuncCall(call) => Expr::FuncCall(self.visit_func_call(call)),
      Expr::Variable(var) => self.visit_var(var),
      Expr::BinaryOp(op) => self.visit_binary_op(op),
      Expr::AttrAccess(_) => expr.clone(),
      Expr::ArrayIndex(array_idx) => self.visit_array_index(array_idx),
      Expr::NewExpr(ne) => self.visit_new_expr(ne),
      Expr::Cast(cast) => self.visit_cast(cast)
    }
  }

  fn visit_binary_op(&mut self, op: &Rc<BinaryOp>) -> Expr {
    match &op.op.value {
      TokenType::AttrAccess => {
        let lhs = self.visit_expr(&op.lhs);
        match &op.rhs {
          Expr::UnknownRef(attr) => {
            let ty = lhs.dtype(&self.scopes).as_class(&self.scopes);
            if let Some(class) = ty {
              for (i, elem) in class.attrs.iter().enumerate() {
                if elem.id.literal == attr.literal {
                  return Expr::AttrAccess(Rc::new(AttrAccess{
                    this: lhs,
                    attr: attr.clone(), idx: i
                  }));
                }
              }
              panic!("{} not founded", attr);
            }
            panic!("Expect {} to be a class, but {}", lhs, lhs.dtype(&self.scopes));
          }
          Expr::FuncCall(call) => {
            let ty = lhs.dtype(&self.scopes);
            if let Type::Class(class) = ty {
              let callee = format!("{}::{}", class.id(), call.fname.literal);
              if let WithID::Function(_) = self.scopes.find(&callee).unwrap() {
                let mut params = call.params.clone();
                params.insert(0, lhs);
                let fname = Token{
                  literal: callee, row:0, col: 0,
                  value: TokenType::Identifier
                };
                return Expr::FuncCall(Rc::new(FuncCall{fname, params}));
              }
              panic!("{} not founded as a function", call.fname);
            } else {
              panic!("Expect {} to be a class, but {}", lhs, lhs.dtype(&self.scopes));
            }
          }
          _ => {
            panic!("Expect {} to be an class attribute", op.rhs);
          }
        }
      }
      _ => {
        let lhs = self.visit_expr(&op.lhs);
        let rhs = self.visit_expr(&op.rhs);
        if expr_eq(&lhs, &op.lhs) && expr_eq(&rhs, &op.rhs) {
          return Expr::BinaryOp(op.clone());
        }
        return Expr::BinaryOp(Rc::new(BinaryOp{ lhs, rhs, op: op.op.clone() }))
      }
    }
  }

  fn visit_func_call(&mut self, call: &Rc<super::ast::FuncCall>) -> Rc<super::ast::FuncCall> {
    let params:Vec<Expr> = call.params.iter().map(|arg| self.visit_expr(arg)).collect();
    let func = find_in_scope!(self.scopes, &call.fname.literal, WithID::Function(func) => Some(func.clone()));
    if let Some(callee) = &func {
      if callee.args.len() != params.len() {
        panic!("Expect {} args, but got {}", callee.args.len(), params.len());
      }
      if self.check_func_sig {
        for (i, (arg, param)) in Iterator::zip(callee.args.iter(), params.iter()).enumerate() {
          if !type_eq(&arg.ty, &param.dtype(&self.scopes)) {
            panic!("Expect argument {} to be {}, but {}", i, arg.ty, param.dtype(&self.scopes));
          }
        }
      }
    } else {
      panic!("Unknown function {}", call.fname.literal);
    }
    if !mutated!(params, call.params, expr_eq) {
      return call.clone();
    }
    Rc::new(FuncCall{
      fname: call.fname.clone(),
      params
    })
  }

  fn visit_builtin(&mut self, x: &Rc<super::ast::BuiltinType>) -> Type {
    match &x.code {
      BuiltinTypeCode::Unknown => {
        let dtype = self.scopes.find(&x.token.literal);
        let dtype = if let Some(dtype) = dtype {
          dtype
        } else {
          panic!("Unknown type {}", x.token);
        };
        if let WithID::Class(class) = dtype {
          return Type::Class(Rc::new(ClassRef {
            id: x.token.clone(),
            class: Some(class.clone())
          }));
        } else {
          panic!("Expect {} to be a class", x.token);
        }
      }
      _ => { Type::Builtin(x.clone()) }
    }
  }

}


pub fn resolve_symbols(ast: &Rc<Linkage>, check_func_sig:bool) -> Result<Rc<Linkage>, String> {
  let mut resolver = SymbolResolver{
    scopes: ScopeStack::new(),
    new_scope: true,
    check_func_sig,
    main: false
  };
  let res = resolver.visit_linkage(ast);
  if resolver.main {
    return Ok(res);
  }
  return Err("No main function found".to_string());
}

