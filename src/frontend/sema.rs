use std::rc::Rc;
use std::collections::HashMap;

use crate::mutated;

use super::ast::{
  FuncDecl, VarDecl, Linkage, TranslateUnit, Decl, ClassDecl, Type, Stmt, CompoundStmt,
  Expr, Variable, BinaryOp, ClassRef, AttrAccess, BuiltinTypeCode, FuncCall
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
  scopes: Vec<Rc<SymbolTable>>,
}

impl ScopeStack {

  pub fn new() -> Self {
    ScopeStack{ scopes: Vec::new() }
  }

  pub fn push(&mut self, scope: Rc<SymbolTable>) {
    self.scopes.push(scope);
  }

  // pub fn insert(&mut self, id: String, instance: WithID) {
  //   if let Some(last) = self.scopes.last_mut() {
  //     if let Some(scope) = Rc::get_mut(last) {
  //       scope.insert(id, instance);
  //     }
  //   }
  // }

  pub fn pop(&mut self) -> Option<Rc<SymbolTable>> {
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
          literal: "this".to_string(),
          value: TokenType::Identifier,
        },
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

// Hoist methods, and gather global symbols
pub fn hoist_methods(ast: &Rc<Linkage>) -> Rc<Linkage> {
  MethodHoister {
    to_add: Vec::new(),
    under_class: None
  }.visit_linkage(ast)
}

struct SymbolResolver {
  scopes: ScopeStack,
  var_decls: Vec<Rc<VarDecl>>,
  check_func_sig: bool,
}


impl Visitor for SymbolResolver {

  fn visit_linkage(&mut self, linkage: &Rc<Linkage>) -> Rc<Linkage> {
    let mut symbols = SymbolTable::new();
    for tu in linkage.tus.iter() {
      tu.decls.iter().for_each(|decl| {
        match decl {
          Decl::Class(class) => {
            symbols.insert(class.id.literal.clone(), WithID::Class(class.clone()));
          }
          Decl::Func(func) => {
            symbols.insert(func.id.literal.clone(), WithID::Function(func.clone()));
          }
        }
      });
    }
    self.scopes.push(Rc::new(symbols));
    let tus:Vec<Rc<TranslateUnit>> = linkage.tus.iter().map(|tu| self.visit_tu(tu)).collect();
    self.scopes.pop().unwrap();
    if mutated!(tus, linkage.tus) {
      return Rc::new(Linkage {
        tus,
      })
    }
    return linkage.clone();
  }

  fn visit_func(&mut self, func: &Rc<FuncDecl>) -> Rc<FuncDecl> {
    self.var_decls = func.args.iter().map(|arg| self.visit_var_decl(arg)).collect();
    let body = self.visit_compound_stmt(&func.body);
    let new_ty = self.visit_type(&func.ty);
    if Rc::ptr_eq(&body, &func.body) && !mutated!(self.var_decls, func.args) && type_eq(&new_ty, &func.ty) {
      return func.clone();
    }
    Rc::new(FuncDecl{
      ty: new_ty,
      id: func.id.clone(),
      args: self.var_decls.clone(),
      body,
    })
  }

  fn visit_compound_stmt(&mut self, block: &Rc<super::ast::CompoundStmt>) -> Rc<super::ast::CompoundStmt> {
    let mut symbols = SymbolTable::new();
    self.var_decls.iter().for_each(|decl| {
      symbols.insert(decl.id.literal.clone(), WithID::Variable(decl.clone()));
    });
    self.scopes.push(Rc::new(symbols));
    let stmts:Vec<Stmt> = block.stmts.iter().map(|stmt| { self.visit_stmt(stmt) }).collect();
    if !mutated!(stmts, block.stmts, stmt_eq) {
      return block.clone();
    }
    self.scopes.pop().unwrap();
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
              return Expr::Variable(Rc::new(Variable{
                id: x.clone(),
                decl: decl.clone()
              }));
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
        let dtype = self.scopes.find(&x.token.literal).unwrap();
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


pub fn resolve_symbols(ast: &Rc<Linkage>, check_func_sig:bool) -> Rc<Linkage> {
  SymbolResolver{
    scopes: ScopeStack::new(),
    var_decls: Vec::new(),
    check_func_sig
  }.visit_linkage(ast)
}

