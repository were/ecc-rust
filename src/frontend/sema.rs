use std::collections::HashMap;

use super::{ast::{FuncDecl, VarDecl, Linkage, TranslateUnit, Decl, ClassDecl, Type, Variable, FuncCall}, visitor::Visitor};
use super::lexer::Token;

#[derive(Clone)]
pub enum WithID {
  Variable(Box<VarDecl>),
  Function(Box<FuncDecl>),
  Class(Box<ClassDecl>),
}

#[derive(Clone)]
pub struct SymbolTable {
  symbols: HashMap<String, WithID>,
}

impl SymbolTable {

  pub fn insert(&mut self, id: String, instance: WithID) {
    self.symbols.insert(id, instance);
  }

  pub fn get(&self, id: &String) -> Option<&WithID> {
    self.symbols.get(id)
  }

  pub fn new() -> SymbolTable {
    SymbolTable{symbols: HashMap::new()}
  }

}

pub struct ScopeStack {
  scopes: Vec<Box<SymbolTable>>,
}

impl ScopeStack {

  pub fn new() -> Self {
    ScopeStack{ scopes: Vec::new() }
  }

  pub fn push(&mut self, scope: Box<SymbolTable>) {
    self.scopes.push(scope);
  }

  pub fn pop(&mut self) -> Option<Box<SymbolTable>> {
    self.scopes.pop()
  }

  pub fn insert(&mut self, id: String, instance: WithID) {
    self.scopes.last_mut().unwrap().insert(id, instance);
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

// Hoist methods, and gather global symbols
pub fn hoist_methods(ast: &mut Linkage) {
  for tu in ast.tus.iter_mut() {
    let mut to_add : Vec<Box<FuncDecl>> = Vec::new();
    for class in tu.decls.iter_mut() {
      match class {
        Decl::Class(class) => {
          ast.symbols.insert(class.id.literal.clone(), WithID::Class(class.clone()));
          for method in class.methods.iter() {
            let mut new_args = method.args.clone();
            new_args.insert(0, Box::new(VarDecl{
              ty: Type::Class(class.clone()),
              id: Token{
                row: class.id.row,
                col: class.id.col,
                literal: "self".to_string(),
                value: super::lexer::TokenType::Identifier,
              },
            }));
            let func = FuncDecl{
              ty: method.ty.clone(),
              id: Token{
                row: method.id.row,
                col: method.id.col,
                literal: format!("{}::{}", class.id.literal, method.id.literal).to_string(),
                value: method.id.value.clone(),
              },
              args: new_args,
              body: method.body.clone(),
            };
            to_add.push(Box::new(func))
          }
        }
        Decl::Func(func) => {
          ast.symbols.insert(func.id.literal.clone(), WithID::Function(func.clone()));
        }
      }
    }
    for method in to_add.iter() {
      tu.decls.push(Decl::Func(method.clone()));
    }
  }
}

struct SymbolResolver {
  scopes: ScopeStack,
}


impl Visitor for SymbolResolver {

  fn visit_linkage(&mut self, linkage: &mut Linkage) {
    let scope : Box<SymbolTable> = linkage.symbols.clone();
    self.scopes.push(scope);
    for unit in linkage.tus.iter_mut() {
      self.visit_tu(unit);
    }
  }

  fn visit_func_call(&mut self, call: &Box<FuncCall>) -> Box<FuncCall> {
    call.clone()
  }

}


pub fn resolve_types(ast: &mut Linkage) {
  SymbolResolver{ scopes: ScopeStack::new() }.visit_linkage(ast);
}
