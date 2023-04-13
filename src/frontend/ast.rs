use std::rc::Rc;

use crate::find_in_scope;
use crate::frontend::sema::WithID;

use super::lexer::{Token, TokenType};
use super::sema::{SymbolTable, ScopeStack};

#[derive(Clone)]
pub struct Linkage {
  pub tus: Vec<Rc<TranslateUnit>>,
  pub symbols: Rc<SymbolTable>
}

#[derive(Clone)]
pub struct TranslateUnit {
  pub fname: String,
  pub decls: Vec<Decl>,
}

#[derive(Clone)]
pub enum Decl {
  Func(Rc<FuncDecl>),
  Class(Rc<ClassDecl>)
}

#[derive(Clone)]
pub struct ClassDecl {
  pub id: Token,
  pub methods: Vec<Rc<FuncDecl>>,
  pub attrs: Vec<Rc<VarDecl>>,
}

#[derive(Clone)]
pub enum BuiltinTypeCode {
  Int,
  Void,
  Char,
  Bool,
  Unknown,
}

#[derive(Clone)]
pub struct ClassRef {
  pub id: Token,
  pub class: Option<Rc<ClassDecl>>
}

#[derive(Clone)]
pub enum Type {
  Builtin(Rc<BuiltinType>),
  Array(Rc<ArrayType>),
  Class(Rc<ClassRef>),
}

impl Type {

  pub fn as_builtin(&self) -> Option<Rc<BuiltinType>> {
    match self {
      Type::Builtin(b) => Some(b.clone()),
      _ => None
    }
  }

  pub fn as_class(&self, table: &ScopeStack) -> Option<Rc<ClassDecl>> {
    match self {
      Type::Class(class_ref) => {
        find_in_scope!(table, &class_ref.id.literal, WithID::Class(class) => Some(class.clone()))
      }
      _ => None
    }
  }

}

#[derive(Clone)]
pub struct BuiltinType {
  pub token: Token,
  pub code: BuiltinTypeCode
}

#[derive(Clone)]
pub struct ArrayType {
  pub scalar_ty: Type,
  pub dims: i32
}

#[derive(Clone)]
pub struct Variable {
  pub id: Token,
  pub decl: Rc<VarDecl>
}

impl Variable {
  pub fn id(&self) -> &String {
    &self.id.literal
  }
}

#[derive(Clone)]
pub struct FuncDecl {
  pub ty: Type,
  pub id: Token,
  pub args: Vec<Rc<VarDecl>>,
  pub body: Rc<CompoundStmt>,
}

#[derive(Clone)]
pub struct VarDecl {
  pub ty: Type,
  pub id: Token
}

#[derive(Clone)]
pub struct CompoundStmt {
  pub left: Token, // Left braces
  pub right: Token, // Right braces
  pub stmts: Vec<Stmt>,
  pub symbols: Rc<SymbolTable>
}

#[derive(Clone)]
pub enum Stmt {
  Ret(Rc<ReturnStmt>),
  FuncCall(Rc<FuncCall>),
  InlineAsm(Rc<InlineAsm>),
}

#[derive(Clone)]
pub struct InlineAsm {
  pub code: Rc<StrImm>,
  pub args: Vec<Expr>,
  pub operands: Rc<StrImm>,
}

#[derive(Clone)]
pub struct ReturnStmt {
  pub token: Token, // The "return" keyword
  pub value: Option<Expr>
}

#[derive(Clone)]
pub enum Expr {
  StrImm(Rc<StrImm>),
  IntImm(Rc<IntImm>),
  FuncCall(Rc<FuncCall>),
  Variable(Rc<Variable>),
  BinaryOp(Rc<BinaryOp>),
  AttrAccess(Rc<AttrAccess>),
  UnknownRef(Token)
}

impl Expr {

  pub fn dtype(&self, symbols: &ScopeStack) -> Type {
    match self {
      Expr::StrImm(s) => {
        Type::Class(Rc::new(ClassRef {
          id: Token{
            literal: "string".to_string(),
            row: s.token.row,
            col: s.token.col,
            value: TokenType::Identifier,
          },
          class: None
        }))
      }
      Expr::IntImm(i) => {
        Type::Builtin(Rc::new(BuiltinType {
          token: i.token.clone(),
          code: BuiltinTypeCode::Int
        }))
      }
      Expr::FuncCall(f) => {
        if let Some(callee) = find_in_scope!(symbols, &f.fname.literal, WithID::Function(f) => Some(f.clone())) {
          callee.ty.clone()
        } else {
          Type::Builtin(Rc::new(BuiltinType {
            token: f.fname.clone(),
            code: BuiltinTypeCode::Unknown
          }))
        }
      }
      Expr::Variable(v) => {
        v.decl.ty.clone()
      }
      Expr::BinaryOp(s) => {
        s.lhs.dtype(symbols)
      }
      Expr::UnknownRef(tok) => {
        Type::Builtin(Rc::new(BuiltinType {
          token: tok.clone(),
          code: BuiltinTypeCode::Unknown
        }))
      }
      Expr::AttrAccess(access) => {
        if let Type::Class(x) = access.this.dtype(symbols) {
          if let Some(class) = &x.class {
            class.attrs[access.idx as usize].ty.clone()
          } else {
            Type::Builtin(Rc::new(BuiltinType {
              token: access.attr.clone(),
              code: BuiltinTypeCode::Unknown
            }))
          }
        } else {
          panic!("Cannot access attribute of non-class type");
        }
      }
    }
  }

}

#[derive(Clone)]
pub struct BinaryOp {
  pub lhs: Expr,
  pub rhs: Expr,
  pub op: Token,
}

#[derive(Clone)]
pub struct AttrAccess {
  pub this: Expr,
  pub attr: Token,
  pub idx: usize,
}

#[derive(Clone)]
pub struct IntImm {
  pub token: Token, // The token this value derived
  pub value: i32,
}

#[derive(Clone)]
pub struct StrImm {
  pub token: Token, // The token this value derived
  pub value: String,
}

#[derive(Clone)]
pub struct FuncCall {
  pub fname : Token,
  pub params : Vec<Expr>,
}
