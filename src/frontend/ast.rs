use super::lexer::{Lexer, Token, TokenValue};

pub struct Parser {
  tokenizer: Lexer,
}

pub enum Decl {
  Func(FuncDecl)
}

pub struct TranslateUnit {
  decl: Vec<Decl>
}

pub enum BuiltinTypeCode {
  Void,
  Char,
  Int,
  Unknown
}

pub enum Type {
  Builtin(BuiltinType),
  Class,
}

pub struct BuiltinType {
  pub token: Token,
  pub code: BuiltinTypeCode
}

pub struct Variable {
  pub ty: Type,
  pub token: Token
}

pub struct FuncDecl {
  pub var: Variable,
  pub body: CompoundStmt
}

pub struct CompoundStmt {
  left: Token, // Left braces
  right: Token, // Right braces
  stmts: Vec<Stmt>
}

impl CompoundStmt {
  pub fn new(left: Token, right: Token, stmts: Vec<Stmt>) -> Self {
    CompoundStmt {
      left, right, stmts
    }
  }
}

pub enum Stmt {
  Ret(ReturnStmt)
}

pub struct ReturnStmt {
  pub token: Token, // The "return" keyword
  pub value: Option<Expr>
}

pub enum Expr {
  IntImm(IntImm)
}

pub struct IntImm {
  pub token: Token, // The token this value derived
}

