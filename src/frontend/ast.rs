use super::lexer::Token;

pub enum Decl {
  Func(FuncDecl)
}

pub struct TranslateUnit {
  pub decls: Vec<Decl>
}

pub enum BuiltinTypeCode {
  Void,
  Char,
  Int,
  Unknown
}

pub enum Type {
  Builtin(BuiltinType),
}

pub struct BuiltinType {
  pub token: Token,
  pub code: BuiltinTypeCode
}

pub struct Variable {
  pub ty: Type,
  pub token: Token
}

impl Variable {
  pub fn id(&self) -> &String {
    &self.token.literal
  }
}

pub struct FuncDecl {
  pub var: Variable,
  pub body: CompoundStmt
}

pub struct CompoundStmt {
  pub left: Token, // Left braces
  pub right: Token, // Right braces
  pub stmts: Vec<Stmt>
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

