use std::boxed::Box;

use super::lexer::Token;
use super::sema::SymbolTable;

#[derive(Clone)]
pub struct Linkage {
  pub tus: Vec<Box<TranslateUnit>>,
  pub symbols: Box<SymbolTable>,
}

#[derive(Clone)]
pub struct TranslateUnit {
  pub fname: String,
  pub decls: Vec<Decl>,
}

#[derive(Clone)]
pub enum Decl {
  Func(Box<FuncDecl>),
  Class(Box<ClassDecl>)
}

#[derive(Clone)]
pub struct ClassDecl {
  pub id: Token,
  pub methods: Vec<Box<FuncDecl>>,
  pub attrs: Vec<Box<VarDecl>>,
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
pub enum Type {
  Builtin(Box<BuiltinType>),
  Array(Box<ArrayType>),
  Class(Box<ClassDecl>),
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
  pub decl: Option<Box<VarDecl>>
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
  pub args: Vec<Box<VarDecl>>,
  pub body: Box<CompoundStmt>
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
  pub symbols: Box<SymbolTable>,
}

#[derive(Clone)]
pub enum Stmt {
  Ret(Box<ReturnStmt>),
  FuncCall(Box<FuncCall>),
  InlineAsm(Box<InlineAsm>),
}

#[derive(Clone)]
pub struct InlineAsm {
  pub code: Box<StrImm>,
  pub args: Vec<Expr>,
  pub operands: Box<StrImm>,
}

#[derive(Clone)]
pub struct ReturnStmt {
  pub token: Token, // The "return" keyword
  pub value: Option<Expr>
}

#[derive(Clone)]
pub enum Expr {
  StrImm(Box<StrImm>),
  IntImm(Box<IntImm>),
  FuncCall(Box<FuncCall>),
  Variable(Box<Variable>),
  BinaryOp(Box<BinaryOp>),
  UnknownRef(Token)
}

#[derive(Clone)]
pub struct BinaryOp {
  pub lhs: Expr,
  pub rhs: Expr,
  pub op: Token,
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
  pub func : Option<Box<FuncDecl>>,
  pub params : Vec<Expr>,
}
