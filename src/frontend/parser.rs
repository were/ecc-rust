use quote::quote;
use std::rc::Rc;

use super::lexer::{Lexer, TokenType, Token};
use super::ast::{BuiltinType, BuiltinTypeCode, Type, TranslateUnit, CompoundStmt, ReturnStmt, IntImm, Decl, InlineAsm};
use super::ast::{FuncDecl, Stmt, Expr, StrImm, FuncCall, ClassDecl, VarDecl, ArrayType, BinaryOp};

macro_rules! required_token {
  ($pp: expr, $expected: pat, $consume: expr) => {
    {
      let to_consume = $pp.lookahead(0);
      if ($consume) {
        $pp.consume().unwrap();
      }
      match to_consume.value {
        $expected => { to_consume }
        _=> {
          let pat_str = quote!($expected).to_string();
          return Err(format!("Expect {} but got {} @{}", pat_str, to_consume, line!()))
        }
      }
    }
  };
}

macro_rules! expected_token {
  ($pp: expr, $expected: pat, $consume: expr) => {
    {
      let to_consume = $pp.lookahead(0);
      match to_consume.value {
        $expected => { true }
        _=> { false }
      }
    }
  };
  ($pp: expr, $expected: pat, $consume: expr, $ahead: expr) => {
    {
      let to_consume = $pp.lookahead($ahead);
      match to_consume.value {
        $expected => { true }
        _=> { false }
      }
    }
  };
}

macro_rules! lookahead_tokens_impl {

  ($pp: expr, $ahead: expr, $head:pat, $($rest: pat),*) => {
    expected_token!($pp, $head, false, $ahead) && lookahead_tokens_impl!($pp, $ahead + 1, $($rest),*)
  };

  ($pp: expr, $ahead: expr, $head:pat) => {
    expected_token!($pp, $head, false, $ahead)
  };

}

macro_rules! lookahead_tokens {

  ($pp: expr, $($expected: pat),*) => {
    lookahead_tokens_impl!($pp, 0, $($expected),*)
  };

}

fn parse_intimm(tokenizer: &mut Lexer) -> Result<Expr, String> {
  let token = required_token!(tokenizer, TokenType::IntLiteral, true);
  let value : i32 = token.literal.parse().unwrap();
  Ok(Expr::IntImm(Rc::new(IntImm{token, value })))
}

fn parse_expr_term(tokenizer: &mut Lexer) -> Result<Expr, String> {
  if expected_token!(tokenizer, TokenType::IntLiteral, false) {
    return parse_intimm(tokenizer)
  }
  if expected_token!(tokenizer, TokenType::StringLiteral, false) {
    return parse_strimm(tokenizer)
  }
  if lookahead_tokens!(tokenizer, TokenType::Identifier, TokenType::LPran) {
    return parse_func_call(tokenizer)
  }
  if lookahead_tokens!(tokenizer, TokenType::Identifier) {
    let tok = required_token!(tokenizer, TokenType::Identifier, true);
    return Ok(Expr::UnknownRef(tok))
  }
  let tok = tokenizer.lookahead(0);
  Err(format!("Fail to parse expr {} {}", tok, tok.value))
}

fn parse_attr_access(tokenizer: &mut Lexer) -> Result<Expr, String> {
  let mut expr = parse_expr_term(tokenizer).unwrap();
  while lookahead_tokens!(tokenizer, TokenType::AttrAccess) {
    let op = tokenizer.lookahead(0);
    tokenizer.consume().unwrap();
    let rhs = required_token!(tokenizer, TokenType::Identifier, true);
    expr = Expr::BinaryOp(Rc::new(BinaryOp{op, lhs: expr, rhs: Expr::UnknownRef(rhs)}));
  }
  Ok(expr)
}

fn parse_rvalue(tokenizer: &mut Lexer, terminator: fn(&Token)->bool) -> Result<Expr, String> {
  let expr = parse_attr_access(tokenizer).unwrap();
  if terminator(&tokenizer.lookahead(0)) {
    return Ok(expr);
  }
  return Err("Unexpected the terminator".to_string());
}

fn parse_strimm(tokenizer: &mut Lexer) -> Result<Expr, String> {
  let token = required_token!(tokenizer, TokenType::StringLiteral, true);
  let value = snailquote::unescape(&token.literal[1..token.literal.len()-1]).unwrap();
  Ok(Expr::StrImm(Rc::new(StrImm{token, value})))
}

fn parse_return(tokenizer: &mut Lexer) -> Result<ReturnStmt, String> {
  let token = required_token!(tokenizer, TokenType::KeywordReturn, true);
  if expected_token!(tokenizer, TokenType::Semicolon, false) {
    required_token!(tokenizer, TokenType::Semicolon, true);
    return Ok(ReturnStmt{token, value: None})
  }
  let cond = |tv: &Token| {
    match tv.value {
      TokenType::Semicolon => true,
      _ => false
    }
  };
  let parsed_value = parse_rvalue(tokenizer, cond);
  match parsed_value {
    Ok(value) => {
      required_token!(tokenizer, TokenType::Semicolon, true);
      Ok(ReturnStmt{token, value: Some(value)})
    }
    Err(msg) => Err(msg)
  }
}

fn parse_params(tokenizer: &mut Lexer) -> Result<Vec<Expr>, String> {
  let mut params = Vec::new();
  while !expected_token!(tokenizer, TokenType::RPran, false) {
    let cond = |tv: &Token| {
      match tv.value {
        TokenType::RPran => true,
        TokenType::Comma => true,
        _ => false
      }
    };
    let parsed = parse_rvalue(tokenizer, cond);
    match parsed {
      Ok(expr) => { params.push(expr) }
      Err(msg) => { return Err(msg) }
    }
    if expected_token!(tokenizer, TokenType::Comma, false) {
      required_token!(tokenizer, TokenType::Comma, true);
    }
  }
  Ok(params)
}

fn parse_func_call(tokenizer: &mut Lexer) -> Result<Expr, String> {
  let fid = required_token!(tokenizer, TokenType::Identifier, true);
  required_token!(tokenizer, TokenType::LPran, true);
  let params = parse_params(tokenizer).unwrap();
  required_token!(tokenizer, TokenType::RPran, true);
  Ok(Expr::FuncCall(Rc::new(FuncCall{fname: fid, params})))
}

fn parse_inline_asm(tokenizer: &mut Lexer) -> Result<InlineAsm, String> {
  required_token!(tokenizer, TokenType::KeywordAsm, true);
  required_token!(tokenizer, TokenType::LPran, true);
  let args = parse_params(tokenizer).unwrap();
  required_token!(tokenizer, TokenType::RPran, true);
  required_token!(tokenizer, TokenType::Semicolon, true);
  if let Expr::StrImm(code) = args[0].clone() {
    if let Expr::StrImm(operands) = args[args.len() - 1].clone() {
      Ok(InlineAsm{code, args: args[1..args.len()-1].to_vec(), operands})
    } else {
      Err(format!("Unexpected last arg to be operand format {}", args[args.len() - 1]))
    }
  } else {
    Err(format!("Unexpected 1st arg to be mnemonic {}", args[0]))
  }
}

fn parse_statement(tokenizer: &mut Lexer) -> Result<Stmt, String> {
  if lookahead_tokens!(tokenizer, TokenType::KeywordReturn) {
    let ret = parse_return(tokenizer).unwrap();
    return Ok(Stmt::Ret(Rc::new(ret)));
  }
  if lookahead_tokens!(tokenizer, TokenType::Identifier, TokenType::LPran) {
    if let Expr::FuncCall(call) = parse_func_call(tokenizer).unwrap() {
      required_token!(tokenizer, TokenType::Semicolon, true);
      return Ok(Stmt::FuncCall(call))
    }
  }
  if lookahead_tokens!(tokenizer, TokenType::KeywordAsm) {
    let asm = parse_inline_asm(tokenizer).unwrap();
    return Ok(Stmt::InlineAsm(Rc::new(asm)));
  }
  Err(format!("Failed to parse statement {}", tokenizer.lookahead(0)))
}

fn parse_compound_stmt(tokenizer: &mut Lexer) -> Result<CompoundStmt, String> {
  let left = required_token!(tokenizer, TokenType::LBrace, true);
  let mut stmts : Vec<Stmt> = Vec::new();
  while !expected_token!(tokenizer, TokenType::RBrace, false) {
    match parse_statement(tokenizer) {
      Ok(stmt) => { stmts.push(stmt); }
      Err(msg) => { return Err(msg); }
    }
  }
  let right = required_token!(tokenizer, TokenType::RBrace, true);

  return Ok(CompoundStmt{ left, right, stmts })
}

pub fn parse_args(tokenizer: &mut Lexer) -> Result<Vec<Rc<VarDecl>>, String> {
  let mut res : Vec<Rc<VarDecl>> = Vec::new();
  while !expected_token!(tokenizer, TokenType::RPran, false) {
    let dtype = parse_dtype(tokenizer).unwrap();
    let id = required_token!(tokenizer, TokenType::Identifier, true);
    res.push(Rc::new(VarDecl{ty: dtype, id}));
    if expected_token!(tokenizer, TokenType::Comma, false) {
      tokenizer.consume().unwrap();
    }
  }
  return Ok(res)
}

pub fn parse_function(tokenizer: &mut Lexer) -> Result<FuncDecl, String> {
  required_token!(tokenizer, TokenType::KeywordFunc, true);
  let parsed_id = required_token!(tokenizer, TokenType::Identifier, true);
  required_token!(tokenizer, TokenType::LPran, true);
  let args = parse_args(tokenizer).unwrap();
  required_token!(tokenizer, TokenType::RPran, true);
  required_token!(tokenizer, TokenType::FuncMap, true);
  let dtype = parse_dtype(tokenizer).unwrap();
  let parsed_body = parse_compound_stmt(tokenizer);
  match parsed_body {
    Ok(body) => {
      Ok(FuncDecl{
        ty: dtype, id: parsed_id, args,
        body: Rc::new(body),
      })
    }
    _ => { Err("Function body parse failure".to_string()) }
  }
}

fn parse_var_decl(tokenizer: &mut Lexer) -> Result<VarDecl, String> {
  let dtype = parse_dtype(tokenizer).unwrap();
  let id = required_token!(tokenizer, TokenType::Identifier, true);
  let var = VarDecl{ty: dtype, id};
  required_token!(tokenizer, TokenType::Semicolon, true);
  Ok(var)
}

fn parse_class(tokenizer: &mut Lexer) -> Result<ClassDecl, String> {
  let mut methods : Vec<Rc<FuncDecl>> = Vec::new();
  let mut attrs : Vec<Rc<VarDecl>> = Vec::new();
  required_token!(tokenizer, TokenType::KeywordClass, true);
  let id = required_token!(tokenizer, TokenType::Identifier, true);
  required_token!(tokenizer, TokenType::LBrace, true);
  while !lookahead_tokens!(tokenizer, TokenType::RBrace) {
    if lookahead_tokens!(tokenizer, TokenType::KeywordFunc) {
      let func = parse_function(tokenizer).unwrap();
      methods.push(Rc::new(func));
    } else {
      let attr = parse_var_decl(tokenizer).unwrap();
      attrs.push(Rc::new(attr));
    }
  }
  required_token!(tokenizer, TokenType::RBrace, true);
  Ok(ClassDecl{id, methods, attrs})
}

pub fn parse_program(tokenizer: &mut Lexer, fname: String) -> Result<TranslateUnit, String> {
  let mut decls : Vec<Decl> = Vec::new();

  while !expected_token!(tokenizer, TokenType::Eof, false) {
    if lookahead_tokens!(tokenizer, TokenType::KeywordClass) {
      let parsed_class = parse_class(tokenizer).unwrap();
      decls.push(Decl::Class(Rc::new(parsed_class)));
    } else {
      let func = parse_function(tokenizer).unwrap();
      decls.push(Decl::Func(Rc::new(func)));
    }
  }
  Ok(TranslateUnit{fname, decls})
}

fn parse_dtype(tokenizer: &mut Lexer) -> Result<Type, String> {
  let token = tokenizer.lookahead(0);
  tokenizer.consume().unwrap();
  let scalar_ty = match token.value {
    TokenType::KeywordInt => {
      Ok(Type::Builtin(Rc::new(BuiltinType{token, code: BuiltinTypeCode::Int})))
    }
    TokenType::KeywordVoid => {
      Ok(Type::Builtin(Rc::new(BuiltinType{token, code: BuiltinTypeCode::Void})))
    }
    TokenType::KeywordChar => {
      Ok(Type::Builtin(Rc::new(BuiltinType{token, code: BuiltinTypeCode::Char})))
    }
    TokenType::KeywordBool => {
      Ok(Type::Builtin(Rc::new(BuiltinType{token, code: BuiltinTypeCode::Bool})))
    }
    TokenType::Identifier => {
      Ok(Type::Builtin(Rc::new(BuiltinType{token, code: BuiltinTypeCode::Unknown})))
    }
    _ => {
      Err(format!("Data type parse failure {}", token))
    }
  };
  let mut dims = 0;
  while expected_token!(tokenizer, TokenType::LBracket, false) {
    required_token!(tokenizer, TokenType::LBracket, true);
    required_token!(tokenizer, TokenType::RBracket, true);
    dims += 1
  }
  if dims == 0 {
    scalar_ty
  } else {
    Ok(Type::Array(Rc::new(ArrayType{scalar_ty: scalar_ty.unwrap(), dims})))
  }
}
