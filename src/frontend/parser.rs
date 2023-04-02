use super::lexer::{Lexer, TokenValue, Token};
use super::ast::{BuiltinType, BuiltinTypeCode, Type, TranslateUnit, Variable, CompoundStmt, ReturnStmt, IntImm, Decl};
use super::ast::{FuncDecl, Stmt, Expr};

macro_rules! required_token {
  ($pp: expr, $expected: pat, $consume: expr) => {
    {
      let to_consume = $pp.next_token($consume);
      match to_consume.value {
        $expected => { to_consume }
        _=> { return Err("Expect A but B got".to_string()) }
      }
    }
  };
}

macro_rules! expected_token {
  ($pp: expr, $expected: pat, $consume: expr) => {
    {
      let to_consume = $pp.next_token($consume);
      match to_consume.value {
        $expected => { true }
        _=> { false }
      }
    }
  };
}

fn parse_intimm(tokenizer: &mut Lexer) -> Result<Expr, String> {
  let token = required_token!(tokenizer, TokenValue::IntLiteral(_), true);
  Ok(Expr::IntImm(IntImm{token}))
}

fn parse_expr(tokenizer: &mut Lexer) -> Result<Expr, String> {
  parse_intimm(tokenizer)
}

fn parse_return(tokenizer: &mut Lexer) -> Result<ReturnStmt, String> {
  let token = required_token!(tokenizer, TokenValue::KeywordReturn, true);
  if expected_token!(tokenizer, TokenValue::Semicolon, false) {
    required_token!(tokenizer, TokenValue::Semicolon, true);
    return Ok(ReturnStmt{token, value: None})
  }
  let parsed_value = parse_expr(tokenizer);
  match parsed_value {
    Ok(value) => {
      required_token!(tokenizer, TokenValue::Semicolon, true);
      Ok(ReturnStmt{token, value: Some(value)})
    }
    Err(msg) => Err(msg)
  }
}

fn parse_statement(tokenizer: &mut Lexer) -> Result<Stmt, String> {
  let parsed = parse_return(tokenizer);
  match parsed {
    Ok(ret) => { Ok(Stmt::Ret(ret)) }
    Err(msg) => Err(msg)
  }
}

fn parse_compound_stmt(tokenizer: &mut Lexer) -> Result<CompoundStmt, String> {
  let left = required_token!(tokenizer, TokenValue::LBrace, true);
  let mut stmts : Vec<Stmt> = Vec::new();
  while !expected_token!(tokenizer, TokenValue::RBrace, false) {
    match parse_statement(tokenizer) {
      Ok(stmt) => { stmts.push(stmt); }
      Err(msg) => { return Err(msg); }
    }
  }
  let right = required_token!(tokenizer, TokenValue::RBrace, true);
  Ok(CompoundStmt{left, right, stmts})
}

pub fn parse_function(tokenizer: &mut Lexer) -> Result<FuncDecl, String> {
  let parsed_dtype = parse_dtype(tokenizer);
  match parsed_dtype {
    Ok(dtype) => {
      let parsed_id = parse_identifier(tokenizer);
      let var : Variable; 
      match parsed_id {
        Ok(token) => { var = Variable{ty: dtype, token} }
        Err(msg) => { return Err(msg); }
      }
      required_token!(tokenizer, TokenValue::LPran, true);
      required_token!(tokenizer, TokenValue::RPran, true);
      let parsed_body = parse_compound_stmt(tokenizer);
      match parsed_body {
        Ok(body) => { Ok(FuncDecl{var, body}) }
        _ => { Err("Function body parse failure".to_string()) }
      }
    }
    Err(msg) => {
      Err(msg)
    }
  }
}

pub fn parse_program(tokenizer: &mut Lexer) -> Result<TranslateUnit, String> {
  let mut decls : Vec<Decl> = Vec::new();
  while !expected_token!(tokenizer, TokenValue::Eof, false) {
    let func = parse_function(tokenizer);
    match func {
      Ok(func) => decls.push(Decl::Func(func)),
      Err(msg) => return Err(msg)
    }
  }
  Ok(TranslateUnit{decls})
}

fn parse_identifier(tokenizer: &mut Lexer) -> Result<Token, String> {
  let token = tokenizer.next_token(true);
  match token.value {
    TokenValue::Identifier => {
      Ok(token)
    }
    _ => {
      Err("Expect an identifier but, ".to_string())
    }
  }
}

fn parse_dtype(tokenizer: &mut Lexer) -> Result<Type, String> {
  let token = tokenizer.next_token(true);
  match token.value {
    TokenValue::KeywordInt => {
      Ok(Type::Builtin(BuiltinType{token, code: BuiltinTypeCode::Int}))
    }
    _ => {
      Err("data type parse failure".to_string())
    }
  }
}
