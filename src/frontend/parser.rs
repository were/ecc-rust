use std::rc::Rc;

use super::lexer::{Lexer, TokenType};
use super::ast::{
  BuiltinType, BuiltinTypeCode, Type, TranslateUnit, CompoundStmt, ReturnStmt, IntImm, Decl, InlineAsm
};
use super::ast::{FuncDecl, Stmt, Expr, StrImm, FuncCall, ClassDecl, VarDecl, ArrayType, BinaryOp, ArrayIndex};

fn parse_intimm(tokenizer: &mut Lexer) -> Result<Expr, String> {
  let token = tokenizer.consume(TokenType::IntLiteral);
  let value : i32 = token.literal.parse().unwrap();
  Ok(Expr::IntImm(Rc::new(IntImm{token, value })))
}

fn parse_expr_term(tokenizer: &mut Lexer) -> Result<Expr, String> {
  if tokenizer.lookahead(TokenType::IntLiteral) {
    return parse_intimm(tokenizer)
  }
  if tokenizer.lookahead(TokenType::StringLiteral) {
    return parse_strimm(tokenizer)
  }
  return parse_possibly_lval(tokenizer);
}

/// Parse [x][y][z]...
/// NOTE: The array identifier itself is NOT here!
fn parse_array_suffix(tokenizer: &mut Lexer) -> Vec<Expr> {
  let mut dim = Vec::new();
  while tokenizer.lookahead(TokenType::LBracket) {
    tokenizer.consume_any();
    let size = parse_rval(tokenizer).unwrap();
    tokenizer.consume(TokenType::RBracket);
    dim.push(size);
  }
  dim
}

fn parse_id_and_suffix(tokenizer: &mut Lexer) -> Result<Expr, String> {

  let id = tokenizer.consume(TokenType::Identifier);

  if tokenizer.lookahead(TokenType::LPran) {
    let params = parse_params(tokenizer).unwrap();
    return Ok(Expr::FuncCall(Rc::new(FuncCall{fname: id, params})));
  }

  if tokenizer.lookahead(TokenType::LBracket) {
    let indices = parse_array_suffix(tokenizer);
    return Ok(Expr::ArrayIndex(Rc::new(ArrayIndex{array: Expr::UnknownRef(id), indices})));
  }

  return Ok(Expr::UnknownRef(id))
}

/// Only value parsed by this function is possbile to be a lvalue.
fn parse_possibly_lval(tokenizer: &mut Lexer) -> Result<Expr, String> {
  let mut expr = parse_id_and_suffix(tokenizer).unwrap();
  while tokenizer.lookahead(TokenType::AttrAccess) {
    let op = tokenizer.consume_any();
    let rhs = parse_id_and_suffix(tokenizer).unwrap();
    expr = match rhs {
      Expr::UnknownRef(_) => {
        Expr::BinaryOp(Rc::new(BinaryOp{op, lhs: expr, rhs}))
      }
      Expr::FuncCall(_) => {
        Expr::BinaryOp(Rc::new(BinaryOp{op, lhs: expr, rhs}))
      }
      Expr::ArrayIndex(array_index) => {
        let new_attr = Expr::BinaryOp(Rc::new(BinaryOp{op, lhs: expr, rhs: array_index.array.clone()}));
        Expr::ArrayIndex(Rc::new(ArrayIndex{array: new_attr, indices: array_index.indices.clone()}))
      }
      _ => { return Err("Expect identifier".to_string()) }
    };
  }
  Ok(expr)
}

fn parse_binary_expr(tokenizer: &mut Lexer, operators: &[TokenType]) -> Result<Expr, String> {
  let mut expr = parse_expr_term(tokenizer).unwrap();
  while tokenizer.tok().is_one_of(operators) {
    let op = tokenizer.consume_any();
    let rhs = parse_expr_term(tokenizer).unwrap();
    expr = Expr::BinaryOp(Rc::new(BinaryOp{op, lhs: expr, rhs}));
  }
  Ok(expr)
}

fn parse_assignment_expr(tokenizer: &mut Lexer) -> Result<Expr, String> {
  let res = parse_possibly_lval(tokenizer);
  if tokenizer.look_n_ahead(&[TokenType::AssignEq]) {
    tokenizer.consume_any();
    return parse_binary_expr(tokenizer, &[TokenType::Add, TokenType::Sub]);
  }
  return res;
}

fn parse_strimm(tokenizer: &mut Lexer) -> Result<Expr, String> {
  let token = tokenizer.consume(TokenType::StringLiteral);
  let value = snailquote::unescape(&token.literal).unwrap();
  Ok(Expr::StrImm(Rc::new(StrImm{token, value})))
}

fn parse_return(tokenizer: &mut Lexer) -> Result<ReturnStmt, String> {
  let token = tokenizer.consume(TokenType::KeywordReturn);
  if tokenizer.lookahead(TokenType::Semicolon) {
    return Ok(ReturnStmt{token, value: None})
  }
  let parsed_value = parse_rval(tokenizer).unwrap();
  Ok(ReturnStmt{token, value: Some(parsed_value)})
}

/// Precedence from low to high, parse each operator expression.
fn parse_operator_expr(tokenizer: &mut Lexer, operators: &[(i32, &[TokenType])]) -> Result<Expr, String> {
  if operators.len() == 0 {
    return parse_expr_term(tokenizer)
  } else if operators[0].0 == 1 {
    if tokenizer.tok().is_one_of(operators[0].1) {
      parse_operator_expr(tokenizer, &operators[1..]).unwrap();
    }
  } else if operators[0].0 == 2 {
    let mut res = parse_operator_expr(tokenizer, &operators[1..]).unwrap();
    while tokenizer.tok().is_one_of(operators[0].1) {
      let op = tokenizer.consume_any();
      let rhs = parse_operator_expr(tokenizer, &operators[1..]).unwrap();
      res = Expr::BinaryOp(Rc::new(BinaryOp{op, lhs: res, rhs}));
    }
    return Ok(res);
  }
  panic!("Invalid operator precedence")
}

fn parse_rval(tokenizer: &mut Lexer) -> Result<Expr, String> {
  return parse_operator_expr(tokenizer, &[(2, &[TokenType::Add, TokenType::Sub])]);
}

fn parse_params(tokenizer: &mut Lexer) -> Result<Vec<Expr>, String> {
  let mut params = Vec::new();
  tokenizer.consume(TokenType::LPran);
  while !tokenizer.lookahead(TokenType::RPran) {
    let parsed = parse_rval(tokenizer);
    match parsed {
      Ok(expr) => { params.push(expr) }
      Err(msg) => { return Err(msg) }
    }
    if tokenizer.lookahead(TokenType::Comma) {
      tokenizer.consume(TokenType::Comma);
    }
  }
  tokenizer.consume(TokenType::RPran);
  Ok(params)
}

fn parse_inline_asm(tokenizer: &mut Lexer) -> Result<InlineAsm, String> {
  tokenizer.consume(TokenType::KeywordAsm);
  let args = parse_params(tokenizer).unwrap();
  tokenizer.consume(TokenType::RPran);
  tokenizer.consume(TokenType::Semicolon);
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
  if tokenizer.lookahead(TokenType::KeywordReturn) {
    let res = parse_return(tokenizer).unwrap();
    return Ok(Stmt::Ret(Rc::new(res)));
  }
  if tokenizer.lookahead(TokenType::KeywordAsm) {
    let asm = parse_inline_asm(tokenizer).unwrap();
    return Ok(Stmt::InlineAsm(Rc::new(asm)));
  }
  if tokenizer.lookahead(TokenType::KeywordLet) {
    return parse_decl_stmt(tokenizer);
  }
  let res = Ok(Stmt::Evaluate(parse_assignment_expr(tokenizer).unwrap()));
  res
}

/// Declare a variable in a compound statement.
fn parse_decl_stmt(tokenizer: &mut Lexer) -> Result<Stmt, String> {
  tokenizer.consume(TokenType::KeywordLet);
  let ty = parse_dtype(tokenizer).unwrap();
  let id = tokenizer.consume(TokenType::Identifier);
  let mut rhs = None;
  if tokenizer.lookahead(TokenType::AssignEq) {
    tokenizer.consume(TokenType::AssignEq);
    rhs = Some(parse_rval(tokenizer).unwrap());
  }
  return Ok(Stmt::VarDecl(Rc::new(VarDecl{ty, id, init: rhs})));
}

fn parse_compound_stmt(tokenizer: &mut Lexer) -> Result<CompoundStmt, String> {
  let left = tokenizer.consume(TokenType::LBrace);
  let mut stmts : Vec<Stmt> = Vec::new();
  while !tokenizer.lookahead(TokenType::RBrace) {
    match parse_statement(tokenizer) {
      Ok(stmt) => {
        stmts.push(stmt);
        tokenizer.consume(TokenType::Semicolon);
      }
      Err(msg) => { return Err(msg); }
    }
  }
  let right = tokenizer.consume(TokenType::RBrace);

  return Ok(CompoundStmt{ left, right, stmts })
}

pub fn parse_args(tokenizer: &mut Lexer) -> Result<Vec<Rc<VarDecl>>, String> {
  let mut res : Vec<Rc<VarDecl>> = Vec::new();
  while !tokenizer.lookahead(TokenType::RPran) {
    let dtype = parse_dtype(tokenizer).unwrap();
    let id = tokenizer.consume(TokenType::Identifier);
    res.push(Rc::new(VarDecl{ty: dtype, id, init: None}));
    if tokenizer.lookahead(TokenType::Comma) {
      tokenizer.consume_any();
    }
  }
  return Ok(res)
}

pub fn parse_function(tokenizer: &mut Lexer) -> Result<FuncDecl, String> {
  tokenizer.consume(TokenType::KeywordFunc);
  let parsed_id = tokenizer.consume(TokenType::Identifier);
  tokenizer.consume(TokenType::LPran);
  let args = parse_args(tokenizer).unwrap();
  tokenizer.consume(TokenType::RPran);
  tokenizer.consume(TokenType::FuncMap);
  let dtype = parse_dtype(tokenizer).unwrap();
  let parsed_body = if tokenizer.lookahead(TokenType::Semicolon) {
    let tok = tokenizer.consume_any();
    Ok(CompoundStmt{
      left: tok.clone(),
      right: tok.clone(),
      stmts: Vec::new(),
    })
  } else {
    parse_compound_stmt(tokenizer)
  };
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

/// Variable declarations in class body and function argument list.
fn parse_var_decl(tokenizer: &mut Lexer) -> Result<VarDecl, String> {
  let dtype = parse_dtype(tokenizer).unwrap();
  let id = tokenizer.consume(TokenType::Identifier);
  let var = VarDecl{ty: dtype, id, init: None};
  Ok(var)
}

fn parse_class(tokenizer: &mut Lexer) -> Result<ClassDecl, String> {
  let mut methods : Vec<Rc<FuncDecl>> = Vec::new();
  let mut attrs : Vec<Rc<VarDecl>> = Vec::new();
  tokenizer.consume(TokenType::KeywordClass);
  let id = tokenizer.consume(TokenType::Identifier);
  tokenizer.consume(TokenType::LBrace);
  while !tokenizer.lookahead(TokenType::RBrace) {
    if tokenizer.lookahead(TokenType::KeywordFunc) {
      let func = parse_function(tokenizer).unwrap();
      methods.push(Rc::new(func));
    } else {
      let attr = parse_var_decl(tokenizer).unwrap();
      attrs.push(Rc::new(attr));
      tokenizer.consume(TokenType::Semicolon);
    }
  }
  tokenizer.consume(TokenType::RBrace);
  Ok(ClassDecl{id, methods, attrs})
}

pub fn parse_program(tokenizer: &mut Lexer, fname: String) -> Result<TranslateUnit, String> {
  let mut decls : Vec<Decl> = Vec::new();

  while !tokenizer.lookahead(TokenType::Eof) {
    if tokenizer.lookahead(TokenType::KeywordClass) {
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
  let token = tokenizer.consume_any();
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
  while tokenizer.lookahead(TokenType::LBracket) {
    tokenizer.consume(TokenType::LBracket);
    tokenizer.consume(TokenType::RBracket);
    dims += 1
  }
  if dims == 0 {
    scalar_ty
  } else {
    Ok(Type::Array(Rc::new(ArrayType{scalar_ty: scalar_ty.unwrap(), dims})))
  }
}
