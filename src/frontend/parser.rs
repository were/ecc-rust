use std::rc::Rc;

use super::lexer::{Lexer, TokenType, Token};
use super::ast::{
  BuiltinType, BuiltinTypeCode, Type, TranslateUnit, CompoundStmt, ReturnStmt, IntImm,
  Decl, InlineAsm, NewExpr, Cast, ForStmt, WhileStmt, LoopJump
};
use super::ast::{
  FuncDecl, Stmt, Expr, StrImm, FuncCall, ClassDecl, VarDecl, ArrayType, BinaryOp, ArrayIndex, IfStmt, UnaryOp
};

fn parse_intimm(tokenizer: &mut Lexer) -> Result<Expr, String> {
  let token = tokenizer.consume(TokenType::IntLiteral);
  let value : i32 = token.literal.parse().unwrap();
  Ok(Expr::IntImm(Rc::new(IntImm{token, value })))
}

fn parse_expr_term(tokenizer: &mut Lexer) -> Result<Expr, String> {
  if tokenizer.lookahead(TokenType::IntLiteral) {
    return parse_intimm(tokenizer)
  }
  if tokenizer.lookahead(TokenType::KeywordFalse) {
    return Ok(Expr::IntImm(Rc::new(IntImm{token: tokenizer.consume_any(), value: 0 })));
  }
  if tokenizer.lookahead(TokenType::KeywordTrue) {
    return Ok(Expr::IntImm(Rc::new(IntImm{token: tokenizer.consume_any(), value: 1 })));
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

  if tokenizer.lookahead(TokenType::LPran) {
    tokenizer.consume(TokenType::LPran);
    let res = parse_rval(tokenizer);
    tokenizer.consume(TokenType::RPran);
    return res
  }

  let id = tokenizer.consume(TokenType::Identifier);

  if tokenizer.lookahead(TokenType::LPran) {
    let params = parse_params(tokenizer).unwrap();
    return Ok(Expr::FuncCall(Rc::new(FuncCall{rewrite:false, fname: id, params})));
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

fn parse_assignment_expr(tokenizer: &mut Lexer) -> Result<Expr, String> {
  let res = parse_possibly_lval(tokenizer);
  if tokenizer.lookahead(TokenType::AssignEq) {
    let op = tokenizer.consume(TokenType::AssignEq);
    let rval = parse_rval(tokenizer).unwrap();
    return Ok(Expr::BinaryOp(Rc::new(BinaryOp { op, lhs: res.unwrap(), rhs: rval })));
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
  match operators.get(0) {
    None => {
      return parse_expr_term(tokenizer);
    }
    Some((1, parsing_operator)) => {
      if tokenizer.tok().is_one_of(&parsing_operator) {
        let op = tokenizer.consume_any();
        let res = parse_operator_expr(tokenizer, &operators[1..]).unwrap();
        return Ok(Expr::UnaryOp(Rc::new(UnaryOp{op, expr: res})));
      } else {
        return parse_operator_expr(tokenizer, &operators[1..]);
      }
    }
    Some((2, parsing_operator)) => {
      let mut res = parse_operator_expr(tokenizer, &operators[1..]).unwrap();
      while tokenizer.tok().is_one_of(parsing_operator) {
        let op = tokenizer.consume_any();
        let rhs = parse_operator_expr(tokenizer, &operators[1..]).unwrap();
        res = Expr::BinaryOp(Rc::new(BinaryOp{op, lhs: res, rhs}));
      }
      return Ok(res);
    }
    _ => panic!("Invalid operator precedence")
  }
}

fn parse_new_expr(tokenizer: &mut Lexer) -> Result<Expr, String> {
  let token = tokenizer.consume(TokenType::KeywordNew);
  let dtype = parse_dtype(tokenizer, true).unwrap();
  Ok(Expr::NewExpr(Rc::new(NewExpr{token, dtype})))
}

fn parse_rval(tokenizer: &mut Lexer) -> Result<Expr, String> {
  let res = if tokenizer.lookahead(TokenType::KeywordNew) {
    parse_new_expr(tokenizer)
  } else {
    parse_operator_expr(tokenizer,
      &[(2, &[TokenType::LogicAnd, TokenType::LogicOr]),
        (2, &[TokenType::BitwiseAnd, TokenType::BitwiseOr, TokenType::BitwiseXor,
              TokenType::BitwiseShl, TokenType::BitwiseShr]),
        (2, &[TokenType::LE, TokenType::LT, TokenType::GE, TokenType::GT, TokenType::EQ, TokenType::NE]),
        (2, &[TokenType::Add, TokenType::Sub]),
        (2, &[TokenType::Mod, TokenType::Div, TokenType::Mul]),
        (1, &[TokenType::LogicNot, TokenType::Sub])])
  };
  if tokenizer.lookahead(TokenType::KeywordCastAs) {
    let token = tokenizer.consume(TokenType::KeywordCastAs);
    let dtype = parse_dtype(tokenizer, false).unwrap();
    return Ok(Expr::Cast(Rc::new(Cast{token, expr: res.unwrap(), dtype})));
  }
  return res;
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

fn parse_for_stmt(tokenizer: &mut Lexer) -> Result<Stmt, String> {
  tokenizer.consume(TokenType::KeywordFor);
  let id = tokenizer.consume(TokenType::Identifier);
  tokenizer.push_token(Token { row: 0, col: 0, literal: "i32".to_string(), value: TokenType::KeywordI32 });
  let ty = parse_dtype(tokenizer, false).unwrap();
  tokenizer.consume(TokenType::KeywordIn);
  let init = Some(parse_rval(tokenizer).unwrap());
  let var = VarDecl{ty, id, init };
  tokenizer.consume(TokenType::RangeDotdot);
  let end = parse_rval(tokenizer).unwrap();
  let body = parse_compound_stmt(tokenizer).unwrap();
  Ok(Stmt::ForStmt(Rc::new(ForStmt { var: Rc::new(var), end, body: Rc::new(body) })))
}

fn parse_while_stmt(tokenizer: &mut Lexer) -> Result<Stmt, String> {
  let loc = tokenizer.consume(TokenType::KeywordWhile);
  let cond = parse_rval(tokenizer).unwrap();
  let body = parse_compound_stmt(tokenizer).unwrap();
  Ok(Stmt::WhileStmt(Rc::new(WhileStmt { loc, cond, body: Rc::new(body) })))
}

fn parse_statement(tokenizer: &mut Lexer) -> Result<Stmt, String> {
  match tokenizer.tok().value {
    TokenType::KeywordReturn => {
      let res = parse_return(tokenizer).unwrap();
      tokenizer.consume(TokenType::Semicolon);
      return Ok(Stmt::Ret(Rc::new(res)));
    }
    TokenType::KeywordAsm => {
      let asm = parse_inline_asm(tokenizer).unwrap();
      tokenizer.consume(TokenType::Semicolon);
      return Ok(Stmt::InlineAsm(Rc::new(asm)));
    }
    TokenType::KeywordLet => {
      let res = parse_decl_stmt(tokenizer);
      tokenizer.consume(TokenType::Semicolon);
      return res;
    }
    TokenType::KeywordFor => {
      return parse_for_stmt(tokenizer);
    }
    TokenType::KeywordWhile => {
      return parse_while_stmt(tokenizer);
    }
    TokenType::LBrace => {
      return Ok(Stmt::CompoundStmt(Rc::new(parse_compound_stmt(tokenizer).unwrap())));
    }
    TokenType::KeywordIf => {
      return Ok(Stmt::IfStmt(Rc::new(parse_if_stmt(tokenizer).unwrap())));
    }
    TokenType::KeywordBreak | TokenType::KeywordContinue => {
      let loc = tokenizer.consume_any();
      tokenizer.consume(TokenType::Semicolon);
      return Ok(Stmt::LoopJump(Rc::new(LoopJump{loc})));
    }
    _ => {
      let res = Ok(Stmt::Evaluate(parse_assignment_expr(tokenizer).unwrap()));
      tokenizer.consume(TokenType::Semicolon);
      return res;
    }
  }
}

fn parse_if_stmt(tokenizer: &mut Lexer) -> Result<IfStmt, String> {
  tokenizer.consume(TokenType::KeywordIf);
  let cond = parse_rval(tokenizer);
  let then_body = Rc::new(parse_compound_stmt(tokenizer).unwrap());
  let else_body = if tokenizer.lookahead(TokenType::KeywordElse) {
    tokenizer.consume(TokenType::KeywordElse);
    Some(Rc::new(parse_compound_stmt(tokenizer).unwrap()))
  } else {
    None
  };
  Ok(IfStmt{ cond: cond.unwrap(), then_body, else_body })
}

/// Declare a variable in a compound statement.
fn parse_decl_stmt(tokenizer: &mut Lexer) -> Result<Stmt, String> {
  tokenizer.consume(TokenType::KeywordLet);
  let ty = parse_dtype(tokenizer, false).unwrap();
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
    let dtype = parse_dtype(tokenizer, false).unwrap();
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
  let dtype = parse_dtype(tokenizer, false).unwrap();
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
  let dtype = parse_dtype(tokenizer, false).unwrap();
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

fn parse_dtype(tokenizer: &mut Lexer, for_new: bool) -> Result<Type, String> {
  let token = tokenizer.consume_any();
  let scalar_ty = match token.value {
    TokenType::KeywordI32 => {
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
  let mut dims = Vec::new();
  while tokenizer.lookahead(TokenType::LBracket) {
    tokenizer.consume(TokenType::LBracket);
    if for_new && !tokenizer.lookahead(TokenType::RBracket) {
      dims.push(parse_rval(tokenizer).unwrap());
    } else {
      dims.push(Expr::UnknownRef(Token::new()));
    }
    tokenizer.consume(TokenType::RBracket);
  }
  if dims.len() == 0 {
    scalar_ty
  } else {
    Ok(Type::Array(Rc::new(ArrayType{scalar_ty: scalar_ty.unwrap(), dims})))
  }
}
