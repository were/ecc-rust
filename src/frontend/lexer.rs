use std::fmt;
use regex::Regex;

pub struct Lexer {
  i : usize,
  tokens : Vec<Token>,
}

#[derive(Clone, PartialEq, Eq)]
pub enum TokenType {
  Identifier,
  KeywordAsm,
  KeywordNew,
  KeywordBool,
  KeywordChar,
  KeywordVoid,
  KeywordIn,
  KeywordI32,
  KeywordReturn,
  KeywordClass,
  KeywordFunc,
  KeywordFor,
  KeywordLet,
  KeywordCastAs,
  RangeDotdot,
  FuncMap,
  AttrAccess,
  LPran,
  RPran,
  LBrace,
  RBrace,
  LBracket,
  RBracket,
  Semicolon,
  AssignEq,
  Add,
  Sub,
  Mod,
  Div,
  Mul,
  Comma,
  IntLiteral,
  StringLiteral,
  Eof,
  Unknown,
}

impl fmt::Display for TokenType {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      TokenType::AttrAccess => write!(f, "AttrAccess ."),
      TokenType::Identifier => write!(f, "Identifier"),
      TokenType::KeywordAsm => write!(f, "KeywordAsm asm"),
      TokenType::KeywordNew => write!(f, "KeywordNew new"),
      TokenType::KeywordBool => write!(f, "KeywordBool bool"),
      TokenType::KeywordChar => write!(f, "KeywordChar i8"),
      TokenType::KeywordCastAs => write!(f, "KeywordCastAs as"),
      TokenType::KeywordVoid => write!(f, "KeywordVoid void"),
      TokenType::KeywordI32 => write!(f, "KeywordI32 i32"),
      TokenType::KeywordIn => write!(f, "KeywordIn in"),
      TokenType::KeywordReturn => write!(f, "KeywordReturn return"),
      TokenType::KeywordClass => write!(f, "KeywordClass class"),
      TokenType::KeywordFunc => write!(f, "KeywordFunc func"),
      TokenType::KeywordFor => write!(f, "KeywordFor for"),
      TokenType::KeywordLet => write!(f, "KeywordLet let"),
      TokenType::FuncMap => write!(f, "FuncMap ->"),
      TokenType::RangeDotdot => write!(f, "RangeDotdot .."),
      TokenType::LPran => write!(f, "LPran ("),
      TokenType::RPran => write!(f, "RPran )"),
      TokenType::LBrace => write!(f, "LBrace"),
      TokenType::RBrace => write!(f, "RBrace"),
      TokenType::LBracket => write!(f, "LBracket ["),
      TokenType::RBracket => write!(f, "RBracket ]"),
      TokenType::Semicolon => write!(f, "Semicolon ;"),
      TokenType::Comma => write!(f, "Comma ,"),
      TokenType::AssignEq => write!(f, "AssignEq ="),
      TokenType::Add => write!(f, "Add +"),
      TokenType::Sub => write!(f, "Sub -"),
      TokenType::Mod => write!(f, "Mod %"),
      TokenType::Div => write!(f, "Div /"),
      TokenType::Mul => write!(f, "Mul *"),
      TokenType::IntLiteral => write!(f, "IntLiteral"),
      TokenType::StringLiteral => write!(f, "StringLiteral"),
      TokenType::Eof => write!(f, "Eof"),
      TokenType::Unknown => write!(f, "Unknown"),
    }
  }
}

macro_rules! valueless_token {
  ($tok:expr) => {
    |row, col, literal| TokenHandle::valueless_token_impl(row, col, literal, $tok)
  };
}


#[derive(Clone)]
pub struct Token {
  pub row: usize,
  pub col: usize,
  pub literal: String,
  pub value: TokenType,
}

impl fmt::Display for Token {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "@<row {}, col {}> {}", self.row, self.col, self.literal)
  }
}

impl Token {

  fn len(&self) -> usize {
    return self.literal.len();
  }

  pub fn new() -> Token {
    Token {row: 0, col: 0, literal: "".to_string(), value: TokenType::Unknown }
  }

  pub fn is_one_of(&self, types: &[TokenType]) -> bool {
    for t in types {
      if self.value == *t {
        return true;
      }
    }
    return false;
  }
}

struct TokenHandle {
  handle: Vec<(Result<Regex, regex::Error>, fn(usize, usize, String) -> Token)>
}

impl TokenHandle {

  pub fn new() -> Self {
    TokenHandle {
      handle: vec![
        (Regex::new(r"^as"), valueless_token!(TokenType::KeywordCastAs)),
        (Regex::new(r"^asm"), valueless_token!(TokenType::KeywordAsm)),
        (Regex::new(r"^new"), valueless_token!(TokenType::KeywordNew)),
        (Regex::new(r"^bool"), valueless_token!(TokenType::KeywordBool)),
        (Regex::new(r"^i8"), valueless_token!(TokenType::KeywordChar)),
        (Regex::new(r"^i32"), valueless_token!(TokenType::KeywordI32)),
        (Regex::new(r"^in"), valueless_token!(TokenType::KeywordIn)),
        (Regex::new(r"^void"), valueless_token!(TokenType::KeywordVoid)),
        (Regex::new(r"^return"), valueless_token!(TokenType::KeywordReturn)),
        (Regex::new(r"^class"), valueless_token!(TokenType::KeywordClass)),
        (Regex::new(r"^func"), valueless_token!(TokenType::KeywordFunc)),
        (Regex::new(r"^for"), valueless_token!(TokenType::KeywordFor)),
        (Regex::new(r"^let"), valueless_token!(TokenType::KeywordLet)),
        (Regex::new(r"^\->"), valueless_token!(TokenType::FuncMap)),
        (Regex::new(r"^\.\."), valueless_token!(TokenType::RangeDotdot)),
        (Regex::new(r"^[[:alpha:]_]+[[:alpha:]_\d]*"), valueless_token!(TokenType::Identifier)),
        (Regex::new(r"^\("), valueless_token!(TokenType::LPran)),
        (Regex::new(r"^\)"), valueless_token!(TokenType::RPran)),
        (Regex::new(r"^\{"), valueless_token!(TokenType::LBrace)),
        (Regex::new(r"^\}"), valueless_token!(TokenType::RBrace)),
        (Regex::new(r"^\["), valueless_token!(TokenType::LBracket)),
        (Regex::new(r"^\]"), valueless_token!(TokenType::RBracket)),
        (Regex::new(r"^;"), valueless_token!(TokenType::Semicolon)),
        (Regex::new(r"^,"), valueless_token!(TokenType::Comma)),
        (Regex::new(r"^="), valueless_token!(TokenType::AssignEq)),
        (Regex::new(r"^\+"), valueless_token!(TokenType::Add)),
        (Regex::new(r"^\-"), valueless_token!(TokenType::Sub)),
        (Regex::new(r"^%"), valueless_token!(TokenType::Mod)),
        (Regex::new(r"^/"), valueless_token!(TokenType::Div)),
        (Regex::new(r"^\*"), valueless_token!(TokenType::Div)),
        (Regex::new(r"^\d+"), valueless_token!(TokenType::IntLiteral)),
        (Regex::new(r"^\."), valueless_token!(TokenType::AttrAccess)),
        (Regex::new("^\".*\""), valueless_token!(TokenType::StringLiteral)),
      ]
    }
  }

  // fn handle_string(row: usize, col: usize, literal: String) -> Token {
  //   let value = snailquote::unescape(&literal[1..literal.len()-1]).unwrap();
  //   Token { row, col, literal, value: TokenType::StringLiteral(value) } 
  // }

  fn valueless_token_impl(row: usize, col: usize, literal: String, value: TokenType) -> Token {
    return Token { row, col, literal, value };
  }

}

impl Lexer {

  fn skip(src : &String, head : &mut usize, row : &mut usize, col : &mut usize) -> bool {
    let mut skip_comment = false;
    while *head < src.len() {
      let ch = src.chars().nth(*head);
      match ch {
        Some(' ') => {
          *head += 1;
          *col += 1;
        }
        Some('\n') => {
          *head += 1;
          *row += 1;
          *col = 1;
          skip_comment = false;
        }
        Some('/') => {
          if *head + 1 < src.len() {
            let next = src.chars().nth(*head + 1);
            match next {
              Some('/') => {
                *head += 2;
                *col += 2;
                skip_comment = true;
              }
              _ => {
                break;
              }
            }
          }
        }
        Some(_) => {
          if !skip_comment {
            break;
          }
          *head += 1;
          *col += 1;
        }
        None => {
          break;
        }
      }
    }
    return *head < src.len();
  }

  fn next_token(handle : &TokenHandle, src: &String, head : &mut usize, row : &mut usize, col : &mut usize) -> Token {
    let mut res = Token {
      row: *row, col: *col,
      literal: "".to_string(),
      value: TokenType::Unknown
    };
    for (regex, converter) in &handle.handle {
      match regex {
        Ok(ok) => {
          let matched = ok.find(&src[*head..]);
          match matched {
            Some(x) => {
              let start = *head + x.start();
              let end = *head + x.end();
              let literal = src[start..end].to_string();
              let tok = converter(*row, *col, literal.to_string());
              if res.len() < tok.len() {
                res = tok;
              }
            }
            None => {
            }
          }
        }
        Err(_) => {}
      }
    }
    match res.value {
      TokenType::Unknown => {
        panic!("Unknown token at row {}, col {}: {}", res.row, res.col, &src[*head..]);
      }
      _ => {}
    }
    *head += res.len();
    *col += res.len();
    res
  }

  pub fn new(src: String) -> Self {
    let handle = TokenHandle::new();

    let mut head : usize = 0;
    let mut row : usize = 1;
    let mut col : usize = 1;
    let mut tokens : Vec<Token> = Vec::new();

    while Lexer::skip(&src, &mut head, &mut row, &mut col) {
      tokens.push(Lexer::next_token(&handle, &src, &mut head, &mut row, &mut col));
    }

    tokens.push(Token {
      row, col,
      literal: "".to_string(),
      value: TokenType::Eof
    });

    // for token in &tokens {
    //   println!("{} {}", token.value, token);
    // }

    Lexer { i : 0, tokens, }
  }

  pub fn tok(&self) -> &Token {
    self.tokens.get(self.i).unwrap()
  }

  pub fn lookahead(&self, ty: TokenType) -> bool {
    self.tok().is_one_of(&[ty])
  }

  // pub fn look_n_ahead(&self, types : &[TokenType]) -> bool {
  //   for (i, ty) in types.iter().enumerate() {
  //     let tok = self.tokens.get(i + self.i).unwrap();
  //     if tok.value != *ty {
  //       return false;
  //     }
  //   }
  //   true
  // }

  pub fn consume(&mut self, ty: TokenType) -> Token {
    if self.lookahead(ty.clone()) {
      return self.consume_any()
    }
    panic!("Expect {:} but found {:}", ty, self.tok());
  }

  pub fn consume_any(&mut self) -> Token {
    let res = self.tok().clone();
    self.i += 1;
    return res;
  }

  /// Insert a token right before the token stream.
  pub fn push_token(&mut self, t: Token) {
    self.tokens.insert(self.i, t)
  }

}
