use std::fmt;
use snailquote;
use regex::Regex;

pub struct Lexer {
  src: String,
  head: usize,
  row: usize,
  col: usize,
  token_rules: Vec<(Result<Regex, regex::Error>, fn(usize, usize, String) -> Token)>
}

pub enum TokenValue {
  Identifier,
  KeywordNew,
  KeywordBool,
  KeywordChar,
  KeywordVoid,
  KeywordInt,
  KeywordReturn,
  LPran,
  RPran,
  LBrace,
  RBrace,
  LBracket,
  RBracket,
  Semicolon,
  IntLiteral(i32),
  StringLiteral(String),
  Eof,
  Unknown,
}

macro_rules! valueless_token {
  ($tok:expr) => {
    |row, col, literal| Lexer::valueless_token_impl(row, col, literal, $tok)
  };
}


pub struct Token {
  pub row: usize,
  pub col: usize,
  pub literal: String,
  pub value: TokenValue,
}

impl fmt::Display for Token {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "@<row {}, col {}> \"{}\"", self.row, self.col, self.literal)
  }
}

impl Token {
  fn len(&self) -> usize {
    return self.literal.len();
  }
}

impl Lexer {

  fn handle_integer(row: usize, col: usize, literal: String) -> Token {
    let value : i32 = literal.parse().unwrap();
    let res = Token {
      row, col, literal,
      value: TokenValue::IntLiteral(value),
    };
    res
  }

  fn handle_string(row: usize, col: usize, literal: String) -> Token {
    let value = snailquote::unescape(&literal[1..literal.len()-1]).unwrap();
    Token { row, col, literal, value: TokenValue::StringLiteral(value) } 
  }

  fn valueless_token_impl(row: usize, col: usize, literal: String, value: TokenValue) -> Token {
    return Token { row, col, literal, value };
  }

  pub fn new(src: &String) -> Self {
    Lexer {
      src: src.to_string(),
      head: 0,
      row: 1,
      col: 1,
      token_rules: vec![
        (Regex::new(r"^new"), valueless_token!(TokenValue::KeywordNew)),
        (Regex::new(r"^bool"), valueless_token!(TokenValue::KeywordBool)),
        (Regex::new(r"^char"), valueless_token!(TokenValue::KeywordChar)),
        (Regex::new(r"^int"), valueless_token!(TokenValue::KeywordInt)),
        (Regex::new(r"^void"), valueless_token!(TokenValue::KeywordVoid)),
        (Regex::new(r"^return"), valueless_token!(TokenValue::KeywordReturn)),
        (Regex::new(r"^[[:alpha:]]+[[:alpha:]_\d]*"), valueless_token!(TokenValue::Identifier)),
        (Regex::new(r"^\("), valueless_token!(TokenValue::LPran)),
        (Regex::new(r"^\)"), valueless_token!(TokenValue::RPran)),
        (Regex::new(r"^\{"), valueless_token!(TokenValue::LBrace)),
        (Regex::new(r"^\}"), valueless_token!(TokenValue::RBrace)),
        (Regex::new(r"^\["), valueless_token!(TokenValue::LBracket)),
        (Regex::new(r"^\]"), valueless_token!(TokenValue::RBracket)),
        (Regex::new(r"^;"), valueless_token!(TokenValue::Semicolon)),
        (Regex::new(r"^\d+"), Lexer::handle_integer),
        (Regex::new("\".*\""), Lexer::handle_string)
      ]
    }
  }

  fn skip_spaces(&mut self) -> bool {
    while self.head < self.src.len() {
      let head = self.src.chars().nth(self.head);
      match head {
        Some(' ') => {
          self.head += 1;
          self.col += 1;
        }
        Some('\n') => {
          self.head += 1;
          self.row += 1;
          self.col = 1;
        }
        Some(_) => {
          break;
        }
        None => {
          break;
        }
      }
    }
    return self.head < self.src.len();
  }

  pub fn next_token(&mut self, pop: bool) -> Token {
    let mut res = Token {
      row: self.row,
      col: self.col,
      literal: "".to_string(),
      value: TokenValue::Unknown
    };
    if !self.skip_spaces() {
      res.value = TokenValue::Eof;
      return res;
    }
    for (regex, converter) in &self.token_rules {
      match regex {
        Ok(ok) => {
          let matched = ok.find(&self.src[self.head..]);
          match matched {
            Some(x) => {
              let start = self.head + x.start();
              let end = self.head + x.end();
              let literal = &self.src[start..end];
              let tok = converter(self.row, self.col, literal.to_string());
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
    if pop {
      self.head += res.len();
      self.col += res.len();
    }
    res
  }

}
