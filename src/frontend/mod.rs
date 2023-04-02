mod lexer;
mod ast;
mod parser;

pub fn parse(src: &String) {
  let mut tokenizer = lexer::Lexer::new(src);
  parser::parse_program(&mut tokenizer);
}
