mod lexer;
mod ast;
mod parser;
mod printer;
mod visitor;

pub fn parse(src: &String) {
  let mut tokenizer = lexer::Lexer::new(src);
  let mut ast = parser::parse_program(&mut tokenizer).unwrap();
  println!("{}", ast);
}
