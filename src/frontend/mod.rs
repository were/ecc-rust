use std::rc::Rc;

use self::ast::Linkage;

mod lexer;
mod ast;
mod parser;
mod printer;
mod visitor;
mod sema;

pub fn inject_builtins(ast: ast::TranslateUnit) -> Rc<Linkage> {
  let builtins = include_str!("../../builtins/builtins.ecc");
  let mut tokenizer = lexer::Lexer::new(builtins.to_string());
  let parsed_builtins = parser::parse_program(&mut tokenizer, "builtin.ecc".to_string()).unwrap();
  Rc::new(Linkage{
    tus: vec![Rc::new(ast), Rc::new(parsed_builtins)],
    symbols: Rc::new(sema::SymbolTable::new())
  })
}

pub fn parse(fname: String, src: String) -> Rc<Linkage> {
  let mut tokenizer = lexer::Lexer::new(src);
  let ast = parser::parse_program(&mut tokenizer, fname).unwrap();
  inject_builtins(ast)
}

pub fn semantic_check(ast: &Rc<Linkage>) -> Rc<Linkage> {
  let new_ast = sema::hoist_methods(ast);
  println!("{}", new_ast);
  sema::resolve_types(&new_ast)
}
