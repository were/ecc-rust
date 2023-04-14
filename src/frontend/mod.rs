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
    tus: vec![Rc::new(parsed_builtins), Rc::new(ast)],
    symbols: Rc::new(sema::SymbolTable::new())
  })
}

pub fn parse(fname: String, src: String) -> Rc<Linkage> {
  let mut tokenizer = lexer::Lexer::new(src);
  let ast = parser::parse_program(&mut tokenizer, fname).unwrap();
  let res = inject_builtins(ast);
  return res
}

pub fn semantic_check(ast: &Rc<Linkage>) -> Rc<Linkage> {
  let hoisted = sema::hoist_methods(ast);
  println!("{}", hoisted);
  let type_resolved = sema::resolve_symbols(&hoisted, false);
  sema::resolve_symbols(&type_resolved, true)
}
