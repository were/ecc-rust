use std::rc::Rc;

use crate::ir::module::Module;

use self::ast::Linkage;

mod lexer;
mod ast;
mod parser;
mod printer;
mod visitor;
mod sema;
mod codegen;

pub fn inject_builtins(ast: ast::TranslateUnit) -> Rc<Linkage> {
  let builtins = include_str!("../../builtins/builtins.ecc");
  let mut tokenizer = lexer::Lexer::new(builtins.to_string());
  let parsed_builtins = parser::parse_program(&mut tokenizer, "builtin.ecc".to_string()).unwrap();
  Rc::new(Linkage{
    tus: vec![Rc::new(parsed_builtins), Rc::new(ast)],
  })
}

pub fn parse(fname: String, src: String) -> Rc<Linkage> {
  let mut tokenizer = lexer::Lexer::new(src);
  let ast = parser::parse_program(&mut tokenizer, fname).unwrap();
  let res = inject_builtins(ast);
  return res
}

pub fn semantic_check(ast: &Rc<Linkage>, print_ast: i32) -> Rc<Linkage> {
  let hoisted = sema::hoist_methods(ast);
  if print_ast == 2 {
    println!("{}", &hoisted);
  }
  let type_resolved = sema::resolve_symbols(&hoisted, false);
  let res = sema::resolve_symbols(&type_resolved, true);
  if print_ast == 3 {
    println!("{}", res);
  }
  res
}

pub fn codegen_llvm(ast: &Rc<Linkage>) -> Module {
  codegen::codegen(ast)
}
