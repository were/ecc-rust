use std::rc::Rc;

use trinity::ir::module::Module;

use crate::compiler::CompilerFlags;

use self::ast::Linkage;

mod lexer;
mod ast;
mod parser;
mod printer;
mod visitor;
mod sema;
mod codegen;

pub fn inject_builtins(flags: &CompilerFlags, ast: ast::TranslateUnit) -> Rc<Linkage> {
  let asm = match flags.target.as_str() {
    "x86" => include_str!("../../builtins/x86.ecc"),
    "wasm" => include_str!("../../builtins/wasm.ecc"),
    "riscv" => include_str!("../../builtins/riscv.ecc"),
    "apple-arm" => include_str!("../../builtins/arm.ecc"),
    _ => ""
  };
  let mut builtins = include_str!("../../builtins/builtin.ecc").to_string();
  builtins.push_str(asm);
  let mut tokenizer = lexer::Lexer::new(builtins);
  let parsed_builtins = parser::parse_program(&mut tokenizer, "builtin.ecc".to_string()).unwrap();
  Rc::new(Linkage{
    tus: vec![Rc::new(parsed_builtins), Rc::new(ast)],
  })
}

pub fn parse(flags: &CompilerFlags, src: String) -> Result<Rc<Linkage>, String> {
  let mut tokenizer = lexer::Lexer::new(src);
  match parser::parse_program(&mut tokenizer, flags.fname.clone()) {
    Ok(ast) => {
      return Ok(inject_builtins(flags, ast));
    }
    Err(msg) => {
      Err(msg)
    }
  }
}

pub fn semantic_check(ast: &Rc<Linkage>, print_ast: i32) -> Result<Rc<Linkage>, String> {
  let hoisted = sema::hoist_methods(ast);
  if print_ast == 2 {
    println!("{}", &hoisted);
  }
  match sema::resolve_symbols(&hoisted, false) {
    Ok(type_resolved) => {
      let res = sema::resolve_symbols(&type_resolved, true).unwrap();
      if print_ast == 3 {
        println!("{}", res);
      }
      Ok(res)
    },
    Err(msg) => {
      Err(msg)
    }
  }
}

pub fn codegen_llvm(flags: &CompilerFlags, ast: &Rc<Linkage>) -> Module {
  codegen::codegen(ast, flags.target_triple(), flags.data_layout())
}
