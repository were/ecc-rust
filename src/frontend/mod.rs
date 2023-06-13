use std::rc::Rc;

use trinity::ir::module::Module;

use self::ast::Linkage;

mod lexer;
mod ast;
mod parser;
mod printer;
mod visitor;
mod sema;
mod codegen;

pub fn inject_builtins(ast: ast::TranslateUnit) -> Rc<Linkage> {
  #[cfg(feature = "x86")]
  let asm = include_str!("../../builtins/x86.ecc");
  #[cfg(feature = "wasm")]
  let asm = include_str!("../../builtins/wasm.ecc");
  #[cfg(feature = "riscv")]
  let asm = include_str!("../../builtins/riscv.ecc");
  let mut builtins = include_str!("../../builtins/builtin.ecc").to_string();
  builtins.push_str(asm);
  let mut tokenizer = lexer::Lexer::new(builtins);
  let parsed_builtins = parser::parse_program(&mut tokenizer, "builtin.ecc".to_string()).unwrap();
  Rc::new(Linkage{
    tus: vec![Rc::new(parsed_builtins), Rc::new(ast)],
  })
}

pub fn parse(fname: String, src: String) -> Result<Rc<Linkage>, String> {
  let mut tokenizer = lexer::Lexer::new(src);
  match parser::parse_program(&mut tokenizer, fname) {
    Ok(ast) => {
      return Ok(inject_builtins(ast));
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

pub fn codegen_llvm(ast: &Rc<Linkage>) -> Module {
  #[cfg(feature = "wasm")]
  let tt = "wasm32-unknown-emscripten";
  #[cfg(feature = "wasm")]
  let layout = "e-m:e-p:32:32";
  codegen::codegen(ast, tt.to_string(), layout.to_string())
}
