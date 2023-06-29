use std::env;
use crate::frontend::parse;
use crate::frontend::semantic_check;
use crate::frontend::codegen_llvm;
use crate::transform::optimize;

fn backend() {
  env::temp_dir().to_str().unwrap().to_string();

}

pub fn invoke(fname: String, src: String, print_ast: i32) -> Result<(), String> {
  let tmpname = fname.chars().into_iter().map(
    |x| if x.is_alphanumeric() { x } else { '_' }).collect::<String>();
  println!("tmpname: {}", tmpname);
  let ast = parse(fname, src);
  let ast = match ast {
    Ok(ast) => ast,
    Err(msg) => return Err(msg)
  };
  match semantic_check(&ast, print_ast) {
    Ok(ast) => {
      if print_ast == 1 {
        println!("{}", ast);
      }
      let mut module = codegen_llvm(&ast);
      optimize(&mut module);
      println!("{}", module);
      Ok(())
    },
    Err(msg) => {
      Err(msg)
    }
  }
}

#[test]
fn test_return0() {
  let src = include_str!("../../tests/function/00-return0.ecc");
  invoke("00-return0.ecc".to_string(), src.to_string(), 0).unwrap();
}

