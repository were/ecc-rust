use crate::frontend::parse;
use crate::frontend::semantic_check;
use crate::frontend::codegen_llvm;
use crate::transform::optimize;

pub fn invoke(fname: String, src: String, print_ast: i32) -> Result<(), String> {
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
      let module = codegen_llvm(&ast);
      let optimized_module = optimize(module);
      println!("{}", optimized_module);
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

