use std::env;
use std::io::Write;

use crate::frontend::parse;
use crate::frontend::semantic_check;
use crate::frontend::codegen_llvm;
use crate::transform::optimize;
use crate::backend;


pub fn invoke(
  fname: &String,
  output: &String,
  src: String,
  print_ast: i32,
  backend: &String,
  opt_level: i32) -> Result<(), String> {
  let ast = parse(&fname, src);
  let ast = match ast {
    Ok(ast) => ast,
    Err(msg) => return Err(msg)
  };
  if print_ast == 1 {
    println!("{}", ast);
  }
  let ast = semantic_check(&ast, print_ast)?;
  let module = codegen_llvm(&ast);
  let optimized = optimize(module, opt_level);
  let mangled = fname.chars().into_iter().map(
    |x| if x.is_alphanumeric() { x } else { '_' }).collect::<String>();
  let tmpdir = env::temp_dir().to_str().unwrap().to_string();
  let irname = format!("{}/{}.ll", tmpdir, mangled);
  {
    eprintln!("IR dumped to: {}", irname);
    let mut fd = std::fs::File::create(&irname).unwrap();
    fd.write(format!("{}", optimized).as_bytes()).unwrap();
  }
  if backend == "emcc" {
    backend::emcc_codegen(&irname, output);
  } else if backend == "myown" {
    backend::myown_codegen(&optimized, output);
  } else {
    panic!("Unknown backend: {}", backend);
  }
  Ok(())
}

