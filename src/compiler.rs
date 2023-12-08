use std::env;
use std::io::Write;

use crate::frontend::parse;
use crate::frontend::semantic_check;
use crate::frontend::codegen_llvm;
use crate::transform::optimize;
use crate::backend;

#[derive(Debug)]
pub struct CompilerFlags {
  pub fname: String,
  pub opt_level: i32,
  pub output: String,
  pub backend: String,
  pub target: String,
  pub print_ast: i32,
  pub no_inline: bool,
  pub no_loop_unroll: bool
}

impl CompilerFlags {

  pub fn parse_flags(args: Vec<String>) -> Self {
    let mut print_ast : i32 = 0;
    let mut output: String = String::from("a.wat");
    let mut backend: String = String::from("myown");
    let mut opt_level: i32 = 2;
    let mut no_inline = false;
    let mut no_loop_unroll = false;
    let mut target = String::from("wasm");
    if args.len() < 2 {
      println!("Usage: ./ecc [file-name]");
    }
    for i in 2..args.len() {
      match args[i].as_str() {
        "--print-ast" => { print_ast = args[i + 1].parse().unwrap(); }
        "--output" => { output = args[i + 1].clone(); }
        "--backend" => { backend = args[i + 1].clone(); }
        "--opt" => { opt_level = args[i + 1].parse().unwrap(); }
        "--no-inline" => { no_inline = true; }
        "--no-loop-unroll" => { no_loop_unroll = true; }
        "--target" => { target = args[i + 1].clone(); }
        _ => ()
      }
    }
    Self {
      fname: args[1].clone(),
      opt_level,
      output,
      backend,
      print_ast,
      no_inline,
      no_loop_unroll,
      target
    }
  }

  pub fn data_layout(&self) -> String {
    String::from(match self.target.as_str() {
      "wasm" => "e-m:e-p:32:32",
      "apple-arm" => "e-m:o-i64:64-i128:128-n32:64-S128",
      _ => "",
    })
  }
  
  pub fn target_triple(&self) -> String {
    String::from(match self.target.as_str() {
      "wasm" => "wasm32-unknown-emscripten",
      "apple-arm" => "arm64-apple-macosx14.0.0",
      _ => "",
    })
  }

}


pub fn invoke(src: String, flags: &CompilerFlags) -> Result<(), String> {
  let ast = parse(flags, src);
  let ast = match ast {
    Ok(ast) => ast,
    Err(msg) => return Err(msg)
  };
  let print_ast = flags.print_ast;
  if print_ast == 1 {
    println!("{}", ast);
  }
  let ast = semantic_check(&ast, print_ast)?;
  let module = codegen_llvm(flags, &ast);
  let optimized = optimize(module, flags);
  let mangled = flags.fname.chars().into_iter().map(
    |x| if x.is_alphanumeric() { x } else { '_' }).collect::<String>();
  let tmpdir = env::temp_dir().to_str().unwrap().to_string();
  let irname = format!("{}/{}.ll", tmpdir, mangled);
  {
    eprintln!("IR dumped to: {}", irname);
    let mut fd = std::fs::File::create(&irname).unwrap();
    fd.write(format!("{}", optimized).as_bytes()).unwrap();
  }
  let backend = &flags.backend;
  let output = &flags.output;
  match flags.target.as_str() {
    "wasm" => {
      match backend.as_str() {
        "emcc" => backend::emcc_codegen(&irname, output),
        "myown" => backend::myown_codegen(&optimized, output),
        _ => panic!("Unknown backend: {}", backend)
      }
    }
    "apple-arm" => {
      match backend.as_str() {
        "clang" => backend::clang_codegen(&irname, output),
        _ => panic!("Unknown backend: {}", backend)
      }
    }
    _ => panic!("Unknown target: {}", flags.target)
  }
  Ok(())
}

