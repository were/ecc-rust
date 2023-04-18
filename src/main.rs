#![feature(strict_provenance)]

use std::env;
use std::fs::File;
use std::io::Read;

mod frontend;
mod backend;

use inkwell::{
  context::Context,
};

pub use crate::frontend::parse;
pub use crate::frontend::semantic_check;

fn main() {
  let args: Vec<String> = env::args().collect();
  let mut ofile: String = "a.wat".to_string();
  let mut print_ast : i32 = 0;
  let mut emit_llvm : i32 = 0;
  if args.len() < 2 {
    println!("Usage: ./ecc [file-name]");
  }
  for i in 2..args.len() {
    match args[i].as_str() {
      "-o" => { ofile = args[i + 1].clone(); }
      "--output" => { ofile = args[i + 1].clone(); }
      "--print-ast" => { print_ast = args[i + 1].parse().unwrap(); }
      "--emit-llvm" => { emit_llvm = args[i + 1].parse().unwrap(); }
      _ => ()
    }
  }

  let file = File::open(&args[1]);
  let mut src = String::new();
  match file {
    Ok(mut f) => {
      let ctx = Context::create();
      f.read_to_string(&mut src).unwrap();
      let mut ast = parse(args[1].clone(), src);
      if print_ast == 1 {
        println!("{}", ast);
      }
      ast = semantic_check(&ast, print_ast);
      let (mut module, mut gv_ptr2str) = frontend::codegen(&ast, &ctx);
      if emit_llvm == 1 {
        module.print_to_stderr();
      }
      backend::codegen(module, gv_ptr2str, ofile.clone());
    }
    Err(error) => {
      eprintln!("Failed to open file: {}", error)
    }
  }
  ()
}

