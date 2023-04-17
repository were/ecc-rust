use std::env;
use std::fs::File;
use std::io::Read;

mod frontend;

use inkwell::context::Context;

pub use crate::frontend::parse;
pub use crate::frontend::semantic_check;

fn main() {
  let args: Vec<String> = env::args().collect();
  if args.len() < 2 {
    println!("Usage: ./ecc [file-name]");
  }

  let file = File::open(&args[1]);
  let mut src = String::new();
  match file {
    Ok(mut f) => {
      f.read_to_string(&mut src).unwrap();
      let mut ast = parse(args[1].clone(), src);
      ast = semantic_check(&ast);
      println!("{}", ast);
      let ctx = Context::create();
      frontend::codegen(&ast, &ctx);
    }
    Err(error) => {
      eprintln!("Failed to open file: {}", error)
    }
  }
}

