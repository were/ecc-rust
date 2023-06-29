use std::env;
use std::fs::File;
use std::io::Read;

mod frontend;
mod transform;
mod compiler;
mod tests;

pub use crate::frontend::parse;
pub use crate::frontend::semantic_check;

fn main() -> Result<(), String> {
  let args: Vec<String> = env::args().collect();
  let mut print_ast : i32 = 0;
  if args.len() < 2 {
    println!("Usage: ./ecc [file-name]");
  }
  for i in 2..args.len() {
    match args[i].as_str() {
      "--print-ast" => { print_ast = args[i + 1].parse().unwrap(); }
      _ => ()
    }
  }

  let mut file = File::open(&args[1]).unwrap();
  let mut src = String::new();
  file.read_to_string(&mut src).unwrap();
  compiler::invoke(args[1].clone(), src, print_ast)
}

