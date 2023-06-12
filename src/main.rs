use std::env;
use std::fs::File;
use std::io::Read;

mod frontend;
mod transform;

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

  let file = File::open(&args[1]);
  let mut src = String::new();
  match file {
    Ok(mut f) => {
      f.read_to_string(&mut src).unwrap();
      let ast = parse(args[1].clone(), src);
      let ast = match ast {
        Ok(ast) => ast,
        Err(msg) => return Err(msg)
      };
      match semantic_check(&ast, print_ast) {
        Ok(ast) => {
          if print_ast == 1 {
            println!("{}", ast);
          }
          let module = frontend::codegen_llvm(&ast);
          let optimized_module = transform::optimize(module);
          println!("{}", optimized_module);
          Ok(())
        },
        Err(msg) => {
          Err(msg)
        }
      }
    }
    Err(msg) => {
      eprintln!("Failed to open file: {}", msg);
      Err(msg.to_string())
    }
  }
}

