use std::env;
use std::fs::File;
use std::io::Read;

mod frontend;

pub use crate::frontend::parse;

fn main() {
  let args: Vec<String> = env::args().collect();
  if args.len() < 2 {
    println!("Usage: ./ecc [file-name]");
  }
  let file = File::open(&args[1]);
  let mut src = String::new();
  match file {
    Ok(mut f) => {
      f.read_to_string(&mut src);
      parse(&src);
    }
    Err(error) => {
      eprintln!("Failed to open file: {}", error)
    }
  }
}

