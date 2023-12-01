use std::env;
use std::fs::File;
use std::io::Read;

mod frontend;
mod transform;
mod compiler;
mod backend;
mod analysis;
#[cfg(test)]
mod tests;

use compiler::CompilerFlags;

pub use crate::frontend::parse;
pub use crate::frontend::semantic_check;

fn main() -> Result<(), String> {
  let args: Vec<String> = env::args().collect();
  let compiler_flags = CompilerFlags::parse_flags(args);
  let mut file = File::open(compiler_flags.fname.clone()).unwrap();
  let mut src = String::new();
  file.read_to_string(&mut src).unwrap();
  compiler::invoke(src, &compiler_flags)
}

