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
      no_loop_unroll
    }
  }

}


pub fn invoke(src: String, flags: &CompilerFlags) -> Result<(), String> {
  let fname = &flags.fname;
  let ast = parse(fname, src);
  let ast = match ast {
    Ok(ast) => ast,
    Err(msg) => return Err(msg)
  };
  let print_ast = flags.print_ast;
  if print_ast == 1 {
    println!("{}", ast);
  }
  let ast = semantic_check(&ast, print_ast)?;
  let module = codegen_llvm(&ast);
  let opt_level = flags.opt_level;
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
  let backend = &flags.backend;
  let output = &flags.output;
  if backend.eq("emcc") {
    backend::emcc_codegen(&irname, output);
  } else if backend.eq("myown") {
    backend::myown_codegen(&optimized, output);
  } else {
    panic!("Unknown backend: {}", backend);
  }
  Ok(())
}

