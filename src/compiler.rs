use std::env;
use std::io::Write;

use crate::frontend::parse;
use crate::frontend::semantic_check;
use crate::frontend::codegen_llvm;
use crate::transform::optimize;

fn backend(irname: &String, output: &String) {
  eprintln!("Output to: {}", output);
  assert!(irname.ends_with(".ll"));
  let objname = irname[0..irname.len()-3].to_string() + ".o";
  // emcc a.ll -c
  if let Ok(linker) = std::process::Command::new("emcc")
    .arg("-O0")
    .arg("-c")
    .arg("-o")
    .arg(&objname)
    .arg(&irname)
    .output() {
    assert!(linker.status.success());
  } else {
    panic!("emcc not found, did you `source setup.sh`?");
  }
  // wasm2wat a.o | sed "s/func \$main/func (export \"main\")/" > a.wat
  let disassemble = std::process::Command::new("wasm2wat")
    .arg(&objname)
    .stdout(std::process::Stdio::piped())
    .spawn()
    .expect("failed to execute process");
  let hacker = std::process::Command::new("sed")
    .arg("s/func \\$main/func (export \"main\")/")
    .stdin(disassemble.stdout.unwrap())
    .stdout(std::process::Stdio::piped())
    .spawn()
    .expect("failed to execute process");
  // wat2wasm a.wat
  let assemble = std::process::Command::new("wat2wasm")
    .arg("-")
    .arg("-o")
    .arg(output)
    .stdin(hacker.stdout.unwrap())
    .output()
    .expect("failed to execute process");
  assert!(assemble.status.success());
}

pub fn invoke(fname: &String, output: &String, src: String, print_ast: i32) -> Result<(), String> {
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
  let optimized = optimize(module);
  let mangled = fname.chars().into_iter().map(
    |x| if x.is_alphanumeric() { x } else { '_' }).collect::<String>();
  let tmpdir = env::temp_dir().to_str().unwrap().to_string();
  let irname = format!("{}/{}.ll", tmpdir, mangled);
  {
    eprintln!("IR dumped to: {}", irname);
    let mut fd = std::fs::File::create(&irname).unwrap();
    fd.write(format!("{}", optimized).as_bytes()).unwrap();
  }
  backend(&irname, output);
  Ok(())
}

