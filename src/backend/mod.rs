use std::{io::Write, env};

use trinity::ir::module::Module;

mod wasm;

pub fn myown_codegen(module: &Module, output: &String) {
  let mut cg = wasm::Codegen::new(module);
  let asm = cg.emit();
  let tmpdir = env::temp_dir().to_str().unwrap().to_string();
  let wat = format!("{}/{}.wat", tmpdir, output);
  eprintln!("ASM text output to: {}", wat);
  {
    let mut fd = std::fs::File::create(&wat).unwrap();
    fd.write(format!("{}", asm).as_bytes()).unwrap();
  }
  eprintln!("Binary output to: {}", output);
  // wat2wasm a.wat
  let assemble = std::process::Command::new("wat2wasm")
    .arg(&wat)
    .arg("-o")
    .arg(output)
    .output()
    .expect("failed to execute process");
  assert!(assemble.status.success());
}

pub fn emcc_codegen(irname: &String, output: &String) {
  eprintln!("Output to: {}", output);
  assert!(irname.ends_with(".ll"));
  let objname = irname[0..irname.len()-3].to_string() + ".o";
  // emcc a.ll -c
  if let Ok(linker) = std::process::Command::new("emcc")
    .arg("-mllvm")
    .arg("--disable-loop-idiom-all")
    .arg("-O3")
    .arg("-c")
    .arg("-o")
    .arg(&objname)
    .arg(&irname)
    .output() {
    linker.stderr.iter().for_each(|x| eprint!("{}", *x as char));
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
