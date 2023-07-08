use std::io::Read;
use std::io::Write;
use std::fs::File;
use rstest::*;

use crate::compiler::invoke;

struct E2EMetadata {
  stdin: String,
  stdout: String,
}

fn metadata(src: &String) -> E2EMetadata {
  let mut stdio = vec![String::new(), String::new()];
  let mut io: i32 = -1;
  for line in src.split("\n").into_iter() {
    if line == "// [Metadata.stdin]" {
      io = 0;
      continue;
    }
    if line == "// [Metadata.stdout]" {
      io = 1;
      continue;
    }
    if line == "// [Metadata.end]" {
      io = -1;
      continue;
    }
    if io != -1 {
      stdio[io as usize].push_str(&line[3..]);
      stdio[io as usize].push_str("\n");
    }
  }
  E2EMetadata { stdin: stdio[0].clone(), stdout: stdio[1].clone(), }
}

#[rstest]
#[case("01-return0.ecc")]
#[case("02-helloworld.ecc")]
#[case("03-crossclasses.ecc")]
#[case("04-aplusb.ecc")]
#[case("05-array.ecc")]
#[case("06-method.ecc")]
#[case("07-printab.ecc")]
#[case("08-for.ecc")]
#[case("09-if.ecc")]
#[case("10-while.ecc")]
#[case("11-break.ecc")]
#[case("12-readint.ecc")]
fn test_e2e(#[case] fname: &str) {
  let src_path = format!("../tests/function/{}", fname);
  let mut file = File::open(&src_path).unwrap();
  let mut src = String::new();
  file.read_to_string(&mut src).unwrap();
  let meta = metadata(&src);
  assert!(fname.ends_with(".ecc"));
  let obj_output = format!("{}.o", fname[0..fname.len()-4].to_string());
  // Compile it
  invoke(&fname.to_string(), &obj_output, src, 0).unwrap();
  // Run it
  let mut exec = std::process::Command::new("node")
    .arg("builtins/host.js")
    .arg(&obj_output)
    .stdin(std::process::Stdio::piped())
    .stdout(std::process::Stdio::piped())
    .spawn()
    .unwrap();
  exec.stdin.take().unwrap().write_all(meta.stdin.as_bytes()).unwrap();
  // Parse the reference input/output meta info in the source file
  let mut output = String::new();
  exec.stdout.take().unwrap().read_to_string(&mut output).unwrap();
  let status = exec.wait().unwrap();
  assert!(status.success());
  // Compare the output
  assert_eq!(output, meta.stdout);
  std::process::Command::new("rm")
    .arg(obj_output)
    .spawn()
    .unwrap();
}

