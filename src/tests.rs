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
#[case("01-helloworld.ecc")]
fn test_e2e(#[case] fname: &str) {
  let src_path = format!("../tests/function/{}", fname);
  let mut file = File::open(&src_path).unwrap();
  let mut src = String::new();
  file.read_to_string(&mut src).unwrap();
  let meta = metadata(&src);
  invoke(fname.to_string(), src, 0).unwrap();
  let mut exec = std::process::Command::new("node")
    .arg("builtins/host.js")
    .arg("a.wasm")
    .stdin(std::process::Stdio::piped())
    .stdout(std::process::Stdio::piped())
    .spawn()
    .unwrap();
  exec.stdin.take().unwrap().write_all(meta.stdin.as_bytes()).unwrap();
  let mut output = String::new();
  exec.stdout.take().unwrap().read_to_string(&mut output).unwrap();
  let status = exec.wait().unwrap();
  assert!(status.success());
  assert_eq!(output, meta.stdout);
}

