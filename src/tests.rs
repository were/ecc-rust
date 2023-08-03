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

fn run_binary(binary_name: &String, meta: &E2EMetadata) {
  // Run it
  let mut exec = std::process::Command::new("node")
    .arg("--stack-size=1048576")
    .arg("builtins/host.js")
    .arg(&binary_name)
    .stdin(std::process::Stdio::piped())
    .stdout(std::process::Stdio::piped())
    .spawn()
    .unwrap();
  exec.stdin.take().unwrap().write_all(meta.stdin.as_bytes()).unwrap();
  // Parse the reference input/output meta info in the source file
  let mut output = String::new();
  exec.stdout.take().unwrap().read_to_string(&mut output).unwrap();
  let status = exec.wait().unwrap();
  eprintln!("echo {}; status: {}", binary_name, status);
  assert!(status.success());
  assert_eq!(output, meta.stdout);
}

fn load_source(prefix: &str, fname: &String) -> (String, E2EMetadata, String) {
  let src_path = format!("{}/{}", prefix, fname);
  let mut file = File::open(&src_path).unwrap();
  let mut src = String::new();
  file.read_to_string(&mut src).unwrap();
  let meta = metadata(&src);
  assert!(fname.ends_with(".ecc"));
  let obj_output = format!("{}.o", fname[0..fname.len()-4].to_string());
  (src, meta, obj_output)
}

/// This uses emcc as backend, so we only worry about the frontend.
#[rstest]
#[case("01-return0.ecc")]
#[case("02-helloworld.ecc")]
#[case("03-crossclasses.ecc")]
#[case("04-aplusb.ecc")]
#[case("05-parsearray.ecc")]
#[case("06-method.ecc")]
#[case("07-printab.ecc")]
#[case("08-for.ecc")]
#[case("09-if.ecc")]
#[case("10-while.ecc")]
#[case("11-break.ecc")]
#[case("12-readint.ecc")]
#[case("13-print0.ecc")]
#[case("14-recursion.ecc")]
#[case("15-expr.ecc")]
#[case("16-neg.ecc")]
#[case("17-argorder.ecc")]
#[case("18-arraymove.ecc")]
#[case("19-swap.ecc")]
#[case("20-swap1.ecc")]
#[case("21-shortcircuit.ecc")]
fn test_frontend(#[case] fname: &str) {
  // Load the source file
  let (src, meta, obj_output) = load_source("../tests/function/", &fname.to_string());
  // Compile it
  invoke(&fname.to_string(), &obj_output, src, 0, &"emcc".to_string(), 2).unwrap();
  run_binary(&obj_output, &meta);
  // Compare the output
  std::process::Command::new("rm")
    .arg(obj_output)
    .spawn()
    .unwrap();
}

/// This uses my own backend, all the cases are tested end-to-end.
#[rstest]
#[case("01-return0.ecc")]
#[case("02-helloworld.ecc")]
#[case("03-crossclasses.ecc")]
#[case("04-aplusb.ecc")]
#[case("05-parsearray.ecc")]
#[case("06-method.ecc")]
#[case("07-printab.ecc")]
#[case("08-for.ecc")]
#[case("09-if.ecc")]
#[case("10-while.ecc")]
#[case("11-break.ecc")]
#[case("12-readint.ecc")]
#[case("13-print0.ecc")]
#[case("14-recursion.ecc")]
#[case("15-expr.ecc")]
#[case("16-neg.ecc")]
#[case("17-argorder.ecc")]
#[case("18-arraymove.ecc")]
#[case("19-swap.ecc")]
#[case("20-swap1.ecc")]
#[case("21-shortcircuit.ecc")]
fn test_e2e(#[case] fname: &str) {
  // Load the source file
  let (src, meta, obj_output) = load_source("../tests/function/", &fname.to_string());
  // Compile it
  invoke(&fname.to_string(), &obj_output, src, 0, &"myown".to_string(), 2).unwrap();
  run_binary(&obj_output, &meta);
  // Compare the output
  std::process::Command::new("rm")
    .arg(obj_output)
    .spawn()
    .unwrap();
}

/// This uses my own backend, all the cases are tested end-to-end.
#[rstest]
#[case("01-cse.ecc")]
#[case("02-bulgarian.ecc")]
#[case("03-inflate.ecc")]
#[case("04-hanoi.ecc")]
fn test_pressure(#[case] fname: &str) {
  // Load the source file
  let (src, meta, obj_output) = load_source("../tests/performance/", &fname.to_string());
  // Compile it
  invoke(&fname.to_string(), &obj_output, src, 0, &"myown".to_string(), 2).unwrap();
  run_binary(&obj_output, &meta);
  // Compare the output
  std::process::Command::new("rm")
    .arg(obj_output)
    .spawn()
    .unwrap();
}
