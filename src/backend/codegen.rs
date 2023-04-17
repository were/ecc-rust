use inkwell::{
  module::Module,
  values::{FunctionValue, AnyValue, BasicValueEnum, InstructionOpcode}
};

fn namify(s: &str) -> String {
  let mut res = String::new();
  for c in s.chars() {
    if c.is_digit(10) || c.is_alphabetic() || c == '_' {
      res.push(c);
    } else {
      res.push_str(format!("_{}_", c as u32).as_str());
    }
  }
  res
}

fn codegen_func(func: &FunctionValue, res: &mut String) {
  res.push_str("  (func");
  if func.get_name().to_str().unwrap() == "main" {
    res.push_str(" (export \"main\") ");
  } else {
    res.push_str(" $");
    res.push_str(namify(func.get_name().to_str().unwrap()).as_str());
    res.push(' ');
  }
  res.push_str("(param"); // parameters
  // TODO(@were): Support more data types or it is ok?
  func.get_params().iter().for_each(|p| {
    res.push_str(format!(" i32 (; {} ;)", p.print_to_string().to_str().unwrap()).as_str());
  });
  res.push_str(")"); // parameter ends
  if let Some(_) = func.get_type().get_return_type() {
    res.push_str(" (result i32)\n");
  } else {
    res.push_str("\n");
  }
  func.get_basic_blocks().iter().for_each(|bb| {
    let mut iter = bb.get_first_instruction();
    while let Some(inst) = iter {
      match inst.get_opcode() {
        InstructionOpcode::Call => {
          println!("TODO: call {}", inst.get_num_operands());
        }
        InstructionOpcode::Return => {
          if let Some(_) = inst.get_operand(0) {
            res.push_str("    (i32.const 0)\n");
          }
          res.push_str("    return\n");
        }
        _ => {
          println!("other {:?}", inst.get_opcode());
        }
      }
      println!("{}", inst.print_to_string().to_str().unwrap());
      iter = inst.get_next_instruction();
    }
  });

  res.push_str("  )\n");
}

pub fn codegen_module(module: &Module, ofile: &String) {
  let mut res: String = String::new();
  let mut data: Vec<u8> = Vec::new();
  res.push_str("(module\n"); // module start
  // Generate the i/o outputs
  res.push_str("  (import \"host\" \"print_int\" (func $print_int (param i32)))\n");
  res.push_str("  (import \"host\" \"print_str\" (func $print_str (param i32 i32)))\n");
  for func in module.get_functions() {
    codegen_func(&func, &mut res)
  }
  for (i, gv) in module.get_globals().enumerate() {
    res.push_str(format!("  ;; {}\n", gv.print_to_string().to_str().unwrap()).as_str());
    res.push_str(format!("  (global $gv_{}", i).as_str());
    res.push_str(format!(" i32 (i32.const {})", data.len()).as_str());
    match gv.get_initializer() {
      Some(BasicValueEnum::ArrayValue(av)) => {
        av.get_string_constant().unwrap().to_str().unwrap().chars().for_each(|b| {
          data.push(b as u8);
        });
      }
      _ => {
        panic!("Not supported yet {}", gv.get_initializer().unwrap().print_to_string().to_str().unwrap());
      }
    }
    data.push(0);
    res.push_str(")\n")
  }
  res.push_str("  (memory (export \"memory\") 65536 65536)\n");
  res.push_str("  (data (i32.const 0) \"");
  data.iter().for_each(|c| res.push_str(format!("\\{:02x}", *c as i32).as_str()));
  res.push_str("\")\n");
  res.push(')'); // module end
  std::fs::write(ofile, res).expect("Unable to write file");
}
