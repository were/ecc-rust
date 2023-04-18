use core::panic;
use std::collections::HashMap;

use inkwell::{
  module::Module,
  values::{FunctionValue, AnyValue, BasicValueEnum, InstructionOpcode, InstructionValue, AsValueRef, BasicValue },
  types::{AnyTypeEnum, StructType }
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

fn on_stack(ptr_inst: BasicValueEnum) -> bool {
  if let Some(inst) = ptr_inst.as_instruction_value() {
    if inst.get_opcode() == InstructionOpcode::Alloca {
      return true;
    }
  }
  return false;
}

fn struct_size(s: StructType) -> i32 {
  return (s.get_field_types().len() * 4) as i32;
  // let mut res = 0;
  // s.get_field_types().iter().for_each(|x| {
  //   res += match x {
  //     BasicTypeEnum::IntType(it) => {
  //       it.get_bit_width() / 8
  //     }
  //     BasicTypeEnum::PointerType(_) => {
  //       4
  //     }
  //     _ => { panic!("Not supported yet {}", x.print_to_string().to_str().unwrap()); }
  //   };
  // });
  // res
}

struct CodeGen {
  emit_cache: HashMap<usize, String>,
  gv_ptr2str: HashMap<usize, usize>,
  data: Vec<u8>,
  asm: String
}

impl CodeGen {

  fn codegen_expr(&mut self, expr: &BasicValueEnum) -> String {
    {
      let vkey = expr.as_value_ref() as *const _ as usize;
      if let Some(s) = self.emit_cache.get(&vkey) {
        return s.clone();
      }
    }
    if let Some(inst) = expr.as_instruction_value() {
      let key = inst.as_value_ref() as *const _ as usize;
      match self.emit_cache.get(&key) {
        Some(s) => return s.clone(),
        _ => {
        }
      }
    }
    match expr {
      BasicValueEnum::IntValue(iv) => {
        if let Some(value) = iv.get_sign_extended_constant() {
          format!("(i32.const {})", value)
        } else {
          panic!("Not supported yet {}", expr.print_to_string().to_str().unwrap());
        }
      }
      BasicValueEnum::PointerValue(pv) => {
        let pvkey = pv.as_value_ref() as *const _ as usize;
        if let Some(gv) = self.gv_ptr2str.get(&pvkey) {
          if let Some(cached_gv) = self.emit_cache.get(gv) {
            return format!("(global.get {})", cached_gv);
          } else {
            panic!("No cached str for {}", expr.print_to_string().to_str().unwrap());
          }
        }
        panic!("Not supported yet {}", expr.print_to_string().to_str().unwrap());
      }
      _ => {
        panic!("Not supported yet {}", expr.print_to_string().to_str().unwrap());
      }
    }
  }
  
  fn codegen_inst(&mut self, inst: &InstructionValue) {
    let key = inst.as_value_ref() as *const _ as usize;
    match inst.get_opcode() {
      InstructionOpcode::Call => {
        let mut params : Vec<String> = Vec::new();
        for i in 0..inst.get_num_operands() - 1 {
          let mut text = self.codegen_expr(&inst.get_operand(i).unwrap().left().unwrap());
          if text.starts_with("$arg.") || text.starts_with("$alloc.") {
            text = format!("(local.get {})", text);
          }
          params.push(text);
        }
        let f = inst.get_operand(inst.get_num_operands() - 1).unwrap().left().unwrap();
        let fkey = f.as_value_ref() as *const _ as usize;
        let callee = self.emit_cache.get(&fkey).unwrap();
        self.asm.push_str(format!("      (call {} {})\n", callee, params.join(" ")).as_str());
      }
      InstructionOpcode::Return => {
        if let Some(x) = inst.get_operand(0) {
          let ret_val = self.codegen_expr(&x.left().unwrap());
          self.asm.push_str(format!("      (return {})\n", ret_val).as_str());
        } else {
          self.asm.push_str("      (return)\n");
        }
      }
      InstructionOpcode::Alloca => { }
      InstructionOpcode::GetElementPtr => {
        if let Some(BasicValueEnum::PointerValue(x)) = inst.get_operand(0).unwrap().left() {
          let ptr_ref = x.as_value_ref() as *const _ as usize;
          if let Some(cached) = self.emit_cache.get(&ptr_ref) {
            if let AnyTypeEnum::StructType(_) = x.get_type().get_element_type() {
              // Emit local struct access
              if let Some(idx) = inst.get_operand(inst.get_num_operands() - 1) {
                if let Some(BasicValueEnum::IntValue(iv)) = idx.left() {
                  if let Some(value) = iv.get_sign_extended_constant() {
                    let res = format!("(i32.add (local.get {}) (i32.const {})) (;{};)",
                                      cached, value * 4, inst.print_to_string().to_str().unwrap());
                    // self.asm.push_str(res.as_str());
                    self.emit_cache.insert(key, res.clone());
                  } else {
                    panic!("Last value supposed to be a const, but {}", iv.print_to_string().to_str().unwrap());
                  }
                }
              } else {
                panic!("No last value");
              }
            } else {
              // Emit global variable load.
              // self.asm.push_str(format!("      get_local {}", cached).as_str());
            }
          }
        } else {
          panic!("Not supported yet {}", inst.print_to_string().to_str().unwrap());
        }
      }
      InstructionOpcode::Store => {
        let value = self.codegen_expr(&inst.get_operand(0).unwrap().left().unwrap());
        let addr = inst.get_operand(1).unwrap().left().unwrap();
        let addr_key = addr.as_value_ref() as *const _ as usize;
        let opcode = if on_stack(addr) { "local.set" } else { "i32.store" };
        let addr_text = self.emit_cache.get(&addr_key).unwrap();
        self.asm.push_str(format!("      ;; {}\n", inst.print_to_string().to_str().unwrap()).as_str());
        self.asm.push_str(format!("      ({} {} {})\n", opcode, addr_text, value).as_str());
      }
      InstructionOpcode::Load => {
        let addr = inst.get_operand(0).unwrap().left().unwrap();
        let addr_key = addr.as_value_ref() as *const _ as usize;
        let addr_text = self.emit_cache.get(&addr_key).unwrap();
        let opcode = if on_stack(addr) { "local.get" } else { "i32.load" };
        let res = format!("({} {})", opcode, addr_text);
        self.emit_cache.insert(key, res.clone());
      }
      _ => {
        panic!("other {:?}", inst.get_opcode());
      }
    }
  }

  fn generate_alloc(&mut self, inst: &InstructionValue, idx: i32) -> (bool, i32) {
    let key = inst.as_value_ref() as *const _ as usize;
    let mut res = 0;
    match inst.get_opcode() {
      InstructionOpcode::Alloca => {
        self.asm.push_str(format!("      (local $alloc.{} i32) (; {} ;)\n", idx, inst.print_to_string().to_str().unwrap()).as_str());
        self.emit_cache.insert(key, format!("$alloc.{}", idx));
        if let AnyTypeEnum::PointerType(pt) = inst.get_type() {
          if let AnyTypeEnum::StructType(sty) = pt.get_element_type() {
            res = struct_size(sty);
          }
        }
        return (true, res);
      }
      _ =>  { return (false, 0); }
    }
  }
  
  fn codegen_func(&mut self, func: &FunctionValue) {
    self.asm.push_str("  (func");
    if func.get_name().to_str().unwrap() == "main" {
      self.asm.push_str(" (export \"main\")\n");
    } else {
      self.asm.push_str(" $");
      self.asm.push_str(namify(func.get_name().to_str().unwrap()).as_str());
      self.asm.push('\n');
    }
    // TODO(@were): Support more data types or it is ok
    func.get_params().iter().enumerate().for_each(|(i, p)| {
      self.asm.push_str(format!("    (param $arg.{} i32 (; ${} ;) )\n", i, p.print_to_string().to_str().unwrap()).as_str());
      self.emit_cache.insert(p.as_value_ref() as *const _ as usize, format!("$arg.{}", i));
    });
    let mut local_cnt: i32 = func.get_params().len() as i32;
    let mut postlogue: String = String::new();
    let mut stack_acc = self.data.len() as i32;
    func.get_basic_blocks().iter().for_each(|bb| {
      let mut iter = bb.get_first_instruction();
      while let Some(inst) = iter {
        let (is_alloc, delta) = self.generate_alloc(&inst, local_cnt);
        if delta != 0 {
          let to_print = format!("      (local.set $alloc.{} (i32.const {})) (; init alloc.{} ;)\n", local_cnt, stack_acc, local_cnt);
          postlogue.push_str(&to_print.as_str());
          stack_acc += delta;
        }
        if is_alloc {
          local_cnt += 1;
        }
        iter = inst.get_next_instruction();
      }
    });
    self.asm.push_str(&postlogue.as_str());
    func.get_basic_blocks().iter().for_each(|bb| {
      let mut iter = bb.get_first_instruction();
      while let Some(inst) = iter {
        self.codegen_inst(&inst);
        iter = inst.get_next_instruction();
      }
    });
    self.asm.push_str("  )\n");
  }
  
  pub fn codegen_module(&mut self, module: &Module) {
    self.asm.push_str("(module\n"); // module start
    // Generate the i/o outputs
    self.asm.push_str("  (import \"host\" \"print_int\" (func $print_int (param i32)))\n");
    self.asm.push_str("  (import \"host\" \"print_str\" (func $print_str (param i32 i32)))\n");

    // Generate global variables
    for (i, gv) in module.get_globals().enumerate() {
      self.asm.push_str(format!("  ;; {}\n", gv.print_to_string().to_str().unwrap()).as_str());
      self.asm.push_str(format!("  (global $gv_{}", i).as_str());
      self.asm.push_str(format!(" i32 (i32.const {})", self.data.len()).as_str());
      let gv_key = gv.as_value_ref() as *const _ as usize;
      self.emit_cache.insert(gv_key, format!("$gv_{}", i));
      match gv.get_initializer() {
        Some(BasicValueEnum::ArrayValue(av)) => {
          av.get_string_constant().unwrap().to_str().unwrap().chars().for_each(|b| {
            self.data.push(b as u8);
          });
        }
        _ => {
          panic!("Not supported yet {}", gv.get_initializer().unwrap().print_to_string().to_str().unwrap());
        }
      }
      self.data.push(0);
      self.asm.push_str(")\n")
    }

    // Generate functions.
    for func in module.get_functions() {
      let fkey = func.as_value_ref() as *const _ as usize;
      self.emit_cache.insert(fkey, format!("${}", namify(func.get_name().to_str().unwrap())));
    }
    for func in module.get_functions() {
      self.codegen_func(&func)
    }

    self.asm.push_str("  (memory (export \"memory\") 65536 65536)\n");
    self.asm.push_str("  (data (i32.const 0) \"");
    self.data.iter().for_each(|c| self.asm.push_str(format!("\\{:02x}", *c as i32).as_str()));
    self.asm.push_str("\")\n");
    self.asm.push(')'); // module end
  }

}

pub fn codegen_module<'ctx>(module: Module, gv_ptr2str: HashMap<usize, usize>, ofile: String) {
  let mut cg = CodeGen {
    emit_cache: HashMap::new(),
    gv_ptr2str,
    asm: String::new(),
    data: Vec::new(),
  };
  cg.codegen_module(&module);
  std::fs::write(ofile, cg.asm).expect("Unable to write file");
}
