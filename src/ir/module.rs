use std::fmt;
use std::rc::Rc;

use slab::Slab;
use super::block;
use super::function;
use super::value;
use super::types;
use super::instruction;
use super::value::{
  ValueRef, VKindCode, Argument
};
use super::types::{
  Type, StructType, FunctionType
};

struct InsertPoint {
  func_id: i32,
  block_id: i32,
  inst_id: i32,
}

pub struct Module {
  mod_name: String,
  src_name: String,
  /// Manage all data structures in slab.
  pub(super) func_buffer: Slab<function::Function>,
  pub(super) struct_buffer: Slab<types::StructType>,
  pub(super) block_buffer: Slab<block::Block>,
  pub(super) inst_buffer: Slab<instruction::Instruction>,
  pub(super) arg_buffer: Slab<value::Argument>,
  insert_point: InsertPoint,
}

impl<'ctx> Module {

  /// Construct a module
  pub fn new(mod_name: String, src_name: String) -> Module {
    Module {
      mod_name,
      src_name,
      func_buffer: Slab::new(),
      struct_buffer: Slab::new(),
      block_buffer: Slab::new(),
      inst_buffer: Slab::new(),
      arg_buffer: Slab::new(),
      insert_point: InsertPoint{func_id: -1, block_id: -1, inst_id: -1}
    }
  }

  /// Add a struct without body to the module
  pub fn add_struct_decl(&mut self, name: String) {
    self.struct_buffer.insert(types::StructType::new(name));
  }

  /// Get the struct reference by name
  pub fn get_struct(&'ctx self, name: &String) -> Option<&'ctx types::StructType> {
    for (_, elem) in self.struct_buffer.iter() {
      if elem.name == *name {
        return Some(elem);
      }
    }
    None
  }

  /// Get the mutable struct reference by name
  pub fn get_struct_mut(&'ctx mut self, name: &String) -> Option<&'ctx mut types::StructType> {
    for (_, elem) in self.struct_buffer.iter_mut() {
      if elem.name == *name {
        return Some(elem);
      }
    }
    None
  }

  pub fn functions(&'ctx self) -> &'ctx Slab<function::Function> {
    &self.func_buffer
  }

  pub fn functions_mut(&'ctx mut self) -> &'ctx mut Slab<function::Function> {
    &mut self.func_buffer
  }

  /// Get the function by name
  pub fn get_function(&'ctx self, name: &String) -> Option<&'ctx function::Function> {
    for (_, elem) in &self.func_buffer {
      if elem.name == *name {
        return Some(&elem);
      }
    }
    None
  }

  /// Get the function by name
  pub fn get_function_mut(&'ctx mut self, name: &String) -> Option<&'ctx mut function::Function> {
    for (_, elem) in self.func_buffer.iter_mut() {
      if elem.name == *name {
        return Some(elem);
      }
    }
    None
  }

  /// Set insert point
  pub fn set_insert_block(&mut self, func_id: i32, block_id: i32) {
    self.insert_point.func_id = func_id;
    self.insert_point.block_id = block_id;
    self.insert_point.inst_id = 0;
  }

}

fn namify(name: &String) -> String {
  let mut res = String::new();
  name.chars().into_iter().for_each(|c| match c {
    'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => res.push(c),
    _ => res.push_str(&format!("_{:x}_", c as u32)),
  });
  res
}

impl fmt::Display for Module {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "; ModuleID = '{}'\n", self.mod_name).unwrap();
    write!(f, "source_filename = '{}'\n\n", self.src_name).unwrap();
    for (_, elem) in &self.struct_buffer {
      write!(f, "{}\n", elem.to_string()).unwrap();
    }
    write!(f, "\n").unwrap();
    for (_, func) in &self.func_buffer {
      // TODO(@were): More linkage policies
      write!(f, "define dso_local {} @{}(", func.fty.ret_ty.as_ref(), namify(&func.name)).unwrap();
      for i in 0..func.get_num_args() {
        if i != 0 {
          write!(f, ", ").unwrap();
        }
        let arg_ref = func.get_arg(i);
        let arg = arg_ref.as_typed_ref::<Argument>(self).unwrap();
        write!(f, "{} {}", arg.ty, &arg.name()).unwrap();
      }
      write!(f, ")").unwrap();
      if func.blocks.len() != 0 {
        write!(f, " {{\n").unwrap();
        for i in 0..func.get_num_blocks() {
          let block_ref = func.get_block(i);
          let block = block_ref.as_typed_ref::<block::Block>(self).unwrap();
          write!(f, "{}:\n", block.name).unwrap();
        }
        write!(f, "}}").unwrap();
      } else {
        write!(f, ";").unwrap();
      }
      write!(f, "\n\n").unwrap();
    }
    Ok(())
  }
}


