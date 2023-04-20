use std::rc::Rc;

use slab::Slab;

use super::{
  module::Module,
  value::{ValueRef, Argument, VKindCode},
  types::FunctionType,
  block::Block,
  function::Function,
  function,
  types
};


pub struct Builder {
  pub module: Module,
  func_id: Option<usize>,
  block_id: Option<usize>,
  inst_id: Option<usize>,
}

impl Builder {

  pub fn new(module: Module) -> Builder {
    Builder { module, func_id: None, block_id: None, inst_id: None }
  }

  /// Add a function to the module
  pub fn add_function(&mut self, name: String, fty: FunctionType) -> ValueRef {
    let args = fty.args.iter().enumerate().map(|(i, ty)| {
      let arg = Argument {
        skey: None,
        ty: ty.clone(),
        arg_idx: i,
        parent: ValueRef{skey: 0, v_kind: VKindCode::Unknown}
      };
      self.module.arg_buffer.insert(arg)
    }).collect();
    let mut func = function::Function {
      skey: None,
      name, args, fty,
      blocks: Vec::new(),
    };
    let skey = self.module.func_buffer.insert(func);
    self.module.func_buffer[skey].skey = Some(skey);
    self.module.func_buffer[skey].args.iter().for_each(|arg_ref| {
      let arg = &mut self.module.arg_buffer[*arg_ref];
      arg.skey = Some(*arg_ref);
      arg.parent = ValueRef{skey, v_kind: VKindCode::Function};
    });
    self.module.func_buffer[skey].as_ref()
  }

  /// Set the current function to insert.
  pub fn set_current_function(&mut self, func: ValueRef) {
    assert!(func.v_kind == VKindCode::Function, "Given value is not a function");
    self.func_id = Some(func.skey);
  }

  /// Add a block to the current function.
  pub fn add_block(&mut self, name: String) -> ValueRef {
    let block_name = if name != "" { name } else { format!("block{}", self.module.block_buffer.len()) };
    let skey = self.module.block_buffer.insert(Block{
      skey: None,
      name: block_name,
      insts: Vec::new(),
    });
    let block = &mut self.module.block_buffer[skey];
    block.skey = Some(skey);
    let func = &mut self.module.func_buffer[self.func_id.unwrap()];
    func.blocks.push(skey);
    block.as_ref()
  }

  /// Set the current block to insert.
  pub fn set_current_block(&mut self, block: ValueRef) {
    assert!(block.v_kind == VKindCode::Block, "Given value is not a block");
    self.block_id = Some(block.skey);
    self.inst_id = None;
  }

  /// Set the instruction as the insert point.
  pub fn set_insert_point(&mut self, inst: ValueRef) {
    assert!(inst.v_kind == VKindCode::Instruction, "Given value is not a instruction");
    self.inst_id = Some(inst.skey);
  }

  /// Get an integer type
  pub fn int_type(&self, bits: i32) -> types::Type {
    types::Type::IntType(Rc::new(types::IntType::new(bits)))
  }

  /// Get a void type
  pub fn void_type(&self) -> types::Type {
    types::Type::VoidType(Rc::new(types::VoidType{}))
  }


}

