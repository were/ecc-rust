use std::fmt;
use crate::value_impl_as_ref_and_mut;

use super::block::Block;
use super::module::Module;
use super::value::{WithVKindCode, FindInstance, FindInstanceMut};

use super::{
  value::{Argument, ValueRef, VKindCode},
  types::FunctionType
};

pub struct Function {
  pub(super) skey: Option<usize>,
  pub(super) name: String,
  pub(super) args: Vec<usize>,
  pub(super) fty: FunctionType,
  pub(super) blocks: Vec<usize>,
}

impl Function {

  pub fn get_num_args(&self) -> usize {
    return self.args.len();
  }

  pub fn get_arg(&self, i: usize) -> ValueRef {
    return ValueRef{skey: self.args[i], v_kind: VKindCode::Argument};
  }

  pub fn get_num_blocks(&self) -> usize {
    return self.blocks.len();
  }

  pub fn get_block(&self, i: usize) -> ValueRef {
    return ValueRef{skey: self.blocks[i], v_kind: VKindCode::Block};
  }

}

value_impl_as_ref_and_mut!(Function, func_buffer);

impl Argument {

  pub fn name(&self) -> String {
    format!("%arg.{}", self.arg_idx)
  }

}

value_impl_as_ref_and_mut!(Argument, arg_buffer);



impl fmt::Display for Argument {

  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{} {}", self.ty, self.name())
  }

}
