use crate::value_impl_as_ref_and_mut;

use super::module::Module;
use super::value::{ValueRef, VKindCode, WithVKindCode, FindInstance, FindInstanceMut};

pub struct Block {
  pub(super) skey: Option<usize>,
  pub(super) name: String,
  pub(super) insts: Vec<usize>,
}

value_impl_as_ref_and_mut!(Block, block_buffer);
