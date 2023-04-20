use crate::value_impl_as_ref_and_mut;

use super::value::{ValueRef, VKindCode, WithVKindCode, FindInstance, FindInstanceMut};
use super::module::Module;

pub struct Instruction {
  pub(super) skey: Option<usize>,
  pub(super) name: String,
  pub(super) operands: Vec<ValueRef>,
  pub(super) parent: ValueRef,
}

value_impl_as_ref_and_mut!(Instruction, inst_buffer);
