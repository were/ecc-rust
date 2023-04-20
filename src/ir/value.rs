use super::block::Block;
use super::function::Function;
use super::instruction::Instruction;
use super::module::Module;
use super::types::Type;

#[macro_export]
macro_rules! value_impl_as_ref_and_mut {
  ($ty_name: tt, $buffer: tt) => {
    impl WithVKindCode for $ty_name {
      fn kind_code() -> VKindCode {
        VKindCode::$ty_name
      }
    }
    impl<'ctx> FindInstance<'ctx, $ty_name> for $ty_name {
      fn find_instance(module: &'ctx Module, value: &'ctx ValueRef) -> &'ctx $ty_name {
        &module.$buffer[value.skey]
      }
    }
    impl<'ctx> FindInstanceMut<'ctx, $ty_name> for $ty_name {
      fn find_instance(module: &'ctx mut Module, value: &'ctx ValueRef) -> &'ctx mut $ty_name {
        &mut module.$buffer[value.skey]
      }
    }
  };
}

#[derive(Clone)]
pub struct Argument {
  pub(super) skey: Option<usize>,
  pub(super) ty: Type,
  pub(super) arg_idx: usize,
  pub(super) parent: ValueRef
}

#[derive(Clone)]
pub struct ValueRef {
  pub skey: usize,
  pub v_kind: VKindCode
}

impl<'ctx> ValueRef {

  pub fn as_typed_ref<T: WithVKindCode + FindInstance<'ctx, T>>(&'ctx self, module: &'ctx Module) -> Option<&'ctx T> {
    if self.v_kind == T::kind_code() {
      Some(T::find_instance(module, self))
    } else {
      None
    }
  }

  pub fn as_typed_mut<T: WithVKindCode + FindInstanceMut<'ctx, T>>(&'ctx mut self, module: &'ctx mut Module) -> Option<&'ctx mut T> {
    if self.v_kind == T::kind_code() {
      Some(T::find_instance(module, self))
    } else {
      None
    }
  }

}

#[derive(Clone, PartialEq)]
pub enum VKindCode {
  Argument,
  Instruction,
  Function,
  Block,
  Unknown
}

pub trait WithVKindCode {
  fn kind_code() -> VKindCode;
}

pub trait FindInstance<'ctx, T> {
  fn find_instance(module: &'ctx Module, value: &'ctx ValueRef) -> &'ctx T;
}

pub trait TypedValueRef {
  fn get_type() -> Type;
}

pub trait FindInstanceMut<'ctx, T> {
  fn find_instance(module: &'ctx mut Module, value: &'ctx ValueRef) -> &'ctx mut T;
}


macro_rules! impl_as_ref {
  ($type:tt) => {
    impl $type {
      pub fn as_ref(&self) -> ValueRef {
        ValueRef { skey: self.skey.unwrap(), v_kind: VKindCode::$type }
      }
    }
  };
}

impl_as_ref!(Argument);
impl_as_ref!(Block);
impl_as_ref!(Function);
impl_as_ref!(Instruction);

