use std::rc::Rc;
use std::fmt;

/// Very basic integer type
#[derive(Clone)]
pub struct IntType {
  bits: i32,
}

impl IntType {
  
  /// Construct an integer type
  pub fn new(bits: i32) -> Self {
    IntType { bits }
  }

  /// Return the number of bits
  pub fn get_bits(&self) -> i32 {
    self.bits
  }
}

impl fmt::Display for IntType {

  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "i{}", self.bits)
  }

}

/// Void type
#[derive(Clone)]
pub struct VoidType {}

impl fmt::Display for VoidType {

  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "void")
  }

}

/// Pointer type
#[derive(Clone)]
pub struct PointerType {
  scalar_ty: Type,
}

impl fmt::Display for PointerType {

  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    if let Type::StructTypeRef(sty) = &self.scalar_ty {
      return write!(f, "%{}*", sty.name);
    }
    write!(f, "{}*", self.scalar_ty)
  }

}

/// Struct type
#[derive(Clone)]
pub struct StructType {
  pub(crate) name: String,
  pub(crate) attrs: Vec<Rc<Type>>,
}

/// Reference a struct type
#[derive(Clone)]
pub struct StructTypeRef {
  pub(crate) name: String,
}

impl StructType {

  pub fn new(name: String) -> Self {
    StructType {
      name,
      attrs: Vec::new(),
    }
  }

  pub fn set_body(&mut self, elements: Vec<Type>) {
    self.attrs = elements.iter().map(|ty| Rc::new(ty.clone())).collect();
  }

}

impl fmt::Display for StructType {

  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "%{} = type {{", self.name).unwrap();
    for (i, elem) in self.attrs.iter().enumerate() {
      write!(f, "{}{}", if i == 0 { " " } else { ", " },  elem.as_ref()).unwrap();
    }
    write!(f, " }}")
  }

}

/// A function signature type
#[derive(Clone)]
pub struct FunctionType {
  pub(super) args: Vec<Type>,
  pub(super) ret_ty: Rc<Type>,
}

/// The base variant of a type
#[derive(Clone)]
pub enum Type {
  IntType(Rc<IntType>),
  FunctionType(Rc<FunctionType>),
  PointerType(Rc<PointerType>),
  StructTypeRef(Rc<StructTypeRef>),
  VoidType(Rc<VoidType>),
}

impl fmt::Display for Type {

  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Type::IntType(ty) => write!(f, "{}", ty),
      Type::FunctionType(_) => todo!("FunctionType"),
      Type::PointerType(ty) => write!(f, "{}", ty),
      Type::StructTypeRef(ty) => write!(f, "{}", ty.name),
      Type::VoidType(ty) => write!(f, "{}", ty),
    }
  }

}

impl Type {

  pub fn ptr_type(&self) -> Type {
    Type::PointerType(Rc::new(PointerType {
      scalar_ty: self.clone(),
    }))
  }

}

impl Type {

  /// Return a function signature type
  pub fn fn_type(&self, args: &Vec<Type>) -> FunctionType {
    FunctionType {
      args: args.iter().map(|ty| ty.clone()).collect(),
      ret_ty: Rc::new(self.clone()),
    }
  }

}

