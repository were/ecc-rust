use std::{collections::HashMap, rc::Rc};

use trinity::{
  ir::{TypeRef, StructType, module::Module},
  builder::Builder,
  context::Context
};

use crate::frontend::ast;

#[derive(Debug, Clone)]
pub(super) enum CGType {
  Pointer(Box<CGType>),
  Type(TypeRef),
}

impl From<TypeRef> for CGType {

  fn from(t: TypeRef) -> Self {
    CGType::Type(t)
  }

}

impl CGType {

  pub(super) fn unwrap(&self) -> TypeRef {
    match self {
      Self::Pointer(_) => { panic!("Cannot get the raw type!") },
      Self::Type(ty) => ty.clone(),
    }
  }

  pub(super) fn get_pointee_ty(&self) -> CGType {
    match self {
      Self::Pointer(ty) => ty.as_ref().clone(),
      Self::Type(_) => panic!("Not a pointer type!")
    }
  }

  pub(super) fn to_llvm(&self, ctx: &mut Context) -> TypeRef {
    match self {
      Self::Pointer(_) => {
        ctx.pointer_type()
      }
      Self::Type(ty) => {
        ty.clone()
      }
    }
  }

}

pub(super) struct TypeGen {
  pub(super) builder: Builder,
  field_pointee: HashMap<(TypeRef, usize), CGType>,
  class_cache: HashMap<String, TypeRef>,
}

impl TypeGen {

  pub(super) fn new(ast: &Rc<ast::Linkage>, m: Module) -> Self {
    let mut builder = Builder::new(m);
    let mut class_cache: HashMap<String, TypeRef> = HashMap::new();
    // Declare all classes first to get prepared for potential cross references.
    for tu in ast.tus.iter() {
      for decl in &tu.decls {
        if let ast::Decl::Class(class) = decl {
          let sty_ref = builder.create_struct(class.id().clone());
          // Note: We insert the struct type into the cache, NOT the pointer of the struct type.
          class_cache.insert(class.id().clone(), sty_ref);
        }
      }
    }

    let res = TypeGen {
      builder,
      class_cache,
      field_pointee: HashMap::new(),
    };
    res
  }

  pub(super) fn register_struct_field(&mut self, sty: TypeRef, i: usize, ty: CGType) {
    self.field_pointee.insert((sty, i), ty);
  }

  pub(super) fn enter_linkage(&mut self, ast: &Rc<ast::Linkage>) {
    assert_eq!(ast.tus.len(), 2);
    for tu in &ast.tus {
      self.generate_translation_unit(&tu);
    }
  }

  pub(super) fn generate_translation_unit(&mut self, tu: &Rc<ast::TranslateUnit>) {
    for decl in &tu.decls {
      match decl {
        ast::Decl::Class(class) => {
          self.class_to_struct(&class);
        }
        _ => {}
      }
    }
  }

  pub(super) fn get_class(&self, id: &String) -> Option<TypeRef> {
    self.class_cache.get(id).map(|ty| ty.clone())
  }

  pub(super) fn get_struct_field(&self, ty: TypeRef, i: usize) -> CGType {
    if let Some(ty) = self.field_pointee.get(&(ty.clone(), i)) {
      ty.clone()
    } else {
      let sty = ty.as_ref::<StructType>(&self.builder.module.context).unwrap();
      sty.get_attr(i).into()
    }
  }

  pub(super) fn generate_type(&mut self, ty: &ast::Type) -> CGType {
    match ty {
      ast::Type::Class(class) => {
        // NOTE: Here we generate a pointer type for class type
        let res = self.class_cache.get(&class.id.literal).unwrap();
        return CGType::Pointer(Box::new(res.clone().into()));
      }
      ast::Type::Builtin(builtin) => {
        let ty = self.builtin_to_llvm(builtin.as_ref());
        ty.into()
      }
      ast::Type::Array(array) => {
        return self.array_to_llvm(array).into();
      }
    }
  }

  pub(super) fn class_to_struct(&mut self, class: &Rc<ast::ClassDecl>) {
    let attrs = class.attrs.iter().map(|attr| { self.generate_type(&attr.ty) }).collect();
    let sty = self.class_cache.get(&class.id.literal).unwrap();
    self.struct_set_body(sty.clone(), attrs);
    // let sty = self.class_cache.get(&class.id.literal).unwrap();
    // eprintln!("{}", sty.as_ref::<StructType>(&self.builder.module.context).unwrap().to_string());
  }

  pub(super) fn builtin_to_llvm(&mut self, builtin : &ast::BuiltinType) -> TypeRef {
    match builtin.code {
      ast::BuiltinTypeCode::Int => self.builder.context().int_type(32),
      ast::BuiltinTypeCode::Char => self.builder.context().int_type(8),
      ast::BuiltinTypeCode::Void => self.builder.context().void_type(),
      ast::BuiltinTypeCode::Bool => self.builder.context().int_type(1),
      _ => { panic!("Unknown builtin type {}", builtin.token.literal); }
    }
  }

  pub(super) fn create_struct(&mut self, id: String) -> TypeRef {
    let res = self.builder.create_struct(id.clone());
    self.class_cache.insert(id.clone(), res.clone());
    return res
  }

  fn struct_set_body(&mut self, sty: TypeRef, fields: Vec<CGType>) {
    let ptr = self.builder.context().pointer_type();
    let body = fields.into_iter().enumerate().map(|(i, attr)| {
      match attr {
        CGType::Pointer(_) => {
          self.register_struct_field(sty.clone(), i, attr.clone());
          ptr.clone()
        }
        CGType::Type(ty) => ty
      }
    }).collect();
    let sty_mut = sty.as_mut::<StructType>(self.builder.context()).unwrap();
    sty_mut.set_body(body);
  }

  pub(super) fn array_to_llvm(&mut self, array: &ast::ArrayType) -> CGType {
    let scalar_ty = self.generate_type(&array.scalar_ty);
    self.generate_array_runtime(scalar_ty, array.dims.len())
  }

  pub(super) fn generate_array_runtime(&mut self, scalar_ty: CGType, n: usize) -> CGType {
    let ty_name = Self::extract_type_name(&self.builder.module.context, &scalar_ty);
    let mut pointee = scalar_ty;
    for i in 0..n {
      let dim = i + 1;
      let id = format!("__arrayof.{}.{}d__", ty_name, dim);
      pointee = if let Some(ty) = self.class_cache.get(&id) {
        ty.clone().into()
      } else {
        let aty = self.create_struct(id.clone());
        let i32ty = self.builder.context().int_type(32);
        self.struct_set_body(aty.clone(), vec![
          CGType::Type(i32ty.clone()),
          CGType::Pointer(pointee.into())
        ]);
        aty.into()
      };
      pointee = CGType::Pointer(pointee.into());
    }
    pointee
  }

  pub(super) fn extract_type_name(ctx: &Context, ty: &CGType) -> String {
    match ty {
      CGType::Type(x) => {
        x.to_string(ctx)
        .chars()
        .filter(|c| *c != ' ' && *c != '%')
        .collect::<_>()
      }
      CGType::Pointer(ty) => {
        format!("ptr.{}", Self::extract_type_name(ctx, ty.as_ref()))
      }
    }
  }

}

