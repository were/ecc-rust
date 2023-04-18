use std::collections::HashMap;

use inkwell::module::Module;

mod codegen;
mod asm;

pub fn codegen<'ctx>(module: Module, gv_ptr2str: HashMap<usize, usize>, ofile: String) {
  codegen::codegen_module(module, gv_ptr2str, ofile);
}
