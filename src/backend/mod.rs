use inkwell::module::Module;

mod codegen;

pub fn codegen(module: &Module, ofile: &String) {
  codegen::codegen_module(module, ofile);
}
