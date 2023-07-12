use trinity::ir::module::Module;

mod ssa;
mod dce;

pub fn optimize(module: Module) -> Module {
  eprintln!("{}", module);
  let ssa = ssa::transform(module);
  ssa
}

