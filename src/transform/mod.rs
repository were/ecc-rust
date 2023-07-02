use trinity::ir::module::Module;

mod ssa;
mod dce;

pub fn optimize(module: Module) -> Module {
  ssa::transform(module)
  // dce::transform(module);
  // return dceed;
}
