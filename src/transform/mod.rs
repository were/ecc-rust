use trinity::ir::module::Module;

mod ssa;
mod dce;

pub fn optimize(module: &mut Module) {
  ssa::transform(module);
  // dce::transform(module);
  // return dceed;
}
