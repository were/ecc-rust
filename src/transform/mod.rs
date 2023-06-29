use trinity::ir::module::Module;

mod dealias;
mod dce;

pub fn optimize(module: &mut Module) {
  dealias::transform(module);
  dce::transform(module);
  // return dceed;
}
