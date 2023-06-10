use trinity::ir::module::Module;

mod dealias;
mod dce;

pub fn optimize(module: Module) -> Module {
  let dealiased = dealias::transform(module);
  let dceed = dce::transform(dealiased);
  return dceed;
}
