use trinity::ir::module::Module;

mod ssa;
mod dce;
mod mem2reg;

pub fn optimize(module: Module) -> Module {
  let p1 = mem2reg::transform(module);
  let p2 = ssa::transform(p1);
  let p3 = dce::transform(p2);
  // return dceed;
  p3
}
