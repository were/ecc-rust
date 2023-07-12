use trinity::ir::module::Module;

mod ssa;
mod dce;
mod mem2reg;

pub fn optimize(module: Module) -> Module {
  // eprintln!("{}", module);
  let p1 = mem2reg::transform(module);
  let p2 = ssa::transform(p1);
  p2
}

