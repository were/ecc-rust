use trinity::ir::module::Module;

mod ssa;
mod dce;
mod cse;
mod simplify;

pub fn optimize(mut module: Module) -> Module {
  // eprintln!("{}", module);
  simplify::const_propagate(&mut module);
  let (ssa, dom) = ssa::transform(module);
  let cse = cse::transform(ssa, &dom);
  cse
}

