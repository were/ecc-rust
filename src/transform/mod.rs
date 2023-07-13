use trinity::ir::module::Module;

mod ssa;
mod dce;
mod cse;

pub fn optimize(module: Module) -> Module {
  eprintln!("{}", module);
  let (ssa, dom) = ssa::transform(module);
  let cse = cse::transform(ssa, &dom);
  cse
}

