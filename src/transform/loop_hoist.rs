use trinity::ir::module::Module;

use crate::analysis::topo::analyze_topology;

fn has_loop_invariant(m: &Module, visited: &mut Vec<bool>) {
  for f in m.func_iter() {
    let topo = analyze_topology(&f, visited);
  }
}

fn hoist_loop_invariants(m: &mut Module) {
  let mut visited = vec![false; m.context.capacity()];
}
