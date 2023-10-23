use trinity::ir::module::Module;

use crate::analysis::topo::analyze_topology;

fn has_loop_invariant(m: &Module, visited: &mut Vec<bool>) -> Option<usize> {
  for f in m.func_iter() {
    let topo = analyze_topology(&f, visited);
  }
  None
}

fn hoist_loop_invariants(m: &mut Module) {
  let mut visited = vec![false; m.context.capacity()];
  while let Some(invariant) = has_loop_invariant(m, &mut visited) {
  }
}
