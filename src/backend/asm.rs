use std::rc::Rc;

struct Asm {
  opcode: String,
  dtype: String,
  operands: Vec<Rc<Asm>>,
}

