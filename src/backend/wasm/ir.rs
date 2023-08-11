use std::collections::HashMap;

use trinity::ir::value::instruction::{CmpPred, BinaryOp};

pub(super) enum WASMOpcode {
  /// The begin of the block.
  BlockBegin(String),
  /// The begin of the block.
  LoopBegin(String),
  /// The end of the block.
  BlockEnd,
  /// Jump to the block.
  Br(String),
  /// Jump to the block with condition.
  BrIf(String),
  /// Plain string.
  Plain(String),
  /// Const (is_int, dbits, int).
  Const(bool, usize, u64),
  /// Compare inst (dtype, pred).
  Compare(i32, CmpPred),
  /// Binary operator.
  Binary(BinaryOp),
  /// Local load instruction.
  LocalGet(String),
  /// Local store instruction.
  LocalSet(String),
  /// Local store instruction.
  Call(String),
  /// Store value to memory.
  Store(usize),
  /// Load value to memory.
  Load(usize),
  /// Return instruction.
  Return,
}

pub(super) struct WASMFunc {
  name: String,
  args: Vec<String>,
  insts: Vec<WASMInst>,
  pub(super) locals: HashMap<usize, String>,
  rty: String
}

impl WASMFunc {

  pub(super) fn push(&mut self, inst: WASMInst) {
    if let Some(last_inst) = self.insts.last() {
      if let WASMOpcode::BlockBegin(_) = last_inst.opcode {
        if let WASMOpcode::BlockEnd = inst.opcode {
          // Remove a trivial block.
          self.insts.pop();
          return;
        }
      }
    }
    self.insts.push(inst);
  }

  pub(super) fn extend(&mut self, insts: Vec<WASMInst>) {
    self.insts.extend(insts);
  }

  pub(super) fn new(name: String, args: Vec<String>, rty: String) -> Self {
    Self {
      name,
      args,
      insts: Vec::new(),
      locals: HashMap::new(),
      rty
    }
  }

  pub(super) fn to_string(&self) -> String {
    let mut indent = 2;
    let mut res = if self.name == "main" {
      " (func (export \"main\")\n".to_string()
    } else {
      format!(" (func ${}\n", self.name)
    };
    res.push_str(self.args.iter().map(|x| format!("  (param ${} i32)", x)).collect::<Vec<String>>().join("\n").as_str());
    res.push('\n');
    if !self.rty.is_empty() {
      res.push_str("  (result i32)\n");
    }
    for (_, local) in self.locals.iter() {
      res.push_str(format!("  (local ${} i32)\n", local).as_str());
    }
    for elem in self.insts.iter() {
      res.push_str((elem.to_string(&mut indent) + "\n").as_str());
    }
    res.push_str(" )\n");
    return res;
  }
}

pub(super) struct WASMInst {
  _skey: usize,
  opcode: WASMOpcode,
  operands: Vec<Box<WASMInst>>,
  pub(super) comment: String,
}

impl WASMInst {

  pub(super) fn loop_begin(skey: usize, label: String) -> WASMInst {
    WASMInst {
      _skey: skey,
      opcode: WASMOpcode::LoopBegin(label),
      operands: Vec::new(),
      comment: String::new(),
    }
  }

  pub(super) fn store(skey: usize, bits: usize, value: WASMInst, addr: WASMInst) -> WASMInst {
    WASMInst {
      _skey: skey,
      opcode: WASMOpcode::Store(bits),
      operands: vec![Box::new(value), Box::new(addr)],
      comment: String::new(),
    }
  }

  pub(super) fn load(skey: usize, bits: usize, addr: WASMInst) -> WASMInst {
    WASMInst {
      _skey: skey,
      opcode: WASMOpcode::Load(bits),
      operands: vec![Box::new(addr)],
      comment: String::new(),
    }
  }

  pub(super) fn block_begin(skey: usize, label: String) -> WASMInst {
    WASMInst {
      _skey: skey,
      opcode: WASMOpcode::BlockBegin(label),
      operands: Vec::new(),
      comment: String::new(),
    }
  }

  pub(super) fn block_end(skey: usize) -> WASMInst {
    WASMInst {
      _skey: skey,
      opcode: WASMOpcode::BlockEnd,
      operands: Vec::new(),
      comment: String::new(),
    }
  }

  pub(super) fn binop(skey: usize, op: &BinaryOp, lhs: WASMInst, rhs: WASMInst) -> WASMInst {
    let mut res = WASMInst {
      _skey: skey,
      opcode: WASMOpcode::Binary(op.clone()),
      operands: vec![Box::new(lhs), Box::new(rhs)],
      comment: String::new(),
    };

    if op == &BinaryOp::Add {
      for i in 0..2 {
        if let WASMOpcode::Const(_, _, value) = res.operands[i].opcode {
          if value == 0 {
            return *res.operands.remove(1 - i)
          }
        }
      }
    }

    if op == &BinaryOp::Mul {
      for i in 0..2 {
        if let WASMOpcode::Const(_, _, value) = res.operands[i].opcode {
          if value == 1 {
            return *res.operands.remove(1 - i)
          }
          if value == 0 {
            return Self::iconst(skey, 0);
          }
        }
      }
    }

    res
  }

  pub(super) fn call(skey: usize, name: String, args: Vec<WASMInst>) -> WASMInst {
    WASMInst {
      _skey: skey,
      opcode: WASMOpcode::Call(name),
      operands: args.into_iter().map(|x| Box::new(x)).collect::<Vec<Box<WASMInst>>>(),
      comment: String::new(),
    }
  }

  pub(super) fn plain(s: String) -> WASMInst {
    WASMInst {
      _skey: 0,
      opcode: WASMOpcode::Plain(s),
      operands: Vec::new(),
      comment: String::new(),
    }
  }

  pub(super) fn local_get(skey: usize, name: String) -> WASMInst {
    WASMInst {
      _skey: skey,
      opcode: WASMOpcode::LocalGet(name),
      operands: Vec::new(),
      comment: String::new(),
    }
  }

  pub(super) fn local_set(skey: usize, name: String, value: WASMInst) -> WASMInst {
    WASMInst {
      _skey: skey,
      opcode: WASMOpcode::LocalSet(name),
      operands: vec![Box::new(value)],
      comment: String::new(),
    }
  }

  pub(super) fn ret(skey: usize, val: Option<WASMInst>) -> WASMInst {
    WASMInst {
      _skey: skey,
      opcode: WASMOpcode::Return,
      operands: if val.is_some() { vec![Box::new(val.unwrap())] } else { vec![] },
      comment: String::new(),
    }
  }

  pub(super) fn cmp(skey: usize, pred: CmpPred, lhs: WASMInst, rhs: WASMInst) -> WASMInst {
    WASMInst {
      _skey: skey,
      opcode: WASMOpcode::Compare(32, pred),
      operands: vec![Box::new(lhs), Box::new(rhs)],
      comment: String::new()
    }
  }

  pub(super) fn br_if(skey: usize, label: String, cond: WASMInst) -> WASMInst {
    WASMInst {
      _skey: skey,
      opcode: WASMOpcode::BrIf(label),
      operands: vec![Box::new(cond)],
      comment: String::new(),
    }
  }

  pub(super) fn br(skey: usize, label: String) -> WASMInst {
    WASMInst {
      _skey: skey,
      opcode: WASMOpcode::Br(label),
      operands: Vec::new(),
      comment: String::new(),
    }
  }

  pub(super) fn iconst(skey: usize, i: u64) -> WASMInst {
    WASMInst {
      _skey: skey,
      opcode: WASMOpcode::Const(true, 32, i),
      operands: Vec::new(),
      comment: String::new(),
    }
  }

  fn to_string(&self, indent: &mut usize) -> String {
    let mut res = match &self.opcode {
      WASMOpcode::BlockBegin(label) => {
        *indent += 1;
        format!("{}(block ${}", " ".repeat(*indent - 1), label)
      },
      WASMOpcode::LoopBegin(label) => {
        *indent += 1;
        format!("{}(loop ${}", " ".repeat(*indent - 1), label)
      },
      WASMOpcode::BlockEnd => {
        *indent -= 1;
        format!("{})", " ".repeat(*indent))
      }
      WASMOpcode::Br(label) => {
        format!("{}(br ${})", " ".repeat(*indent), label)
      }
      WASMOpcode::BrIf(label) => {
        let indent0 = " ".repeat(*indent);
        *indent += 1;
        let cond = self.operands[0].to_string(indent);
        *indent -= 1;
        format!("{}(br_if ${}\n{}\n{})", indent0, label, cond, indent0)
      }
      WASMOpcode::Plain(s) => {
        format!("{}{}", " ".repeat(*indent), s)
      }
      WASMOpcode::Const(_, dbits, i) => {
        format!("{}(i{}.const {})", " ".repeat(*indent), dbits, *i as i32)
      }
      WASMOpcode::Compare(dtype, pred) => {
        let pred = pred.to_string();
        let pred = if pred.len() > 2 { pred[1..].to_string() + "_s" } else { pred.to_string() };
        *indent += 1;
        let lhs = self.operands[0].to_string(indent);
        let rhs = self.operands[1].to_string(indent);
        *indent -= 1;
        let indent = " ".repeat(*indent);
        format!("{}(i{}.{}\n{}\n{}\n{})", indent, dtype, pred, lhs, rhs, indent)
      }
      WASMOpcode::Return => {
        let value = if self.operands.len() > 0 {
          *indent += 1;
          let res = self.operands[0].to_string(indent);
          *indent -= 1;
          res
        } else {
          "".to_string()
        };
        let indent = " ".repeat(*indent);
        format!("{}(return\n{}\n{})", indent, value, indent)
      }
      WASMOpcode::Binary(op) => {
        *indent += 1;
        let lhs = self.operands[0].to_string(indent);
        let rhs = self.operands[1].to_string(indent);
        *indent -= 1;
        let op = op.to_string();
        let op = if op.len() > 3 {
          op[1..].to_string() + "_s"
        } else {
          op.to_string()
        };
        let indent = " ".repeat(*indent);
        format!("{}(i32.{}\n{}\n{}\n{})", indent, op, lhs, rhs, indent)
      }
      WASMOpcode::LocalGet(var) => {
        format!("{}(local.get ${})", " ".repeat(*indent), var)
      }
      WASMOpcode::LocalSet(var) => {
        *indent += 1;
        let value = self.operands[0].to_string(indent);
        *indent -= 1;
        let indent = " ".repeat(*indent);
        format!("{}(local.set ${}\n{}\n{})", indent, var, value, indent)
      }
      WASMOpcode::Call(callee) => {
        let operands = {
          *indent += 1;
          let operands = self.operands.iter().map(|op| op.to_string(indent)).collect::<Vec<String>>().join("\n");
          *indent -= 1;
          operands
        };
        let indent = " ".repeat(*indent);
        format!("{}(call ${}\n{}\n{})", indent, callee, operands, indent)
      }
      WASMOpcode::Load(_) => {
        let addr = {
          *indent += 1;
          let addr = self.operands[0].to_string(indent);
          *indent -= 1;
          addr
        };
        let indent = " ".repeat(*indent);
        format!("{}(i32.load\n{}\n{})", indent, addr, indent)
      },
      WASMOpcode::Store(bits) => {
        let value = {
          *indent += 1;
          let value = self.operands[0].to_string(indent);
          *indent -= 1;
          value
        };
        let addr = {
          *indent += 1;
          let addr = self.operands[1].to_string(indent);
          *indent -= 1;
          addr
        };
        let indent = " ".repeat(*indent);
        let bits = if *bits == 32 { "".to_string() } else { bits.to_string() };
        format!("{}(i32.store{}\n{}\n{}\n{})", indent, bits, addr, value, indent)
      }
    };
    if !self.comment.is_empty() {
      res.push_str(&format!(" ;; {}", self.comment).as_str());
    }
    res
  }

}


