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
  /// Return instruction.
  Return,
}

pub(super) struct WASMFunc {
  name: String,
  args: Vec<String>,
  pub(super) insts: Vec<WASMInst>,
  pub(super) locals: HashMap<usize, String>,
  rty: String
}

impl WASMFunc {

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
    let mut res = format!(" (func ${}\n", self.name);
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
    WASMInst {
      _skey: skey,
      opcode: WASMOpcode::Binary(op.clone()),
      operands: vec![Box::new(lhs), Box::new(rhs)],
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
      operands: if val.is_some() { vec![Box::new(val.unwrap())] } else { Vec::new() },
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
        format!("{}(i{}.const {})", " ".repeat(*indent), dbits, i)
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
        format!("{}(i32.{}\n{}\n{}\n)", " ".repeat(*indent), op.to_string(), lhs, rhs)
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
    };
    if !self.comment.is_empty() {
      res.push_str(&format!(" ;; {}", self.comment).as_str());
    }
    res
  }

}


