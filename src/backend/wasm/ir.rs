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
}

pub(super) struct WASMFunc {
  name: String,
  args: Vec<String>,
  pub(super) insts: Vec<WASMInst>,
  rty: String
}

impl WASMFunc {
  pub(super) fn new(name: String, args: Vec<String>, rty: String) -> Self {
    Self {
      name,
      args,
      insts: Vec::new(),
      rty
    }
  }

  pub(super) fn to_string(&self) -> String {
    let mut indent = 2;
    let mut res = format!(" (func ${}\n", self.name);
    res.push_str(self.args.iter().map(|x| format!("  (param i32 ${})", x)).collect::<Vec<String>>().join("\n").as_str());
    res.push('\n');
    if !self.rty.is_empty() {
      res.push_str("  (result i32)\n");
    }
    for elem in self.insts.iter() {
      res.push_str((elem.to_string(&mut indent) + "\n").as_str());
    }
    res.push_str(" )\n");
    return res;
  }
}

pub(super) struct WASMInst {
  skey: usize,
  opcode: WASMOpcode,
  operands: Vec<Box<WASMInst>>,
  pub(super) comment: String,
}

impl WASMInst {

  pub(super) fn loop_begin(skey: usize, label: String) -> WASMInst {
    WASMInst {
      skey,
      opcode: WASMOpcode::LoopBegin(label),
      operands: Vec::new(),
      comment: String::new(),
    }
  }

  pub(super) fn block_begin(skey: usize, label: String) -> WASMInst {
    WASMInst {
      skey,
      opcode: WASMOpcode::BlockBegin(label),
      operands: Vec::new(),
      comment: String::new(),
    }
  }

  pub(super) fn block_end(skey: usize) -> WASMInst {
    WASMInst {
      skey,
      opcode: WASMOpcode::BlockEnd,
      operands: Vec::new(),
      comment: String::new(),
    }
  }

  pub(super) fn plain(s: String) -> WASMInst {
    WASMInst {
      skey: 0,
      opcode: WASMOpcode::Plain(s),
      operands: Vec::new(),
      comment: String::new(),
    }
  }

  pub(super) fn br_if(skey: usize, label: String, cond: WASMInst) -> WASMInst {
    WASMInst {
      skey,
      opcode: WASMOpcode::BrIf(label),
      operands: vec![Box::new(cond)],
      comment: String::new(),
    }
  }

  pub(super) fn br(skey: usize, label: String) -> WASMInst {
    WASMInst {
      skey,
      opcode: WASMOpcode::Br(label),
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
        format!("{}(br_if ${} (i32.const 0))", " ".repeat(*indent), label)
      }
      WASMOpcode::Plain(s) => {
        format!("{}", s)
      }
    };
    if !self.comment.is_empty() {
      res.push_str(&format!(" ;; {}", self.comment).as_str());
    }
    res
  }

}


