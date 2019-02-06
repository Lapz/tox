pub use crate::instructions::*;
use std::collections::{HashMap, HashSet};

pub struct Liveness {
    live_range: HashMap<Register, Vec<(usize, usize)>>,
}

impl Liveness {
    pub fn new() -> Self {
        Self {
            live_range: HashMap::new(),
        }
    }

    fn find_range(&mut self, val: &Value, i: usize) {
        match val {
            Value::Register(ref reg) => {
                let ranges = self.live_range.entry(*reg).or_insert_with(Vec::new);
                ranges.push((i - 1, i))
            }

            _ => (),
        }
    }

    pub fn liveness(&mut self, program: &Program) {
        for function in program.functions.iter() {
            for (id, block) in function.blocks.iter() {
                let in_edges: HashMap<BlockID, HashSet<BlockID>> = HashMap::new();
                let out_edges: HashMap<BlockID, HashSet<BlockID>> = HashMap::new();

                for (i, instruction) in block.instructions.iter().rev().enumerate() {
                    match &instruction.instruction {
                        Inst::Array(ref l, ref s) => {
                            self.find_range(l, i);
                        }
                        Inst::StatementStart => (),
                        Inst::Binary(ref res, ref lhs, ref op, ref rhs) => {
                            self.find_range(res, i);
                            self.find_range(lhs, i);
                            self.find_range(rhs, i);
                        }
                        Inst::Print(ref v) => write!(out, "print {}", v),

                        Inst::Drop(ref reg) => (|)
                        ,
                        Inst::Store(ref dest, ref source) => write!(out, "{} <- {}", dest, source),
                        Inst::Cast(ref dest, _, ref ty) => write!(out, "{} as {}", dest, ty),
                        Inst::Unary(ref dest, ref source, ref op) => {
                            write!(out, "{} <- {}{}", dest, op, source)
                        }
                        Inst::Return(ref label) => write!(out, "return @{}", label),
                        Inst::Call(ref dest, ref callee, ref args) => {
                            write!(out, "{} <- call {} ", dest, callee)?;

                            for arg in args {
                                write!(out, "{}", arg)?;
                            }

                            write!(out, "")?;

                            Ok(())
                        }
                    }
                }
            }
        }
    }
}

pub fn liveness(program: &Program) {
    for function in program.functions.iter() {
        for (id, block) in function.blocks.iter().reverse() {
            let in_edges: HashMap<BlockID, HashSet<BlockID>> = HashMap::new();
            let out_edges: HashMap<BlockID, HashSet<BlockID>> = HashMap::new();
        }
    }
}
