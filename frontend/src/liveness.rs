use ir::instructions::{Block, BlockEnd, Function, Instruction, Program, Register,BlockID};
use std::collections::{HashMap, HashSet};

enum Use {
    Dead,
    Alive,
}

#[derive(Default)]
pub struct LivenessChecker {
    pub values: HashMap<BlockID, (HashSet<Register>,HashSet<Register>)>,
}

impl LivenessChecker {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self,id:BlockID,sets:(HashSet<Register>,HashSet<Register>)) {
        self.values.insert(id,sets);
    }
}

pub fn calculate_liveness(p: &Program) {
    let mut checker = LivenessChecker::new();
    for function in &p.functions {
        for (id, block) in &function.blocks {
            checker.insert(*id,init(block))
        }
    }


    println!("lable \tlive \tkill");

    for (block,(live,kill)) in checker.values {
        println!("{} \t {:?} \t {:?}",block,live,kill)
    }
}

pub fn init(b: &Block) ->( HashSet<Register>,HashSet<Register>) {
    let mut uevar = HashSet::new(); // variables used before they are defined
    let mut varkill = HashSet::new(); // All variables defined in the block
                                      //
    for inst in &b.instructions {
        use Instruction::*;
        match inst {
            Array(ref dest, _) => {
                varkill.insert(*dest);
            }

            Binary(ref dest, ref lhs, _, ref rhs) => {
                if !varkill.contains(lhs) {
                    uevar.insert(*lhs);
                } else if !varkill.contains(rhs) {
                    uevar.insert(*rhs);
                }
                varkill.insert(*dest);
            }
            Cast(ref dest, ref value, _) => {
                if !varkill.contains(value) {
                    uevar.insert(*value);
                }
                varkill.insert(*dest);
            }

            Call(ref dest, _, ref args) => {
                for arg in args {
                    if !varkill.contains(arg) {
                        uevar.insert(*arg);
                    }
                }

                varkill.insert(*dest);
            }

            StatementStart => (),

            StoreI(ref dest, _) => {
                varkill.insert(*dest);
            }

            Store(ref dest, ref val) => {
                if !varkill.contains(val) {
                    uevar.insert(*val);
                }

                varkill.insert(*dest);
            }

            Unary(ref dest, ref val, _) => {
                if !varkill.contains(val) {
                    uevar.insert(*val);
                }

                varkill.insert(*dest);
            }

            Return(ref val) => {
                if !varkill.contains(val) {
                    uevar.insert(*val);
                }
            }
        }
    }

    (uevar,varkill)
}
