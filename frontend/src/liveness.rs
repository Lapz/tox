use ir::instructions::{Block, BlockEnd, BlockID, Function, Instruction, Program, Register};
use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
enum Use {
    Dead,
    Alive,
}

#[derive(Clone)]
pub struct LivenessChecker<'a> {
    pub values: HashMap<BlockID, (HashSet<Register>, HashSet<Register>)>,
    pub function: &'a Function,
    pub successors: HashMap<BlockID, HashSet<BlockID>>,
    pub predecessors:HashMap<BlockID, HashSet<BlockID>>,
}

impl<'a> LivenessChecker<'a> {
    pub fn new(function: &'a Function) -> Self {
        Self {
            function,
            values: HashMap::new(),
            successors: HashMap::new(),
            predecessors:HashMap::new()
        }
    }

    pub fn insert(&mut self, id: BlockID, sets: (HashSet<Register>, HashSet<Register>)) {
        self.values.insert(id, sets);
    }

    pub fn calculate_successors(&mut self) {
        for (i, (id, block)) in self.function.blocks.iter().enumerate() {
            match block.end {
                BlockEnd::Branch(_, ref rhs, ref lhs) => {
                    let entry = self.successors.entry(*id);
                    match entry {
                        Entry::Occupied(mut entry) => {
                            entry.get_mut().insert(*rhs);
                            entry.get_mut().insert(*lhs);
                        }
                        Entry::Vacant(entry) => {
                            let mut set = HashSet::new();
                            set.insert(*lhs);
                            set.insert(*rhs);
                            entry.insert(set);
                        }
                    }
                }

                BlockEnd::Jump(ref dest) => {
                    let entry = self.successors.entry(*id);
                    match entry {
                        Entry::Occupied(mut entry) => {
                            entry.get_mut().insert(*dest);
                        }
                        Entry::Vacant(entry) => {
                            let mut set = HashSet::new();
                            set.insert(*dest);

                            entry.insert(set);
                        }
                    }
                }
                BlockEnd::Return(_) => (),
                BlockEnd::Link(_) => (),
                BlockEnd::End => {
                    // if !(i + 1 > self.function.blocks.len()) {
                    //     let entry = self.successors.entry(*id);
                    //     match entry {
                    //         Entry::Occupied(entry) => {
                    //             entry.get_mut().insert(self.function.blocks[i]);
                    //         }
                    //         Entry::Vacant(entry) => {
                    //             let mut set = HashSet::new();
                    //             set.insert(*dest);

                    //             entry.insert(set);
                    //         }
                    //     }
                    // }
                }
            }
        }
    }
}

pub fn calculate_liveness(p: &Program) {
    for function in &p.functions {
        let mut checker = LivenessChecker::new(function);
        checker.calculate_successors();

        for (block,successors) in &checker.successors {
            print!("{} ,",block);

            print!("{{");

            for (i,suc) in successors.iter().enumerate() {
                if i+1 > successors.len() {
                    print!("{}",suc)
                }else {
                    print!("{},",suc)
                }
            }

            print!("}}\n");
        }
        for (id, block) in &function.blocks {
            checker.insert(*id, init(block))
        }

        println!("lable \tlive \tkill");

        for (block, (live, kill)) in checker.values {
            println!("{} \t {:?} \t {:?}", block, live, kill)
        }

        println!("========================");
    }
}

fn live_out(block: BlockID) -> HashSet<Register> {
    // uevar.union()

    unimplemented!()
}

pub fn init(b: &Block) -> (HashSet<Register>, HashSet<Register>) {
    let mut uevar = HashSet::new(); // variables used before they are defined
    let mut varkill = HashSet::new(); // All variables defined in the block
                                      //
    for inst in b.instructions.iter().rev() {
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

    (uevar, varkill)
}
