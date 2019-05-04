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
    pub predecessors: HashMap<BlockID, HashSet<BlockID>>,
}

impl<'a> LivenessChecker<'a> {
    pub fn new(function: &'a Function) -> Self {
        Self {
            function,
            values: HashMap::new(),
            successors: HashMap::new(),
            predecessors: HashMap::new(),
        }
    }

    pub fn insert(&mut self, id: BlockID, sets: (HashSet<Register>, HashSet<Register>)) {
        self.values.insert(id, sets);
    }

    pub fn add_successors(&mut self, id: BlockID, block: BlockID) {
        let entry = self.successors.entry(id);
        match entry {
            Entry::Occupied(mut entry) => {
                entry.get_mut().insert(block);
            }
            Entry::Vacant(entry) => {
                let mut set = HashSet::new();
                set.insert(block);
                entry.insert(set);
            }
        }
    }

    pub fn add_predecessor(&mut self, id: BlockID, block: BlockID) {
        let entry = self.predecessors.entry(id);
        match entry {
            Entry::Occupied(mut entry) => {
                entry.get_mut().insert(block);
            }
            Entry::Vacant(entry) => {
                let mut set = HashSet::new();
                set.insert(block);
                entry.insert(set);
            }
        }
    }

    pub fn calculate_successors(&mut self) {
        for (i, (id, block)) in self.function.blocks.iter().enumerate().peekable() {
            if i > 0 && !(i + 1 > self.function.blocks.len()) {
                self.add_predecessor(*id, self.function.blocks[i - 1].0)
            }
            match block.end {
                BlockEnd::Branch(_, lhs, rhs) => {
                    self.add_successors(*id, lhs);
                    self.add_successors(*id, rhs);
                    self.add_predecessor(lhs, *id);
                    self.add_predecessor(rhs, *id);
                }

                BlockEnd::Jump(dest) => {
                    self.add_successors(*id, dest);
                    self.add_predecessor(dest, *id);
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

        for (block, successors) in &checker.successors {
            print!("{} ", block);

            print!("{{");

            for (i, suc) in successors.iter().enumerate() {
                if i + 1 == successors.len() {
                    print!("{}", suc)
                } else {
                    print!("{},", suc)
                }
            }

            print!("}}\n");
        }
        println!("========================");
        for (block, predecessors) in &checker.predecessors {
            print!("{} ", block);

            print!("{{");

            for (i, suc) in predecessors.iter().enumerate() {
                if i + 1 == predecessors.len() {
                    print!("{}", suc)
                } else {
                    print!("{},", suc)
                }
            }

            print!("}}\n");
        }
        println!("========================");

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
    // used.union()

    unimplemented!()
}

pub fn init(b: &Block) -> (HashSet<Register>, HashSet<Register>) {
    let mut used = HashSet::new(); // variables used before they are defined
    let mut defined = HashSet::new(); // All variables defined in the block
                                      //
    for inst in b.instructions.iter().rev() {
        use Instruction::*;
        match inst {
            Array(ref dest, _) => {
                defined.insert(*dest);
            }

            Binary(ref dest, ref lhs, _, ref rhs) => {
                if !defined.contains(lhs) {
                    used.insert(*lhs);
                } else if !defined.contains(rhs) {
                    used.insert(*rhs);
                }
                defined.insert(*dest);
            }
            Cast(ref dest, ref value, _) => {
                if !defined.contains(value) {
                    used.insert(*value);
                }
                defined.insert(*dest);
            }

            Call(ref dest, _, ref args) => {
                for arg in args {
                    if !defined.contains(arg) {
                        used.insert(*arg);
                    }
                }

                defined.insert(*dest);
            }

            StatementStart => (),

            StoreI(ref dest, _) => {
                defined.insert(*dest);
            }

            Store(ref dest, ref val) => {
                if !defined.contains(val) {
                    used.insert(*val);
                }

                defined.insert(*dest);
            }

            Unary(ref dest, ref val, _) => {
                if !defined.contains(val) {
                    used.insert(*val);
                }

                defined.insert(*dest);
            }

            Return(ref val) => {
                if !defined.contains(val) {
                    used.insert(*val);
                }
            }
        }
    }

    (used, defined)
}
