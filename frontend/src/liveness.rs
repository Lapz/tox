use ir::instructions::{Block, BlockEnd, BlockID, Function, Instruction, Program, Register};
use std::collections::{HashMap, HashSet};
use std::{collections::hash_map::Entry, hash::Hash};
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
    pub live_in: HashMap<BlockID, HashSet<Register>>,
    pub live_out: HashMap<BlockID, HashSet<Register>>,
}

impl<'a> LivenessChecker<'a> {
    pub fn new(function: &'a Function) -> Self {
        Self {
            function,
            values: HashMap::new(),
            successors: HashMap::new(),
            predecessors: HashMap::new(),
            live_in: HashMap::new(),
            live_out: HashMap::new(),
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
            self.live_in.insert(*id, HashSet::new()); // init the in[n] to empty
            self.live_out.insert(*id, HashSet::new()); // init the out[n] to empty
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
                BlockEnd::End => {}
            }
        }
    }

    pub fn live_in(&mut self, id: BlockID, b: &Block) -> (HashSet<Register>, HashSet<Register>) {
        let mut used = HashSet::new(); // variables used before they are defined
        let mut defined = HashSet::new(); // All variables defined in the block

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

    fn calulate_live_out(&mut self) {
        'outer: loop {
            for (id, _) in self.function.blocks.iter().rev() {
                //save the current results
                let current_in = self.live_in[&id].clone();
                let current_out = self.live_out[&id].clone();

                let (used, defined) = self.values[id].clone();

                let diff: HashSet<_> = current_out.difference(&defined).cloned().collect();

                *self.live_in.get_mut(id).unwrap() =
                    used.union(&diff).cloned().collect::<HashSet<_>>();

                if let Some(successors) = self.successors.get(&id) {
                    let mut new_out = HashSet::new();

                    for suc in successors {
                        new_out.extend(self.live_in[suc].clone())
                    }

                    *self.live_out.get_mut(id).unwrap() = new_out;
                }

                // let successors = self.successors.get(&id).unwrap_or(&HashSet::new()); // some blocks have now suc

                // let mut new_out = HashSet::new();

                if current_in == self.live_in[&id] && current_out == self.live_out[&id] {
                    break 'outer;
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

        for (block, (live, kill)) in &checker.values {
            println!("{} \t {:?} \t {:?}", block, live, kill)
        }

        println!("========================");

        checker.calulate_live_out();

        println!("lable \tlive_in");

        for (block, regs) in &checker.live_in {
            println!("{} \t {:?} \t", block, regs)
        }

        println!("========================");

        println!("lable \tlive_out");

        for (block, regs) in &checker.live_out {
            println!("{} \t {:?} \t", block, regs)
        }
    }
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
