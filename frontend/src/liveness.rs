use ir::instructions::{Block, BlockEnd, BlockID, Function, Instruction, Program, Register};
#[cfg(feature = "graphviz")]
use petgraph::dot::{Config, Dot};
use petgraph::Graph;
use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
#[cfg(feature = "graphviz")]
use std::{
    fs::{self, File},
    io::{self, Write},
    process::Command,
};

#[derive(Clone)]
pub struct LivenessChecker<'a> {
    /// A mapping of the used and defined sets for a basic block
    pub sets: HashMap<BlockID, (HashSet<Register>, HashSet<Register>)>,
    pub function: &'a Function,
    pub successors: HashMap<BlockID, HashSet<BlockID>>,
    pub predecessors: HashMap<BlockID, HashSet<BlockID>>,
    pub live_in: HashMap<BlockID, HashSet<Register>>,
    pub live_out: HashMap<BlockID, HashSet<Register>>,
    pub live_ranges: HashMap<Register, HashSet<Register>>,
}

#[derive(Clone)]
pub struct InterferenceGraph {
    pub graph: HashMap<Register, HashSet<Register>>,
}

impl<'a> LivenessChecker<'a> {
    pub fn new(function: &'a Function) -> Self {
        let mut checker = Self {
            function,
            sets: HashMap::new(),
            successors: HashMap::new(),
            predecessors: HashMap::new(),
            live_in: HashMap::new(),
            live_out: HashMap::new(),
            live_ranges: HashMap::new(),
        };

        checker.init();
        checker.calculate_successors();

        checker
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

    /// Initialize the set of used and defined regsiters for a basic block
    pub fn init(&mut self) {
        for (id, block) in &self.function.blocks {
            let mut used = HashSet::new(); // variables used before they are defined
            let mut defined = HashSet::new(); // All variables defined in the block

            for inst in block.instructions.iter().rev() {
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

            self.sets.entry(*id).or_insert((used, defined));
        }
    }

    fn calulate_live_out(&mut self) {
        #[cfg(feature = "prettytable")]
        let mut data: Vec<Vec<String>> = Vec::new();
        #[cfg(feature = "prettytable")]
        {
            data.push(vec![
                "label".into(),
                "use".into(),
                "def".into(),
                "sucessors".into(),
                "out".into(),
                "in".into(),
            ]);
        }
        let mut changed = true;
        #[cfg(feature = "prettytable")]
        let mut iteration = 0;
        while changed {
            changed = false;

            for (id, _) in self.function.blocks.iter().rev() {
                let old_in = self.live_in[&id].clone();
                let old_out = self.live_out[&id].clone();

                let (used, defined) = self.sets[id].clone();

                #[cfg(feature = "prettytable")]
                {
                    data.push(vec![
                        id.to_string(),
                        format!("{:?}", used),
                        format!("{:?}", defined),
                        format!("{:?}", self.successors.get(id).unwrap_or(&HashSet::new())),
                        format!("{:?}", &self.live_in[id]),
                        format!("{:?}", &self.live_out[id]),
                    ]);
                }

                *self.live_in.get_mut(id).unwrap() = used
                    .union(
                        &old_out
                            .difference(&defined)
                            .cloned()
                            .collect::<HashSet<_>>(),
                    )
                    .cloned()
                    .collect::<HashSet<_>>();

                if let Some(successors) = self.successors.get(&id) {
                    let mut new_out = HashSet::new();

                    for suc in successors {
                        new_out.extend(self.live_in[suc].clone())
                    }

                    *self.live_out.get_mut(id).unwrap() = new_out;
                }

                if !(old_in == self.live_in[&id] && old_out == self.live_out[&id]) {
                    changed = true;
                }
            }
        }

        #[cfg(feature = "prettytable")]
        {
            text_tables::render(&mut std::io::stdout(), &data).unwrap();
        }
    }

    fn build_interference_graphs(&mut self) {
        let mut graph = Graph::new_undirected();
        let mut mappings = HashMap::new();
        for (id, block) in &self.function.blocks {
            let mut live = self.live_out[id].clone();

            for inst in block.instructions.iter().rev() {
                let used = inst.used();
                if inst.is_move() {
                    live = live
                        .difference(&used)
                        .cloned()
                        .collect::<HashSet<Register>>();
                }

                let def = inst.def();

                live = live.union(&def).cloned().collect::<HashSet<Register>>();

                for d in &def {
                    let start_node = if let Some(entry) = mappings.get(d) {
                        *entry
                    } else {
                        let node = graph.add_node(*d);

                        mappings.insert(*d, node);
                        node
                    };
                    for l in &live {
                        let end_node = if let Some(entry) = mappings.get(l) {
                            *entry
                        } else {
                            let node = graph.add_node(*l);
                            mappings.insert(*l, node);
                            node
                        };

                        graph.add_edge(end_node, start_node, 0);
                    }
                }

                live = used
                    .union(&live.difference(&def).cloned().collect::<HashSet<_>>())
                    .cloned()
                    .collect::<HashSet<_>>()
            }
        }

           #[cfg(feature = "graphviz")]
            {
                let file_name = format!("graphviz/main.dot");

                File::create(&file_name)
                    .unwrap()
                    .write(
                        Dot::with_config(&graph, &[Config::EdgeNoLabel])
                            .to_string()
                            .as_bytes(),
                    )
                    .unwrap();

                let mut dot = Command::new("dot");

                let output = dot
                    .args(&["-Tpng", &file_name])
                    .output()
                    .expect("failed to execute process")
                    .stdout;

                let mut file = File::create(format!("graphviz/main_reg.png", id)).unwrap();
                file.write(&output).unwrap();
            }
    }
}

pub fn calculate_liveness(p: &Program) {
    for function in &p.functions {
        let mut checker = LivenessChecker::new(function);
        checker.calulate_live_out();
        checker.build_interference_graphs();
    }
}
