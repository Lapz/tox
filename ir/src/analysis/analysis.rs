use crate::analysis::{AnalysisState, Interval};
use crate::instructions::{BlockEnd, BlockID, Function, Register};
#[cfg(feature = "graphviz")]
use petgraph::dot::{Config, Dot};
use petgraph::Graph;
use std::collections::{hash_map::Entry, HashMap, HashSet};
#[cfg(feature = "graphviz")]
use std::{
    fs::{self, File},
    io::{self, Write},
    process::Command,
};

impl AnalysisState {
    pub fn new() -> Self {
        Self {
            used_defined: HashMap::new(),
            successors: HashMap::new(),
            predecessors: HashMap::new(),
            live_in: HashMap::new(),
            live_out: HashMap::new(),
            intervals: HashMap::new(),
        }
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

    pub fn calculate_successors(&mut self, function: &Function) {
        for (i, (id, block)) in function.blocks.iter().enumerate().peekable() {
            self.live_in.insert(*id, HashSet::new()); // init the in[n] to empty
            self.live_out.insert(*id, HashSet::new()); // init the out[n] to empty
            if i > 0 && !(i + 1 > function.blocks.len()) {
                self.add_predecessor(*id, function.blocks[i - 1].0)
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
    pub fn init(&mut self, function: &Function) {
        for (id, block) in &function.blocks {
            let mut used = HashSet::new(); // variables used before they are defined
            let mut defined = HashSet::new(); // All variables defined in the block

            for inst in block.instructions.iter().rev() {
                use crate::instructions::Instruction::*;
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

            self.used_defined.entry(*id).or_insert((used, defined));
        }
    }
    pub fn calulate_live_out(&mut self, function: &Function) {
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

            for (id, _) in function.blocks.iter().rev() {
                let old_in = self.live_in[&id].clone();
                let old_out = self.live_out[&id].clone();

                let (used, defined) = self.used_defined[id].clone();

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

    pub fn calculate_live_intervals(&mut self, function: &Function) {
        #[cfg(feature = "prettytable")]
        let mut data: Vec<Vec<String>> = Vec::new();
        #[cfg(feature = "prettytable")]
        {
            data.push(vec!["reg".into(), "range".into()]);
        }

        for (id, block) in &function.blocks {
            let live_out = &self.live_out[id];

            for i in 0..block.instructions.len() {
                for reg in live_out {
                    let entry = self.intervals.entry(*reg);

                    match entry {
                        Entry::Occupied(mut entry) => {
                            entry.get_mut().end = i;
                        }
                        Entry::Vacant(entry) => {
                            entry.insert(Interval { start: i, end: i });
                        }
                    }
                }
            }
        }

        #[cfg(feature = "prettytable")]
        {
            for (reg, interval) in &self.intervals {
                {
                    data.push(vec![reg.to_string(), interval.to_string()]);
                }
            }
        }

        #[cfg(feature = "prettytable")]
        {
            text_tables::render(&mut std::io::stdout(), &data).unwrap();
        }
    }
}
