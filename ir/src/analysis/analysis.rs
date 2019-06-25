use crate::analysis::{AnalysisState, Interval};
use crate::instructions::{BlockEnd, BlockID, Function, Register};
use indexmap::map::Entry;
use indexmap::{indexset, IndexMap, IndexSet};
#[cfg(feature = "graphviz")]
use petgraph::dot::{Config, Dot};

#[cfg(any(feature = "graphviz", feature = "prettytable"))]
use std::{
    fs::{self, File},
    io::{self, Write},
    process::Command,
};

impl AnalysisState {
    pub fn add_successors(&mut self, id: BlockID, block: BlockID) {
        let entry = self.successors.entry(id);
        match entry {
            Entry::Occupied(mut entry) => {
                entry.get_mut().insert(block);
            }
            Entry::Vacant(entry) => {
                let mut set = IndexSet::new();
                set.insert(block);
                entry.insert(set);
            }
        }
    }

    pub fn add_predecessor(&mut self, id: BlockID, block: BlockID) {
        let entry = self.predecessors.entry(id);

        if id == block {
            return;
        }

        match entry {
            Entry::Occupied(mut entry) => {
                entry.get_mut().insert(block);
                entry.get_mut().sort();
            }
            Entry::Vacant(entry) => {
                let mut set = IndexSet::new();
                set.insert(block);
                entry.insert(set);
            }
        }
    }

    pub fn calculate_successors(&mut self, function: &Function) {
        for (id, block) in function.blocks.iter().rev() {
            self.live_in.insert(*id, IndexSet::new()); // init the in[n] to empty
            self.live_out.insert(*id, IndexSet::new()); // init the out[n] to empty

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
            let mut gen = IndexSet::new(); // variables used before they are defined
            let mut kill = IndexSet::new(); // All variables defined in the block

            for inst in block.instructions.iter().rev() {
                gen.extend(inst.used());
                kill.extend(inst.def());
            }

            // match block.end {
            //     BlockEnd::Branch(cond, _, _) => {
            //         gen.insert(cond);
            //     }
            //     BlockEnd::Return(value) => {
            //         gen.insert(value);
            //     }
            //     BlockEnd::Jump(_) => (),
            //     BlockEnd::Link(_) => (),
            //     BlockEnd::End => {}
            // }

            self.gen.insert(*id, gen);
            self.kill.insert(*id, kill);
            self.live_in.insert(*id, IndexSet::new());
            self.live_out.insert(*id, IndexSet::new());
        }
    }

    pub fn calulate_live_out(&mut self, function: &Function) {
        let mut changed = true;

        #[cfg(feature = "prettytable")]
        let mut iteration = 0;

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

        println!("{:?}", self.gen);

        while changed {
            changed = false;

            for (id, _) in function.blocks.iter().rev() {
                let old_out = self.live_out[id].clone();
                let old_in = self.live_in[id].clone();

                let (gen, kill) = (self.gen[id].clone(), self.kill[id].clone());

                #[cfg(feature = "prettytable")]
                {
                    data.push(vec![
                        id.to_string(),
                        format!("{:?}", gen),
                        format!("{:?}", kill),
                        format!("{:?}", self.successors.get(id).unwrap_or(&IndexSet::new())),
                        format!("{:?}", &self.live_in[id]),
                        format!("{:?}", &self.live_out[id]),
                    ]);
                }

                *self.live_in.get_mut(id).unwrap() = gen
                    .union(
                        &self.live_out[id]
                            .difference(&kill)
                            .cloned()
                            .collect::<IndexSet<_>>(),
                    )
                    .cloned()
                    .collect::<IndexSet<_>>();

                if let Some(successors) = self.successors.get(id) {
                    let mut new_out = IndexSet::new();

                    for suc in successors {
                        new_out.extend(self.live_in[suc].clone())
                    }

                    *self.live_out.get_mut(id).unwrap() = new_out;
                };

                if !(old_in == self.live_in[id] && old_out == self.live_out[id]) {
                    changed = true;
                }
            }
        }

        #[cfg(feature = "live_out")]
        {
            text_tables::render(&mut std::io::stdout(), &data).unwrap();
        }
    }

    pub fn calculate_live_now(&mut self, function: &Function) {
        for (id, block) in &function.blocks {
            let live_out = self.live_out[id].clone();
            self.live_now.insert(*id, live_out);

            for inst in block.instructions.iter() {
                let def = inst.def();
                let used = inst.used();

                for reg in def {
                    self.live_now.get_mut(id).unwrap().remove(&reg);
                }

                self.live_now.get_mut(id).unwrap().extend(used);
            }
        }
    }

    pub fn calculate_live_intervals(&mut self, function: &Function) {
        for (id, block) in &function.blocks {
            self.intervals.insert(*id, IndexMap::new());
            for (i, instruction) in block.instructions.iter().enumerate() {
                for reg in &self.live_out[id] {
                    if let Some(ref mut interval) = self.intervals[id].get_mut(reg) {
                        if instruction.used().contains(reg) || instruction.def().contains(reg) {
                            interval.end = i;
                        }
                    } else {
                        self.intervals[id].insert(*reg, Interval { start: i, end: i });
                    };
                }
            }
        }

        #[cfg(feature = "live_ranges")]
        {
            writeln!(&mut std::io::stdout(), "block|\treg|\trange");

            for (block, intervals) in &self.intervals {
                writeln!(&mut std::io::stdout(), "{}", block);

                for (reg, interval) in intervals {
                    writeln!(&mut std::io::stdout(), "\t{}:\t{}", reg, interval);
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::AnalysisState;
    use crate::instructions::{
        BinaryOp, Block, BlockEnd, BlockID, Function, Instruction, Register, Value,
    };
    use indexmap::{indexmap, indexset, IndexMap};
    use pretty_assertions::assert_eq;
    use std::rc::Rc;
    use util::symbol::{SymbolFactory, Symbols};

    #[test]

    fn check_predecessors() {
        let mut function = test_function();

        let mut symbols = Symbols::new(Rc::new(SymbolFactory::new()));

        let mut analysis = AnalysisState::new(&mut function, &mut symbols);

        let expected_pred = indexmap!(
            BlockID(1) => indexset!(BlockID(0),BlockID(3)),
            BlockID(2) => indexset!(BlockID(1)),
            BlockID(3) => indexset!(BlockID(2),BlockID(7)),
            BlockID(4) => indexset!(BlockID(3)),
            BlockID(5) => indexset!(BlockID(1)),
            BlockID(6) => indexset!(BlockID(5)),
            BlockID(7) => indexset!(BlockID(6),BlockID(8)),
            BlockID(8) => indexset!(BlockID(5)),
        );

        analysis.predecessors.sort_keys();

        assert_eq!(analysis.predecessors, expected_pred)
    }

    fn test_function() -> Function {
        let mut example_cfg = IndexMap::new();

        let b0 = BlockID(0);
        let b1 = BlockID(1);
        let b2 = BlockID(2);
        let b3 = BlockID(3);
        let b4 = BlockID(4);
        let b5 = BlockID(5);
        let b6 = BlockID(6);
        let b7 = BlockID(7);
        let b8 = BlockID(8);

        example_cfg.insert(b0, Block::new(vec![], BlockEnd::Jump(b1)));
        example_cfg.insert(
            b1,
            Block::new(vec![], BlockEnd::Branch(Register::new(), b2, b5)),
        );
        example_cfg.insert(b2, Block::new(vec![], BlockEnd::Jump(b3)));
        example_cfg.insert(
            b5,
            Block::new(vec![], BlockEnd::Branch(Register::new(), b6, b8)),
        );
        example_cfg.insert(b6, Block::new(vec![], BlockEnd::Jump(b7)));
        example_cfg.insert(b8, Block::new(vec![], BlockEnd::Jump(b7)));
        example_cfg.insert(b7, Block::new(vec![], BlockEnd::Jump(b3)));
        example_cfg.insert(
            b3,
            Block::new(vec![], BlockEnd::Branch(Register::new(), b4, b1)),
        );
        example_cfg.insert(b4, Block::new(vec![], BlockEnd::End));

        let mut function = Function::dummy();

        function.blocks = example_cfg;

        function
    }

    #[test]
    fn liveness_works() {
        //         a←0
        // L1 :b←a+1
        // c←c+b a←b∗2
        // if a < N goto L1 return c
        //
        let mut example_cfg = IndexMap::new();

        let b1 = BlockID(1);
        let b2 = BlockID(2);
        let b3 = BlockID(3);
        let b4 = BlockID(4);
        let b5 = BlockID(5);
        let b6 = BlockID(6);

        let a = Register::new();
        let b = Register::new();
        let c = Register::new();

        let gen = indexmap!(
            b1 => indexset!(),
            b2 => indexset!(a),
            b3 => indexset!(b,c),
            b4 => indexset!(b),
            b5 => indexset!(a),
            b6 => indexset!(c),
        );

        let kill = indexmap!(
            b1 => indexset!(a),
            b2 => indexset!(b),
            b3 => indexset!(c),
            b4 => indexset!(a),
            b5 => indexset!(),
            b6 => indexset!(),
        );

        example_cfg.insert(b1, Block::new(vec![], BlockEnd::Jump(b2)));
        example_cfg.insert(b2, Block::new(vec![], BlockEnd::Jump(b3)));
        example_cfg.insert(b3, Block::new(vec![], BlockEnd::Jump(b4)));
        example_cfg.insert(b4, Block::new(vec![], BlockEnd::Jump(b5)));
        example_cfg.insert(
            b5,
            Block::new(vec![], BlockEnd::Branch(Register::new(), b2, b6)),
        );
        example_cfg.insert(b6, Block::new(vec![], BlockEnd::End));

        let mut function = Function::dummy();

        function.blocks = example_cfg;

        let mut symbols = Symbols::new(Rc::new(SymbolFactory::new()));

        let mut analysis = AnalysisState::new(&mut function, &mut symbols);

        analysis.gen = gen;
        analysis.kill = kill;

        analysis.calulate_live_out(&function);

        analysis.live_out.sort_keys();
        analysis.live_in.sort_keys();

        for (_, live_out) in analysis.live_out.iter_mut() {
            live_out.sort();
        }

        println!("{:?}", analysis.live_in);

        assert_eq!(
            analysis.live_in,
            indexmap!(
                b1 => indexset!(c),
                b2 => indexset!(a,c),
                b3 => indexset!(b,c),
                b4 => indexset!(b,c),
                b5 => indexset!(a,c),
                b6 => indexset!(c),
            )
        );

        assert_eq!(
            analysis.live_out,
            indexmap!(
                b1 => indexset!(a,c),
                b2 => indexset!(b,c),
                b3 => indexset!(b,c),
                b4 => indexset!(a,c),
                b5 => indexset!(a,c),
                b6 => indexset!(),
            )
        );
    }
}
