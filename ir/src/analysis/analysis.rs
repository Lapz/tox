use crate::analysis::{AnalysisState, Interval};
use crate::instructions::{BlockEnd, BlockID, Function};
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
            let mut used = IndexSet::new(); // variables used before they are defined
            let mut defined = IndexSet::new(); // All variables defined in the block

            for inst in block.instructions.iter().rev() {
                use crate::instructions::Instruction::*;
                used.extend(inst.used());
                defined.extend(inst.def());
            }

            self.used_defined.entry(*id).or_insert((used, defined));
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

        while changed {
            changed = false;

            for (id, _) in function.blocks.iter().rev() {
                let old_in = self.live_in[id].clone();
                let old_out = self.live_out[id].clone();

                let (used, defined) = self.used_defined[id].clone();

                #[cfg(feature = "prettytable")]
                {
                    data.push(vec![
                        id.to_string(),
                        format!("{:?}", used),
                        format!("{:?}", defined),
                        format!("{:?}", self.successors.get(id).unwrap_or(&IndexSet::new())),
                        format!("{:?}", &self.live_in[id]),
                        format!("{:?}", &self.live_out[id]),
                    ]);
                }

                *self.live_in.get_mut(id).unwrap() = used
                    .union(
                        &old_out
                            .difference(&defined)
                            .cloned()
                            .collect::<IndexSet<_>>(),
                    )
                    .cloned()
                    .collect::<IndexSet<_>>();

                if let Some(successors) = self.successors.get(id) {
                    let mut new_out = IndexSet::new();

                    for suc in successors {
                        new_out.extend(self.live_in.get(suc).clone().unwrap_or(&IndexSet::new()))
                    }

                    *self.live_out.get_mut(id).unwrap() = new_out;
                }

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

    pub fn immediate_dominator(&self, id: &BlockID) -> Option<BlockID> {
        let mut set = self.dominator[id].clone();

        set.remove(id);

        set.sort();
        set.pop()
    }

    pub fn find_dominance(&mut self, function: &Function) {
        // panic!("{:#?}", self.predecessors);
        #[cfg(feature = "prettytable")]
        let mut iteration = 0;

        #[cfg(feature = "prettytable")]
        let mut data: Vec<Vec<String>> = Vec::new();
        #[cfg(feature = "prettytable")]
        {
            data.push(vec!["label".into()]);
        }

        // we need to set the first block to contain itself only
        let all_nodes = function
            .blocks
            .iter()
            .map(|(id, _)| *id)
            .collect::<IndexSet<_>>();

        let first = *function.blocks.get_index(0).unwrap().0;

        self.dominator.insert(first, indexset!(first));
        // // intialise the each Domset to include all the nodes except the first
        for (id, _) in function.blocks.iter().skip(1) {
            self.dominator.insert(*id, all_nodes.clone());
        }

        let mut changed = true;

        while changed {
            changed = false;

            #[cfg(feature = "prettytable")]
            {
                for (id, _) in function.blocks.iter().rev() {
                    data.push(vec![id.to_string(), format!("{:?}", self.dominator[id])]);
                }

                if let Some(mut col) = data[0].get_mut(1) {
                    col = &mut format!("{} pass", iteration);
                } else {
                    data[0].push(format!("{} pass", iteration))
                }
            }
            for (id, _) in function.blocks.iter().rev() {
                let mut new_set = indexset!(*id);
                if let Some(predecessors) = self.predecessors.get(id) {
                    let mut intersection =
                        self.dominator[predecessors.get_index(0).unwrap()].clone();
                    for pred in predecessors.iter().skip(1) {
                        intersection = intersection
                            .intersection(&self.dominator[pred])
                            .cloned()
                            .collect::<IndexSet<_>>();
                    }

                    new_set.extend(intersection);
                };

                if new_set != self.dominator[id] {
                    *self.dominator.get_mut(id).unwrap() = new_set;
                    changed = true;
                    #[cfg(feature = "prettytable")]
                    {
                        iteration += 1;
                    }
                }
            }
        }

        #[cfg(feature = "dom")]
        {
            text_tables::render(&mut std::io::stdout(), &data).unwrap();
        }

        self.dominator.sort_keys();
    }

    pub fn find_dominance_frontier(&mut self, function: &Function) {
        for (id, _) in &function.blocks {
            self.frontier.insert(*id, IndexSet::new());
        }

        for (id, _) in function.blocks.iter() {
            if let Some(predecessors) = self.predecessors.get(id) {
                if predecessors.len() >= 2 {
                    for p in predecessors {
                        let mut runner = *p;

                        while runner != self.immediate_dominator(id).unwrap() {
                            self.frontier.get_mut(&runner).unwrap().insert(*id);
                            runner = self.immediate_dominator(&runner).unwrap();
                        }
                    }
                }
            }
        }

        self.frontier.sort_keys();
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
    use crate::instructions::{Block, BlockEnd, BlockID, Function, Program, Register};
    use indexmap::{indexmap, indexset};
    use pretty_assertions::assert_eq;

    #[test]

    fn check_predecessors() {
        let function = test_function();

        let mut analysis = AnalysisState::new(&function);

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

    #[test]
    fn check_dominator() {
        let function = test_function();

        let analysis = AnalysisState::new(&function);

        let expected_dom = indexmap!(
            BlockID(0) => indexset!(BlockID(0)),
            BlockID(1) => indexset!(BlockID(1),BlockID(0)),
            BlockID(2) => indexset!(BlockID(2),BlockID(1),BlockID(0)),
            BlockID(3) => indexset!(BlockID(3),BlockID(1),BlockID(0)),
            BlockID(4) => indexset!(BlockID(4),BlockID(3),BlockID(1),BlockID(0)),
            BlockID(5) => indexset!(BlockID(5),BlockID(1),BlockID(0)),
            BlockID(6) => indexset!(BlockID(6),BlockID(5),BlockID(1),BlockID(0)),
            BlockID(7) => indexset!(BlockID(7),BlockID(5),BlockID(1),BlockID(0)),
            BlockID(8) => indexset!(BlockID(8),BlockID(5),BlockID(1),BlockID(0)),
        );

        assert_eq!(analysis.dominator, expected_dom);
    }

    #[test]
    fn check_idom() {
        let function = test_function();

        let analysis = AnalysisState::new(&function);

        assert_eq!(analysis.immediate_dominator(&BlockID(0)), None);
        assert_eq!(analysis.immediate_dominator(&BlockID(1)), Some(BlockID(0)));
        assert_eq!(analysis.immediate_dominator(&BlockID(2)), Some(BlockID(1)));
        assert_eq!(analysis.immediate_dominator(&BlockID(3)), Some(BlockID(1)));
        assert_eq!(analysis.immediate_dominator(&BlockID(4)), Some(BlockID(3)));
        assert_eq!(analysis.immediate_dominator(&BlockID(5)), Some(BlockID(1)));
        assert_eq!(analysis.immediate_dominator(&BlockID(6)), Some(BlockID(5)));
        assert_eq!(analysis.immediate_dominator(&BlockID(7)), Some(BlockID(5)));
        assert_eq!(analysis.immediate_dominator(&BlockID(8)), Some(BlockID(5)));
    }

    #[test]
    fn check_dom_frontier() {
        let function = test_function();

        let analysis = AnalysisState::new(&function);

        let expected_frontier = indexmap!(
            BlockID(0) => indexset!(),
            BlockID(1) => indexset!(BlockID(1)),
            BlockID(2) => indexset!(BlockID(3),),
            BlockID(3) => indexset!(BlockID(1)),
            BlockID(4) => indexset!(),
            BlockID(5) => indexset!(BlockID(3)),
            BlockID(6) => indexset!(BlockID(7)),
            BlockID(7) => indexset!(BlockID(3)),
            BlockID(8) => indexset!(BlockID(7)),
        );

        assert_eq!(analysis.frontier, expected_frontier);
    }

    fn test_function() -> Function {
        let mut example_cfg = Vec::new();

        let b0 = BlockID(0);
        let b1 = BlockID(1);
        let b2 = BlockID(2);
        let b3 = BlockID(3);
        let b4 = BlockID(4);
        let b5 = BlockID(5);
        let b6 = BlockID(6);
        let b7 = BlockID(7);
        let b8 = BlockID(8);

        example_cfg.push((b0, Block::new(vec![], BlockEnd::Jump(b1))));
        example_cfg.push((
            b1,
            Block::new(vec![], BlockEnd::Branch(Register::new(), b2, b5)),
        ));
        example_cfg.push((b2, Block::new(vec![], BlockEnd::Jump(b3))));
        example_cfg.push((
            b5,
            Block::new(vec![], BlockEnd::Branch(Register::new(), b6, b8)),
        ));
        example_cfg.push((b6, Block::new(vec![], BlockEnd::Jump(b7))));
        example_cfg.push((b8, Block::new(vec![], BlockEnd::Jump(b7))));
        example_cfg.push((b7, Block::new(vec![], BlockEnd::Jump(b3))));
        example_cfg.push((
            b3,
            Block::new(vec![], BlockEnd::Branch(Register::new(), b4, b1)),
        ));
        example_cfg.push((b4, Block::new(vec![], BlockEnd::End)));

        let mut function = Function::dummy();

        function.blocks = example_cfg;

        function
    }
}
