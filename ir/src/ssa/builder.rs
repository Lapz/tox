use crate::analysis::AnalysisState;
use crate::instructions::{BlockEnd, BlockID, Function, Instruction, Program, Register};
use indexmap::map::Entry;
use indexmap::{indexset, IndexMap, IndexSet};
use util::symbol::Symbols;

#[derive(Debug)]
pub struct SSABuilder<'a> {
    pub successors: IndexMap<BlockID, IndexSet<BlockID>>,
    pub predecessors: IndexMap<BlockID, IndexSet<BlockID>>,
    pub dominator: IndexMap<BlockID, IndexSet<BlockID>>,
    pub frontier: IndexMap<BlockID, IndexSet<BlockID>>,
    pub ssa_blocks: IndexMap<Register, IndexSet<BlockID>>,
    pub global_regs: IndexSet<Register>,
    pub name_counter: IndexMap<Register, usize>,
    pub stack_counter: IndexMap<Register, Vec<usize>>,
    pub symbols: &'a mut Symbols<()>,
}

impl<'a> SSABuilder<'a> {
    pub fn new(function: &mut Function, symbols: &'a mut Symbols<()>) -> Self {
        let mut state = AnalysisState::empty();
        state.calculate_successors(function);

        let mut builder = SSABuilder {
            symbols,
            successors: IndexMap::new(),
            predecessors: IndexMap::new(),
            dominator: IndexMap::new(),
            frontier: IndexMap::new(),
            ssa_blocks: IndexMap::new(),
            global_regs: IndexSet::new(),
            name_counter: IndexMap::new(),
            stack_counter: IndexMap::new(),
        };

        builder.predecessors = state.predecessors;
        builder.successors = state.successors;

        builder
    }

    pub fn build_ssa(&mut self, function: &mut Function) {
        self.find_dominance(function);
        self.find_dominance_frontier(function);
        self.find_global_names(function);
        self.rewrite_code(function);
        self.rename_registers(function);
        self.rename(BlockID(0), function);
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

    pub fn find_global_names(&mut self, function: &Function) {
        for (id, block) in &function.blocks {
            let mut var_kill = IndexSet::new();

            for instruction in &block.instructions {
                let used = instruction.used();
                let defined = instruction.def();

                for reg in used {
                    self.ssa_blocks.entry(reg).or_insert(IndexSet::new());
                    if !var_kill.contains(&reg) {
                        self.global_regs.insert(reg);
                    }
                }

                var_kill.extend(defined.clone());

                if !instruction.def().is_empty() {
                    // self.ssa_blocks.insert(instruction.def().pop().unwrapIndexMap::new(),IndexSet::new());
                    self.ssa_blocks
                        .entry(instruction.def().pop().unwrap())
                        .or_insert(indexset!())
                        .insert(*id);

                    // println!("{:?}",self.ssa_blocks[&instruction.def().pop().unwrap()]);
                }
            }
        }
        println!("blocks {:#?}", self.ssa_blocks);
        println!("global {:#?}", self.global_regs);
    }

    pub fn rewrite_code(&mut self, function: &mut Function) {
        let mut inserted_phi_nodes = IndexMap::new();

        for name in &self.global_regs {
            let mut worklist = self.ssa_blocks[name].clone();
            println!("{:?}", worklist);

            for block_b in worklist.clone() {
                for block_d in &self.frontier[&block_b] {
                    if !inserted_phi_nodes
                        .entry(block_d)
                        .or_insert(IndexSet::new())
                        .contains(name)
                    {
                        function
                            .blocks
                            .get_mut(block_d)
                            .unwrap()
                            .instructions
                            .insert(0, Instruction::Phi(*name, *name, *name));
                        inserted_phi_nodes.get_mut(block_d).unwrap().insert(*name);
                        worklist.insert(*block_d);
                    }
                }
            }
        }
    }
    pub fn rename_registers(&mut self, function: &mut Function) {
        for name in &self.global_regs {
            self.name_counter.insert(*name, 0);
            self.stack_counter.insert(*name, vec![0]);
        }
    }

    pub fn rename(&mut self, b: BlockID, function: &mut Function) {
        for instruction in &mut function.blocks[&b].instructions {
            if instruction.is_phi() {
                let x = instruction.def().pop().unwrap();
                instruction.rewrite_phi_name(self.new_name(x));
            } else {
                match instruction {
                    Instruction::Binary(ref mut x, ref mut y, _, ref mut z) => {
                        *y = Register::Named(self.symbols.symbol(&format!(
                            "{}_{}",
                            y,
                            self.stack_counter.entry(*y).or_insert(Vec::new()).last().unwrap()
                        )));
                        *z = Register::Named(self.symbols.symbol(&format!(
                            "{}_{}",
                            z,
                            self.stack_counter.entry(*z).or_insert(Vec::new()).last().unwrap()
                        )));
                        *x = self.new_name(*x);
                    }
                    Instruction::Call(ref mut dest, _, _) => {
                        *dest = self.new_name(*dest);
                    }
                    Instruction::Cast(ref mut dest, ref mut val, _) => {
                        *val = Register::Named(self.symbols.symbol(&format!(
                            "{}_{}",
                            val,
                            self.stack_counter.entry(*val).or_insert(Vec::new()).last().unwrap()
                        )));
                        *dest = self.new_name(*dest);
                    }
                    Instruction::Unary(ref mut dest, ref mut val, _) => {
                        *val = Register::Named(self.symbols.symbol(&format!(
                            "{}_{}",
                            val,
                            self.stack_counter.entry(*val).or_insert(Vec::new()).last().unwrap()
                        )));
                        *dest = self.new_name(*dest);
                    }

                    Instruction::Store(ref mut dest, ref mut val) => {
                        *val = Register::Named(self.symbols.symbol(&format!(
                            "{}_{}",
                            val,
                            self.stack_counter.entry(*val).or_insert(Vec::new()).last().unwrap()
                        )));
                        *dest = self.new_name(*dest);
                    }
                    Instruction::StoreI(ref mut dest, _) => {
                        // *val =
                        *dest = self.new_name(*dest);
                    }
                    _ => (),
                }
            }
        }

        if let Some(successors) = self.successors.get(&b) {
            for suc in successors {
                for instruction in &mut function.blocks[suc].instructions {
                    if instruction.is_phi() {
                        match instruction {
                            Instruction::Phi(_, ref mut lhs, ref mut rhs) => {
                                *lhs = Register::Named(self.symbols.symbol(&format!(
                                    "{}_{}",
                                    lhs,
                                    self.stack_counter[lhs].last().unwrap()
                                )));
                                *rhs = Register::Named(self.symbols.symbol(&format!(
                                    "{}_{}",
                                    rhs,
                                    self.stack_counter[rhs].last().unwrap()
                                )));
                            }

                            _ => (),
                        }
                    }
                }
            }
        }

        if let Some(successors) = self.dominator.get(&b).cloned() {
            for suc in successors {
                if suc != b {
                    self.rename(suc, function);
                }
            }
        }

        for instruction in &function.blocks[&b].instructions {
            let mut def = instruction.def();

            if def.is_empty() {
                continue;
            }

            let x = def.pop().unwrap();

            if self.stack_counter.contains_key(&x) {
                self.stack_counter[&x].pop();
            }
        }
    }

    fn new_name(&mut self, n: Register) -> Register {
        let i = *self.name_counter.entry(n).or_insert(0);

        self.name_counter[&n] += 1;

        self.stack_counter.entry(n).or_insert(Vec::new()).push(i);

        let name = n.name(self.symbols);

        Register::Named(self.symbols.symbol(&format!("{}_{}", name, i)))
    }
}

pub fn ir_to_ssa(program: &mut Program, symbols: &mut Symbols<()>) {
    for function in &mut program.functions {
        let mut builder = SSABuilder::new(function, symbols);
        builder.build_ssa(function);
    }
}

#[cfg(test)]
mod test {
    use super::SSABuilder;
    use crate::instructions::{Block, BlockEnd, BlockID, Function, Program, Register};
    use indexmap::{indexmap, indexset, IndexMap};
    use pretty_assertions::assert_eq;
    use std::rc::Rc;
    use util::symbol::{SymbolFactory, Symbols};

    #[test]
    fn check_dominator() {
        let mut function = test_function();
        let mut symbols = Symbols::new(Rc::new(SymbolFactory::new()));

        let mut builder = SSABuilder::new(&mut function, &mut symbols);

        builder.find_dominance(&function);

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

        assert_eq!(builder.dominator, expected_dom);
    }

    #[test]
    fn check_idom() {
        let mut function = test_function();
        let mut symbols = Symbols::new(Rc::new(SymbolFactory::new()));

        let mut builder = SSABuilder::new(&mut function, &mut symbols);
        builder.find_dominance(&function);

        assert_eq!(builder.immediate_dominator(&BlockID(0)), None);
        assert_eq!(builder.immediate_dominator(&BlockID(1)), Some(BlockID(0)));
        assert_eq!(builder.immediate_dominator(&BlockID(2)), Some(BlockID(1)));
        assert_eq!(builder.immediate_dominator(&BlockID(3)), Some(BlockID(1)));
        assert_eq!(builder.immediate_dominator(&BlockID(4)), Some(BlockID(3)));
        assert_eq!(builder.immediate_dominator(&BlockID(5)), Some(BlockID(1)));
        assert_eq!(builder.immediate_dominator(&BlockID(6)), Some(BlockID(5)));
        assert_eq!(builder.immediate_dominator(&BlockID(7)), Some(BlockID(5)));
        assert_eq!(builder.immediate_dominator(&BlockID(8)), Some(BlockID(5)));
    }

    #[test]
    fn check_dom_frontier() {
        let mut function = test_function();
        let mut symbols = Symbols::new(Rc::new(SymbolFactory::new()));

        let mut builder = SSABuilder::new(&mut function, &mut symbols);
        builder.find_dominance(&function);
        builder.find_dominance_frontier(&function);

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

        assert_eq!(builder.frontier, expected_frontier);
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
}
