use crate::analysis::AnalysisState;
use crate::instructions::{BlockEnd, BlockID, Function, Instruction,Register};
use indexmap::map::Entry;
use indexmap::{indexset, IndexMap, IndexSet};
use util::symbol::Symbols;

impl AnalysisState {
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
                    // self.ssa_blocks.insert(instruction.def().pop().unwrap(),IndexSet::new());
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

    pub fn rename_registers(&mut self,function:&mut Function,symbols:&mut Symbols<()>) {
        for name in &self.global_regs {
            self.name_counter.insert(*name,0);
            self.stack_counter.insert(*name,Vec::new());
        }
    }

    pub fn rename(&mut self,b:BlockID,function:&mut Function,symbols:&mut Symbols<()>) {
        for instruction in &mut function.blocks[&b].instructions {
            if instruction.is_phi() {
                let x = instruction.def().pop().unwrap();
                instruction.rewrite_phi_name(self.new_name(x,symbols));
            }else {
                
            }
        }
    }

    fn new_name(&mut self,n:Register,symbols:&mut Symbols<()>) -> Register {
        let i = self.name_counter[&n];

        self.name_counter[&n] += 1;

        self.stack_counter.get_mut(&n).unwrap().push(i);

        let name = n.name(symbols);

        Register::Named(symbols.symbol(&format!("{}_{}",name,i)))
    }
}
