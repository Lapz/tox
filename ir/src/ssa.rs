use crate::analysis::AnalysisState;
use crate::instructions::{BlockEnd, BlockID, Function,Instruction};
use indexmap::map::Entry;
use indexmap::{indexset, IndexMap, IndexSet};

impl AnalysisState {
    pub fn find_global_names(&mut self, function: &Function) {
        for (id, block) in &function.blocks {
            let mut var_kill = IndexSet::new();

            for instruction in &block.instructions {
                let used = instruction.used();

                for reg in used {
                    if !var_kill.contains(&reg) {
                        self.global_regs.insert(reg);
                    }
                }

                var_kill.extend(instruction.def());

                if !instruction.def().is_empty() {
                    self.ssa_blocks
                        .entry(instruction.def().pop().unwrap())
                        .or_insert(IndexSet::new())
                        .insert(*id);
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
            

            for block_b in worklist.clone() {
                for block_d in &self.frontier[&block_b] {

                    if !inserted_phi_nodes.entry(block_d).or_insert(IndexSet::new()).contains(name) {
                        function.blocks.get_mut(block_d).unwrap().instructions.insert(0,Instruction::Phi(*name,*name,*name));
                        inserted_phi_nodes.get_mut(block_d).unwrap().insert(*name);
                        worklist.insert(*block_d);
                    }
                }
            }
        }
    }
}
