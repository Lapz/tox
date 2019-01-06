use crate::instructions::{Block, BlockEnd, BlockID, Instruction, Program};
use petgraph::Graph;
use std::collections::HashMap;
use std::io::{self, Write};
use petgraph::dot::{Dot, Config};
impl Program {
    pub fn graphviz<W: Write>(&self, out: &mut W) -> io::Result<()> {
        

        for function in self.functions.iter() {
            let mut graph = Graph::<&BlockID,&Block>::new();

            let mut mappings = HashMap::new();

            for (id, block) in function.blocks.iter() {
                let node = graph.add_node(&id);

                mappings.insert(*id, node);

                
            }

            for (id,block) in function.blocks.iter() {

                if block.instructions.is_empty() {
                    continue; // skip empty blocks
                    
                }
                match block.end {
                    BlockEnd::Jump(to) => {
                        graph.update_edge(mappings[&id],mappings[&to],block);
                    },

                    BlockEnd::Return(_) => (),
                    BlockEnd::End => (),
                    BlockEnd::Branch(_,t,f) => {
                         graph.update_edge(mappings[&function.start_block],mappings[&t],block);
                         graph.update_edge(mappings[&function.start_block],mappings[&f],block);
                    }
                }
            }

            writeln!(out, "{:?}",Dot::new(&graph))?;
        }

        Ok(())
    }
}
