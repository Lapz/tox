use crate::instructions::{Block, BlockEnd, BlockID, Instruction, Program};
use petgraph::dot::{Config, Dot};
use petgraph::Graph;
use std::collections::HashMap;
use std::io::{self, Write};

const GRAPHSTART: &'static str = r##"digraph {
    rankdir=TD; ordering=out;
    color="#efefef";
    node[shape=box style=filled fontsize=8 fontname="Verdana" fillcolor="#efefef"];
    edge[fontsize=8 fontname="Verdana"];"##;

impl Program {
    pub fn graphviz<W: Write>(&self, out: &mut W) -> io::Result<()> {
        for function in self.functions.iter() {
            write!(out, "{}", GRAPHSTART)?;


            let mut end = Vec::new();

           

            for (id, block) in function.blocks.iter() {
                if block.instructions.is_empty() {
                    continue; // skip empty blocks
                }

                write!(out, "\n\t{} [label=\"",id.0)?;

                for inst in block.instructions.iter() {
                    write!(out, "\\l{}", inst)?;
                }

               
                write!(out, "\\l{}",block.end)?;
                match block.end {
                    BlockEnd::Jump(to) => {
                        
                        end.push(format!("{}->{}", id.0, to.0));
                    }

                    BlockEnd::Return(_) => (),
                    BlockEnd::End => (),
                    BlockEnd::Branch(_, t, f) => {
                        end.push(format!("{}->{}", id.0, t.0));
                        end.push(format!("{}->{}", id.0, f.0));
                    }
                }

                write!(out, "\"]")?;
            }


            for e in end {
                write!(out, "\n\t{}",e)?;
            }

            write!(out, "\n}}")?;
        }

        Ok(())
    }
}
