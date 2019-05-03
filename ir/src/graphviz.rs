use crate::instructions::{Block, BlockEnd, BlockID, Function, Instruction, Program};
use petgraph::dot::{Config, Dot};
use petgraph::Graph;
use std::collections::HashMap;
use std::fs::{self, File};
use std::io::{self, Write};
use std::process::Command;
use util::symbol::Symbols;

const GRAPHSTART: &'static str = r##"digraph {
    rankdir=TD; ordering=out;
    color="#efefef";
    node[shape=box style=filled fontsize=8 fontname="Verdana" fillcolor="#efefef"];
    edge[fontsize=8 fontname="Verdana"];"##;

impl Program {
    fn viz<W: Write>(&self, out: &mut W, function: &Function) -> io::Result<()> {
        write!(out, "{}", GRAPHSTART)?;

        let mut end = Vec::new();

        for (id, block) in function.blocks.iter() {
            if block.instructions.is_empty() {
                continue; // skip empty blocks
            }

            write!(out, "\n\t{} [label=\"{}", id.0, id)?;

            for inst in block.instructions.iter() {
                write!(out, "\\l{}", inst)?;
            }

            write!(out, "\\l{}", block.end)?;
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
                BlockEnd::Link(to) => {
                    end.push(format!("{}->{}", id.0, to.0));
                }
            }

            write!(out, "\"]")?;
        }

        for e in end {
            write!(out, "\n\t{}", e)?;
        }

        write!(out, "\n}}")?;

        Ok(())
    }

    pub fn graphviz(&self, symbols: &Symbols<()>) -> io::Result<()> {
        fs::create_dir("./graphviz/").unwrap_or(());

        for function in self.functions.iter() {
            let file_name = format!("graphviz/{}.dot", symbols.name(function.name));

            self.viz(&mut File::create(&file_name).unwrap(), function)?;
            self.viz(&mut std::io::stdout(),function)?;

            let mut dot = Command::new("dot");

            let output = dot
                .args(&["-Tpng", &file_name])
                .output()
                .expect("failed to execute process")
                .stdout;

            let mut file =
                File::create(&format!("graphviz/{}.png", symbols.name(function.name))).unwrap();

            file.write(&output)?;

            fs::remove_file(file_name)?;
        }

        Ok(())
    }
}
