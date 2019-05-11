use crate::analysis::Allocator;
use crate::instructions::Register;
use petgraph::dot::{Config, Dot};
use std::{
    fs::{self, File},
    io::{self, Write},
    process::Command,
};

use petgraph::{
    graph::NodeIndex, graphmap::GraphMap, stable_graph::StableGraph, Directed, Graph, Undirected,
};
use util::symbol::Symbol;
impl<'a> Allocator<'a> {
    pub fn dump_debug(
        &self,
        name: Symbol,
        iteration: usize,
        graph: &GraphMap<Register, usize, Undirected>,
    ) {
        let name = self.symbols.name(name);

        fs::create_dir(&format!("graphviz/{}", name));
        let file_name = format!("graphviz/{}/{}_reg_{}.dot", name, name, iteration);

        File::create(&file_name)
            .unwrap()
            .write(
                Dot::with_config(graph, &[Config::EdgeNoLabel])
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

        let mut file =
            File::create(format!("graphviz/{}/{}_reg_{}.png", name, name, iteration)).unwrap();
        file.write(&output).unwrap();

        fs::remove_file(file_name);
    }
}
