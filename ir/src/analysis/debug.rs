use crate::analysis::color2::Allocator;
use crate::instructions::Register;
use petgraph::dot::{Config, Dot};
use petgraph::visit::{IntoEdgeReferences, IntoNodeReferences, NodeIndexable};
use std::{
    collections::{HashMap, HashSet},
    fmt,
    fs::{self, File},
    io::{self, Write},
    process::Command,
};

use petgraph::{
    graph::NodeIndex, graphmap::GraphMap, stable_graph::StableGraph, Directed, Graph, Undirected,
};
use util::symbol::Symbol;

const GRAPHSTART: &'static str = r##"graph {
    ordering=out;
    color="#efefef";
    edge[fontsize=8 fontname="Verdana"];"##;

impl<'a> Allocator<'a> {
    pub fn dump_debug(&self, name: Symbol, iteration: usize) {
        let name = self.symbols.name(name);

        fs::create_dir(&format!("graphviz/{}", name));
        let file_name = format!("graphviz/{}/{}_reg_{}.dot", name, name, iteration);

        let mut file = File::create(&file_name).unwrap();

        write!(&mut file, "{}\n", GRAPHSTART);

        let mut indexs = HashMap::new();

        //output nodes
        for (i, (node, ns)) in self.adj_list.iter().enumerate() {
            indexs.insert(node, i);

            for (j, n) in ns.iter().enumerate() {
                indexs.insert(n, j);
            }
            write!(&mut file, "\t{} [label=\"{}\"", i, node).unwrap();
            if let Some(colour) = self.color.get(node) {
                match colour {
                    0 => write!(&mut file, "fillcolor=red,style=filled").unwrap(),
                    1 => write!(&mut file, "fillcolor=green,style=filled").unwrap(),
                    2 => write!(&mut file, "fillcolor=blue,style=filled").unwrap(),
                    _ => unreachable!(),
                }
            }
            write!(&mut file, "]\n");
        }

        let mut seen = HashSet::new();
        //output edges
        for node in &self.adj_list {
            for (from, to) in &self.adj_set {
                if !seen.contains(&(from, to)) || !seen.contains(&(to, from)) && from != to {
                    writeln!(&mut file, "\t {} -- {}", indexs[from], indexs[to]).unwrap();

                    seen.insert((from, to));
                    seen.insert((to, from));
                }
            }
        }

        write!(&mut file, "}}").unwrap();

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
