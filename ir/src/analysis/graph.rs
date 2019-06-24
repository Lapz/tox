use crate::instructions::Register;
use indexmap::{IndexMap, IndexSet};

pub type Node = Register;

pub struct InterferenceGraph {
    /// A set of edges between nodes
    adj_set: IndexSet<(Node, Node)>,
    /// The neighbours for each node
    adj_list: IndexMap<Node, IndexSet<Node>>,
    /// the degree of each node
    degree: IndexMap<Node, usize>,
}

impl InterferenceGraph {
    pub fn new() -> Self {
        InterferenceGraph {
            adj_set: IndexSet::new(),
            adj_list: IndexMap::new(),
            degree: IndexMap::new(),
        }
    }

    // pub fn add_degree(&mut self,u:Node,v:Node) {
    //     if !(self.adj_set.contains(&(u,v)) && u != v {

    //         // self.degree
    //     }
    // }
}
