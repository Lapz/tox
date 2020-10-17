use crate::{hir::NameId, HirDatabase};
use errors::{FileId, WithError};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ModuleGraph {
    nodes: HashSet<FileId>,
    edges: HashMap<FileId, HashMap<NameId, FileId>>,
}

impl ModuleGraph {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn insert_node(&mut self, node: FileId) {
        self.nodes.insert(node);
    }

    pub fn insert_edges(&mut self, from: FileId, to: FileId, weight: NameId) {
        {
            let edges = self.edges.entry(from).or_default();

            edges.insert(weight, to);
        }

        self.nodes.insert(from);
        self.nodes.insert(to);
        let _ = self.edges.entry(to).or_default();
    }

    pub fn try_get_node(&self, file: &FileId) -> Option<&HashMap<NameId, FileId>> {
        self.edges.get(file)
    }

    pub fn get_node(&self, file: &FileId) -> Option<&HashMap<NameId, FileId>> {
        self.edges.get(file)
    }

    pub fn merge(&mut self, other: ModuleGraph) {
        self.nodes.extend(other.nodes.iter());

        for node in other.nodes {
            let edges = self.edges.entry(node).or_default();

            edges.extend(other.edges[&node].iter())
        }
    }
}

pub(crate) fn module_graph_query(db: &impl HirDatabase, file: FileId) -> WithError<ModuleGraph> {
    let WithError(program, mut errors) = db.lower(file);

    let mut module_graph = ModuleGraph::new();

    for module in &program.modules {
        match db.resolve_modules(file, module.id) {
            WithError(Some(to), _) => {
                module_graph.insert_edges(file, to, module.name.item);

                let WithError(graph, error) = db.module_graph(to);
                errors.extend(error);
                module_graph.merge(graph)
            }
            WithError(None, error) => errors.extend(error),
        }
    }

    WithError(module_graph, errors)
}
