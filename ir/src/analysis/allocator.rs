use crate::analysis::AnalysisState;
use crate::instructions::{BlockEnd, BlockID, Function, Instruction, Register};
use indexmap::set::IndexSet;

use petgraph::{graphmap::GraphMap, stable_graph::StableGraph, Directed, Graph, Undirected};
use std::collections::{hash_map::Entry, HashMap, HashSet};
use std::default::Default;
use util::symbol::Symbols;

pub struct Interval {
    start: usize,
    end: usize,
}

impl Interval {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

const NUMBER_REGISTER: usize = 3;

#[derive(Debug)]
pub struct Allocator<'a> {
    state: AnalysisState,
    function: &'a mut Function,
    pub(crate) symbols: &'a mut Symbols<()>,
    move_list: HashMap<Register, HashSet<Register>>,
    work_list_moves: HashSet<Register>,
    precolored: HashSet<Register>,
    initial: HashSet<Register>,
    spill_work_list: Vec<Register>,
    spilled_nodes: HashSet<Register>,
    freeze_work_list: Vec<Register>,
    simplify_work_list: Vec<Register>,
    select_stack: Vec<Register>,
    coalesced_nodes: Vec<Register>,
    coalesced_moves: HashSet<Register>,
    active_moves: HashSet<Register>,
    constrained_moves: HashSet<Register>,
    alias: HashMap<Register, Register>,
    frozen_moves: Vec<Register>,
    colored_nodes: HashSet<Register>,
    color: HashMap<Register, usize>, // change to colour enum
    ok_colors: Vec<usize>,           // change to colour enum
}

macro_rules! hashset {
    () => { HashSet::new() };

    ( $($x:expr),* ) => {{
        let mut l = HashSet::new();
        $(
            l.insert($x);
        )*
            l
    }};

    ( $($x:expr ,)* ) => {{
        let mut l = HashSet::new();
        $(
            l.insert($x);
        )*
            l
    }};
}

impl<'a> Allocator<'a> {
    pub fn new(symbols: &'a mut Symbols<()>, function: &'a mut Function) -> Self {
        Allocator {
            symbols,
            function,
            state: AnalysisState::empty(),
            work_list_moves: HashSet::new(),
            move_list: HashMap::new(),
            precolored: HashSet::new(),
            initial: HashSet::new(),
            spill_work_list: Vec::new(),
            freeze_work_list: Vec::new(),
            simplify_work_list: Vec::new(),
            select_stack: Vec::new(),
            coalesced_nodes: Vec::new(),
            active_moves: HashSet::new(),
            ok_colors: Vec::new(),
            coalesced_moves: HashSet::new(),
            constrained_moves: HashSet::new(),
            spilled_nodes: HashSet::new(),
            frozen_moves: Vec::new(),
            colored_nodes: HashSet::new(),
            color: HashMap::new(),
            alias: HashMap::new(),
        }
    }

    pub fn allocate(&mut self, count: usize) {
        self.state = AnalysisState::new(self.function);

        let mut graph = self.build_graph();
        #[cfg(feature = "graphviz")]
        let mut count = 0;

        self.make_work_list(&graph);

        while {
            if !self.simplify_work_list.is_empty() {
                self.simpilfy(&mut graph);
            } else if !self.work_list_moves.is_empty() {
                self.colaesce(&mut graph)
            } else if !self.freeze_work_list.is_empty() {
                self.freeze();
            } else if !self.spill_work_list.is_empty() {
                self.select_spill()
            }

            count += 1;

            !(self.simplify_work_list.is_empty()
                && self.work_list_moves.is_empty()
                && self.freeze_work_list.is_empty()
                && self.spill_work_list.is_empty())
        } {}

        self.assign_colors(&graph);

        println!("{:#?}", self);

        if !self.spilled_nodes.is_empty() {
            self.allocate(count + 1)
        }

        // #[cfg(feature = "graphviz")]
        // self.dump_debug(self.function.name, count,&graph);
    }

    pub fn build_graph(&mut self) -> GraphMap<Register, usize, Undirected> {
        let mut graph = GraphMap::with_capacity(5, 5);

        for (id, block) in &self.function.blocks {
            let mut live = self.state.live_out[id].clone();

            for instruction in block.instructions.iter().rev() {
                let used = instruction.used();
                let defined = instruction.def();
                if instruction.is_move() {
                    live = live.difference(&used).cloned().collect::<HashSet<_>>();

                    for register in instruction.def().union(&used) {
                        self.move_list
                            .entry(*register)
                            .or_insert(HashSet::new())
                            .insert(*register);
                        self.work_list_moves.insert(*register);
                    }
                }

                live = live.union(&defined).cloned().collect::<HashSet<Register>>();

                for def in &defined {
                    for live in &live {
                        self.add_edge(*live, *def, &mut graph)
                    }
                }

                live = used
                    .union(
                        &live
                            .difference(&defined)
                            .map(|x| *x)
                            .collect::<HashSet<_>>(),
                    )
                    .cloned()
                    .collect::<HashSet<_>>()
            }
        }

        graph
    }

    fn make_work_list(&mut self, graph: &GraphMap<Register, usize, Undirected>) {
        let mut initial = HashSet::new();

        std::mem::swap(&mut self.initial, &mut initial); // swap them out because we need to empty out the list

        for initial in initial {
            let degree = graph.edges(initial).count();

            if degree >= NUMBER_REGISTER {
                self.spill_work_list.push(initial)
            } else if self.move_related(initial) {
                self.freeze_work_list.push(initial);
            } else {
                self.simplify_work_list.push(initial)
            }
        }
    }

    // helper functions
    /// Add an edge between two register if they are not the same and edge between them is not already present in the graph
    fn add_edge(
        &self,
        u: Register,
        v: Register,
        graph: &mut GraphMap<Register, usize, Undirected>,
    ) {
        if !graph.contains_edge(u, v) && u != v {
            if !self.precolored.contains(&u) && !self.precolored.contains(&v) {
                graph.add_edge(u, v, 1);
            }
        }
    }

    /// find all the nodes that are adjacent to the selected one in the graph.
    fn adjacent(
        &self,
        n: Register,
        graph: &GraphMap<Register, usize, Undirected>,
    ) -> HashSet<Register> {
        let union = self
            .select_stack
            .clone()
            .into_iter()
            .collect::<HashSet<_>>()
            .union(
                &self
                    .coalesced_nodes
                    .clone()
                    .into_iter()
                    .collect::<HashSet<_>>(),
            )
            .cloned()
            .collect::<HashSet<_>>();
        graph
            .neighbors(n)
            .collect::<HashSet<_>>()
            .difference(&union)
            .cloned()
            .collect::<HashSet<_>>()
    }

    fn node_moves(&self, n: Register) -> HashSet<Register> {
        if self.move_list.get(&n).is_some() {
            self.move_list[&n]
                .intersection(
                    &self
                        .active_moves
                        .union(&self.work_list_moves)
                        .cloned()
                        .collect::<HashSet<_>>(),
                )
                .cloned()
                .collect::<HashSet<_>>()
        } else {
            HashSet::new()
        }
    }

    fn move_related(&self, n: Register) -> bool {
        self.node_moves(n).is_empty()
    }

    fn simpilfy(&mut self, graph: &mut GraphMap<Register, usize, Undirected>) {
        if self.simplify_work_list.is_empty() {
            return;
        }

        let node = self.simplify_work_list.pop().unwrap();

        self.select_stack.push(node);

        let adjacent = self.adjacent(node, graph);

        for neighbour in adjacent {
            self.decrement_degree(node, neighbour, graph)
        }
    }

    fn decrement_degree(
        &mut self,
        node: Register,
        neighbour: Register,
        graph: &mut GraphMap<Register, usize, Undirected>,
    ) {
        let degree = graph.edges(node).count();

        graph.remove_edge(node, neighbour);

        if degree - 1 == NUMBER_REGISTER {
            let mut nodes = self.adjacent(node, graph);

            nodes.insert(node);

            self.enable_moves(nodes);
            self.spill_work_list.remove_item(&node);

            if self.move_related(node) {
                self.freeze_work_list.push(node);
            } else {
                self.simplify_work_list.push(node)
            }
        }
    }

    fn enable_moves(&mut self, nodes: HashSet<Register>) {
        for node in nodes {
            for m in self.node_moves(node) {
                if self.active_moves.contains(&m) {
                    self.active_moves.remove(&m);
                    self.work_list_moves.insert(m);
                }
            }
        }
    }

    fn add_work_list(&mut self, node: Register, graph: &GraphMap<Register, usize, Undirected>) {
        if !self.precolored.contains(&node)
            && !(self.move_related(node) && graph.edges(node).count() < NUMBER_REGISTER)
        {
            self.freeze_work_list.remove_item(&node);
            self.simplify_work_list.push(node);
        }
    }

    ///implements the heuristic used for coalescing a precolored register
    //think of a better name
    fn ok(&self, t: Register, r: Register, graph: &GraphMap<Register, usize, Undirected>) -> bool {
        graph.edges(t).count() < NUMBER_REGISTER
            || self.precolored.contains(&t)
            || graph.contains_edge(t, r)
    }

    /// Conservative implements the conservative coalescing heuristic.
    fn conservative(
        &mut self,
        nodes: Vec<Register>,
        graph: &mut GraphMap<Register, usize, Undirected>,
    ) -> bool {
        let mut k = 0;

        for node in nodes {
            if graph.edges(node).count() >= NUMBER_REGISTER {
                k += 1;
            }
        }

        k < NUMBER_REGISTER
    }

    fn colaesce(&mut self, graph: &mut GraphMap<Register, usize, Undirected>) {
        if self.work_list_moves.len() < 2 {
            return;
        }

        let mut work_list_moves = HashSet::new();

        std::mem::swap(&mut self.work_list_moves, &mut work_list_moves);

        let mut work_list_moves = work_list_moves.into_iter().collect::<Vec<_>>();

        let x = self.get_alias(work_list_moves.pop().unwrap());

        let y = self.get_alias(work_list_moves.pop().unwrap());

        let (u, v) = if self.precolored.contains(&y) {
            (y, x)
        } else {
            (x, y)
        };

        if u == v {
            self.coalesced_moves.insert(u);
            self.coalesced_moves.insert(v);
            self.add_work_list(u, graph);
            self.add_work_list(v, graph);
        } else if self.precolored.contains(&v) || graph.contains_edge(u, v) {
            self.constrained_moves.insert(u);
            self.constrained_moves.insert(v);

            self.add_work_list(u, graph);
            self.add_work_list(v, graph);
        }
        // u ∈ precolored ∧ (∀t ∈ Adjacent(v), OK(t, u)) ∨ u ̸∈ precolored ∧ Conservative(Adjacent(u) ∪ Adjacent(v))
        else if self.precolored.contains(&u) && {
            let mut result = false;

            for t in self.adjacent(v, graph) {
                result = self.ok(t, u, graph);
            }
            result
        } || {
            !self.precolored.contains(&u)
                && self.conservative(
                    self.adjacent(u, graph)
                        .union(&self.adjacent(v, graph))
                        .cloned()
                        .collect::<Vec<_>>(),
                    graph,
                )
        } {
            self.coalesced_moves.insert(u);
            self.coalesced_moves.insert(v);

            self.combine(u, v, graph);

            self.add_work_list(u, graph);
        } else {
            self.active_moves.insert(u);
            self.active_moves.insert(v);
        }
    }

    fn combine(
        &mut self,
        u: Register,
        v: Register,
        graph: &mut GraphMap<Register, usize, Undirected>,
    ) {
        if self.freeze_work_list.contains(&v) {
            self.freeze_work_list.remove_item(&v);
        } else {
            self.spill_work_list.remove_item(&v);
        }

        self.coalesced_nodes.push(v);

        self.alias.insert(v, u);

        let union = self.move_list[&u]
            .union(&self.move_list[&v])
            .cloned()
            .collect::<HashSet<_>>();

        *self.move_list.get_mut(&u).unwrap() = union;

        self.enable_moves(hashset!(v));

        for t in self.adjacent(v, graph) {
            self.add_edge(t, u, graph);
            self.decrement_degree(v, t, graph);
        }

        if graph.edges(u).count() >= NUMBER_REGISTER && self.freeze_work_list.contains(&u) {
            self.freeze_work_list.remove_item(&u);
            self.spill_work_list.push(u);
        }
    }

    fn get_alias(&self, n: Register) -> Register {
        if self
            .coalesced_nodes
            .iter()
            .collect::<HashSet<_>>()
            .contains(&n)
        {
            self.get_alias(self.alias[&n])
        } else {
            n
        }
    }

    fn freeze(&mut self) {
        if self.freeze_work_list.is_empty() {
            return;
        }

        let node = self.freeze_work_list.pop().unwrap();

        self.simplify_work_list.push(node);

        self.freeze_moves(node);
    }

    fn freeze_moves(&mut self, node: Register) {
        let node_moves = self.node_moves(node).into_iter().collect::<Vec<_>>();

        for chunk in node_moves.chunks(2) {
            let x = chunk[0];
            let y = chunk[1];
            let y_alias = self.get_alias(y);
            let node_alias = self.get_alias(node);
            let v = if y_alias == node_alias {
                self.get_alias(x)
            } else {
                y_alias
            };

            self.active_moves.remove(&x);
            self.active_moves.remove(&y);

            self.frozen_moves.push(x);
            self.frozen_moves.push(y);

            if self.freeze_work_list.contains(&v) && self.node_moves(v).is_empty() {
                self.freeze_work_list.remove_item(&v);
                self.simplify_work_list.push(v);
            }
        }
    }

    fn select_spill(&mut self) {
        if self.spill_work_list.is_empty() {
            return;
        } else {
            let register = self.spill_work_list.pop().unwrap(); // tot add a heuristic to get this
            self.simplify_work_list.push(register);

            self.freeze_moves(register);
        }
        // let register = self.
    }

    fn assign_colors(&mut self, graph: &GraphMap<Register, usize, Undirected>) {
        while !self.select_stack.is_empty() {
            let node = self.select_stack.pop().unwrap();

            self.ok_colors = (0..NUMBER_REGISTER - 1).collect::<Vec<_>>();

            for neighbor in graph.neighbors(node) {
                if self
                    .colored_nodes
                    .union(&self.precolored)
                    .cloned()
                    .collect::<HashSet<_>>()
                    .contains(&self.get_alias(neighbor))
                {
                    self.ok_colors
                        .remove_item(&self.color[&self.get_alias(neighbor)]);
                }
            }

            if self.ok_colors.is_empty() {
                self.spilled_nodes.insert(node);
            } else {
                self.colored_nodes.insert(node);

                let c = self.ok_colors[0];
                self.color.insert(node, c);
            }

            for node in &self.coalesced_nodes {
                let alias = self.get_alias(*node);

                match self.color.get(&alias) {
                    Some(alias_colour) => {
                        self.color.insert(*node, *alias_colour);
                    }
                    None => (),
                }
            }
        }
    }
}
