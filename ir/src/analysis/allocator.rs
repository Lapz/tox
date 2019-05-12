use crate::analysis::AnalysisState;
use crate::instructions::{Function, Instruction, Register};
use indexmap::{IndexMap, IndexSet};

use petgraph::{graphmap::GraphMap, Undirected};

use util::symbol::Symbols;

pub struct Interval {
    start: usize,
    end: usize,
}

/// store $to $from
#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct Move {
    from: Register,
    to: Register,
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
    move_list: IndexMap<Register, IndexSet<Move>>,
    work_list_moves: IndexSet<Move>,
    precolored: IndexSet<Register>,
    initial: IndexSet<Register>,
    spill_work_list: Vec<Register>,
    spilled_nodes: IndexSet<Register>,
    freeze_work_list: Vec<Register>,
    simplify_work_list: Vec<Register>,
    select_stack: IndexSet<Register>,
    coalesced_nodes: Vec<Register>,
    coalesced_moves: IndexSet<Move>,
    active_moves: IndexSet<Move>,
    constrained_moves: IndexSet<Move>,
    alias: IndexMap<Register, Register>,
    frozen_moves: Vec<Move>,
    colored_nodes: IndexSet<Register>,
    degree: IndexMap<Register, usize>,
    next_colour: usize,
    pub(crate) color: IndexMap<Register, usize>, // change to colour enum
    ok_colors: Vec<usize>,                       // change to colour enum
}

macro_rules! hashset {
    () => { IndexSet::new() };

    ( $($x:expr),* ) => {{
        let mut l = IndexSet::new();
        $(
            l.insert($x);
        )*
            l
    }};

    ( $($x:expr ,)* ) => {{
        let mut l = IndexSet::new();
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
            work_list_moves: IndexSet::new(),
            move_list: IndexMap::new(),
            precolored: IndexSet::new(),
            initial: IndexSet::new(),
            spill_work_list: Vec::new(),
            freeze_work_list: Vec::new(),
            simplify_work_list: Vec::new(),
            select_stack: IndexSet::new(),
            coalesced_nodes: Vec::new(),
            active_moves: IndexSet::new(),
            ok_colors: Vec::new(),
            coalesced_moves: IndexSet::new(),
            constrained_moves: IndexSet::new(),
            spilled_nodes: IndexSet::new(),
            frozen_moves: Vec::new(),
            colored_nodes: IndexSet::new(),
            color: IndexMap::new(),
            degree: IndexMap::new(),
            next_colour: 0,
            alias: IndexMap::new(),
        }
    }

    pub fn allocate(&mut self, mut count: usize) {
        self.state = AnalysisState::new(self.function);

        let mut graph = self.build_graph();

        for node in graph.nodes() {
            self.degree.insert(node, graph.edges(node).count());
        }
        // #[cfg(feature = "graphviz")]

        self.make_work_list(&graph);

        #[cfg(feature = "graphviz")]
        self.dump_debug(self.function.name, count, &graph);

        while {
            // println!("spill {:?}", self.spill_work_list);
            // println!("wkm {:?}", self.work_list_moves);
            // println!("simp {:?}", self.simplify_work_list);

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

            #[cfg(feature = "graphviz")]
            self.dump_debug(self.function.name, count, &graph);

            !(self.simplify_work_list.is_empty()
                && self.work_list_moves.is_empty()
                && self.freeze_work_list.is_empty()
                && self.spill_work_list.is_empty())
        } {}

        // println!("select {:?}", self.select_stack);
        self.assign_colors(&graph);

        if !self.spilled_nodes.is_empty() {
            self.rewrite_program();
            self.allocate(count + 1);
            //  panic!();
        }

        #[cfg(feature = "graphviz")]
        self.dump_debug(self.function.name, count + 1, &graph);
    }

    pub fn build_graph(&mut self) -> GraphMap<Register, usize, Undirected> {
        let mut graph: GraphMap<Register, usize, Undirected> = GraphMap::with_capacity(5, 5);

        for (id, block) in &self.function.blocks {
            let mut live = self.state.live_out[id].clone();

            for instruction in block.instructions.iter().rev() {
                let used = instruction.used();
                let defined = instruction.def();
                if instruction.is_move() {
                    live = live.difference(&used).cloned().collect::<IndexSet<_>>();

                    let reg_move = match instruction {
                        Instruction::Store(to, from) => Move {
                            to: *to,
                            from: *from,
                        },
                        _ => unreachable!(),
                    };

                    for register in defined.union(&used) {
                        self.move_list.entry(*register).or_insert(IndexSet::new());

                        self.move_list.get_mut(register).unwrap().insert(reg_move);
                        self.work_list_moves.insert(reg_move);
                    }
                }

                live = live
                    .union(&defined)
                    .cloned()
                    .collect::<IndexSet<Register>>();

                for def in &defined {
                    for live in &live {
                        //copy to appease the great and wondefull borrow checker
                        if !graph.contains_edge(*live, *def) && live != def {
                            graph.add_node(*live); // add to the graph
                            graph.add_node(*def); // add to the graph

                            self.degree.insert(*live, 0); // init the degree
                            self.degree.insert(*def, 0); // init the degree

                            if !self.precolored.contains(live) && !self.precolored.contains(def) {
                                graph.add_edge(*live, *def, 1);
                                *self.degree.get_mut(live).unwrap() += 1;

                                *self.degree.get_mut(def).unwrap() += 1;
                            }
                        }
                    }
                }

                live = used
                    .union(
                        &live
                            .difference(&defined)
                            .map(|x| *x)
                            .collect::<IndexSet<_>>(),
                    )
                    .cloned()
                    .collect::<IndexSet<_>>()
            }
        }

        graph
    }

    fn make_work_list(&mut self, graph: &GraphMap<Register, usize, Undirected>) {
        let mut initial = IndexSet::new();

        std::mem::swap(&mut self.initial, &mut initial); // swap them out because we need to empty out the list

        for initial in initial {
            if self.degree[&initial] >= NUMBER_REGISTER {
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
        &mut self,
        u: Register,
        v: Register,
        graph: &mut GraphMap<Register, usize, Undirected>,
    ) {
        if !graph.contains_edge(u, v) && u != v {
            if !self.precolored.contains(&u) && !self.precolored.contains(&v) {
                graph.add_edge(u, v, 1);
                *self.degree.get_mut(&u).unwrap() += 1;
                *self.degree.get_mut(&v).unwrap() += 1;
            }
        }
    }

    /// find all the nodes that are adjacent to the selected one in the graph.
    fn adjacent(
        &self,
        n: Register,
        graph: &GraphMap<Register, usize, Undirected>,
    ) -> IndexSet<Register> {
        let union = self
            .select_stack
            .clone()
            .into_iter()
            .collect::<IndexSet<_>>()
            .union(
                &self
                    .coalesced_nodes
                    .clone()
                    .into_iter()
                    .collect::<IndexSet<_>>(),
            )
            .cloned()
            .collect::<IndexSet<_>>();
        graph
            .neighbors(n)
            .collect::<IndexSet<_>>()
            .difference(&union)
            .cloned()
            .collect::<IndexSet<_>>()
    }

    fn node_moves(&self, n: Register) -> IndexSet<Move> {
        if self.move_list.get(&n).is_some() {
            self.move_list[&n]
                .intersection(
                    &self
                        .active_moves
                        .union(&self.work_list_moves)
                        .cloned()
                        .collect::<IndexSet<_>>(),
                )
                .cloned()
                .collect::<IndexSet<_>>()
        } else {
            IndexSet::new()
        }
    }

    fn move_related(&self, n: Register) -> bool {
        !self.node_moves(n).is_empty()
    }

    fn simpilfy(&mut self, graph: &mut GraphMap<Register, usize, Undirected>) {
        let node = self.simplify_work_list.pop().unwrap();

        self.select_stack.insert(node);

        let adjacent = self.adjacent(node, graph);

        for neighbour in adjacent {
            self.decrement_degree(neighbour, graph)
        }
    }

    fn decrement_degree(
        &mut self,
        node: Register,
        graph: &mut GraphMap<Register, usize, Undirected>,
    ) {
        let degree = self.degree[&node];

         *self.degree.get_mut(&node).unwrap() -= 1;

        println!("decreasing edge for node {} ", node);

        if degree < NUMBER_REGISTER {
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

    fn enable_moves(&mut self, nodes: IndexSet<Register>) {
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
            && !(self.move_related(node) && self.degree[&node] < NUMBER_REGISTER)
        {
            self.freeze_work_list.remove_item(&node);
            self.simplify_work_list.push(node);
        }
    }

    ///implements the heuristic used for coalescing a precolored register
    //think of a better name
    fn ok(&self, t: Register, r: Register, graph: &GraphMap<Register, usize, Undirected>) -> bool {
        self.degree[&t] < NUMBER_REGISTER
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
            if self.degree[&node] >= NUMBER_REGISTER {
                k += 1;
            }
        }

        k < NUMBER_REGISTER
    }

    fn colaesce(&mut self, graph: &mut GraphMap<Register, usize, Undirected>) {
        let mut work_list_moves = IndexSet::new();

        std::mem::swap(&mut self.work_list_moves, &mut work_list_moves);

        let mut work_list_moves = work_list_moves.into_iter().collect::<Vec<_>>();

        let work_move = work_list_moves.pop().unwrap();

        let x = self.get_alias(work_move.to);

        let y = self.get_alias(work_move.from);

        let (u, v) = if self.precolored.contains(&y) {
            (y, x)
        } else {
            (x, y)
        };

        if u == v {
            self.coalesced_moves.insert(work_move);
            self.add_work_list(u, graph);
        } else if self.precolored.contains(&v) || graph.contains_edge(u, v) {
            self.constrained_moves.insert(work_move);

            self.add_work_list(u, graph);
            self.add_work_list(v, graph);
        }
        // u ∈ precolored ∧ (∀t ∈ Adjacent(v), OK(t, u)) ∨ u ̸∈ precolored ∧ Conservative(Adjacent(u) ∪ Adjacent(v))
        else if self.precolored.contains(&u) && self.ok(u, v, graph)
            || !self.precolored.contains(&u)
                && self.conservative(
                    self.adjacent(u, graph)
                        .union(&self.adjacent(v, graph))
                        .cloned()
                        .collect::<Vec<_>>(),
                    graph,
                )
        {
            self.coalesced_moves.insert(work_move);
            self.combine(u, v, graph);
            self.add_work_list(u, graph);
        } else {
            self.active_moves.insert(work_move);
        }

        let mut work_list_moves = work_list_moves.into_iter().collect::<IndexSet<_>>();
        std::mem::swap(&mut work_list_moves, &mut self.work_list_moves);
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
            .collect::<IndexSet<_>>();

        *self.move_list.get_mut(&u).unwrap() = union;

        self.enable_moves(hashset!(v));

        for t in self.adjacent(v, graph) {
            self.add_edge(t, u, graph);
            self.decrement_degree(v, graph);
        }

        if self.degree[&u] >= NUMBER_REGISTER && self.freeze_work_list.contains(&u) {
            self.freeze_work_list.remove_item(&u);
            self.spill_work_list.push(u);
        }
    }

    fn get_alias(&self, n: Register) -> Register {
        if self
            .coalesced_nodes
            .iter()
            .collect::<IndexSet<_>>()
            .contains(&n)
        {
            self.get_alias(self.alias[&n])
        } else {
            n
        }
    }

    fn freeze(&mut self) {
        let node = self.freeze_work_list.pop().unwrap();

        self.simplify_work_list.push(node);

        self.freeze_moves(node);
    }

    fn freeze_moves(&mut self, node: Register) {
        let node_moves = self.node_moves(node).into_iter().collect::<Vec<_>>();

        for node_move in node_moves {
            let y_alias = self.get_alias(node_move.from);
            let node_alias = self.get_alias(node);
            let v = if y_alias == node_alias {
                self.get_alias(node_move.to)
            } else {
                y_alias
            };

            self.active_moves.remove(&node_move);

            self.frozen_moves.push(node_move);

            if self.freeze_work_list.contains(&v) && self.node_moves(v).is_empty() {
                self.freeze_work_list.remove_item(&v);
                self.simplify_work_list.push(v);
            }
        }
    }

    fn select_spill(&mut self) {
        let register = self.spill_work_list.pop().unwrap(); // to di add a heuristic to get this

        self.simplify_work_list.push(register);

        self.freeze_moves(register);
    }

    fn assign_colors(&mut self, graph: &GraphMap<Register, usize, Undirected>) {
        let mut select_stack = IndexSet::new();

        println!("stack {:?}", self.select_stack);

        std::mem::swap(&mut self.select_stack, &mut select_stack);

        let mut select_stack = select_stack.into_iter().collect::<Vec<_>>();
        while !select_stack.is_empty() {
            let node = select_stack.pop().unwrap();

            self.ok_colors = (0..NUMBER_REGISTER - 1).collect::<Vec<_>>();

            for neighbor in graph.neighbors(node) {
                if self
                    .colored_nodes
                    .union(&self.precolored)
                    .cloned()
                    .collect::<IndexSet<_>>()
                    .contains(&self.get_alias(neighbor))
                {
                    self.ok_colors
                        .remove_item(&self.color[&self.get_alias(neighbor)]);
                }
            }

            if self.ok_colors.is_empty() {
                self.spilled_nodes.insert(node);
            } else {
                if self.next_colour + 1 > self.ok_colors.len() - 1 {
                    self.next_colour = 0;
                }

                self.colored_nodes.insert(node);

                let c = self.ok_colors[self.next_colour];
                self.color.insert(node, c);
                self.next_colour += 1;
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

    fn rewrite_program(&mut self) {
        let mut new_temps = IndexSet::new();

        for v in &self.spilled_nodes {
            for (_, block) in self.function.blocks.iter_mut() {
                let mut before = Vec::new(); //index of stores to place before
                let mut after = Vec::new(); //index of stores to place after

                for (i, instruction) in block.instructions.iter_mut().enumerate() {
                    use crate::instructions::Instruction::*;

                    let new_temp = Register::new();
                    
                    match instruction {
                        Array(ref mut dest, _) => {
                            if dest == v {
                                after.push((i, new_temp));

                                *dest = new_temp;
                            }
                        }

                        Binary(ref mut dest, ref mut lhs, _, ref mut rhs) => {
                            if lhs == v {
                                before.push((i, new_temp));
                                *lhs = new_temp;
                            } else if rhs == v {
                                before.push((i, new_temp));
                                *rhs = new_temp;
                            }

                            if dest == v {
                                after.push((i, new_temp));
                                *dest = new_temp;
                            }
                        }
                        Cast(ref mut dest, ref mut value, _) => {
                            if value == v {
                                before.push((i, new_temp));
                                *value = new_temp;
                            }
                            if dest == v {
                                before.push((i, new_temp));
                                *dest = new_temp;
                            }
                        }

                        Call(ref mut dest, _, ref mut args) => {
                            for arg in args {
                                if arg == v {
                                    before.push((i, new_temp));
                                    *arg = new_temp;
                                }
                            }

                            if dest == v {
                                after.push((i, new_temp));
                                *dest = new_temp;
                            }
                        }

                        StatementStart => (),

                        StoreI(ref mut dest, _) => {
                            if dest == v {
                                before.push((i, new_temp));
                                *dest = new_temp;
                            }
                        }

                        Store(ref mut dest, ref mut value) => {
                            if value == v {
                                before.push((i, new_temp));
                                *value = new_temp;
                            }

                            if dest == v {
                                after.push((i, new_temp));
                                *dest = new_temp;
                            }
                        }

                        Unary(ref mut dest, ref mut value, _) => {
                            if value == v {
                                before.push((i, new_temp));
                                *value = new_temp;
                            }

                            if dest == v {
                                after.push((i, new_temp));
                                *dest = new_temp;
                            }
                        }

                        Return(ref mut value) => {
                            if value == v {
                                before.push((i, new_temp));
                                *value = new_temp
                            }
                        }
                    }
                }

                for (index, temp) in before {
                    block
                        .instructions
                        .insert(index, Instruction::Store(temp, *v));
                }

                for (index, temp) in after {
                    block
                        .instructions
                        .insert(index, Instruction::Store(temp, *v));
                }
            }
        }

        self.spilled_nodes = IndexSet::new();

        self.initial = self
            .colored_nodes
            .union(
                &self
                    .coalesced_nodes
                    .clone()
                    .into_iter()
                    .collect::<IndexSet<_>>()
                    .union(&new_temps)
                    .cloned()
                    .collect::<IndexSet<_>>(),
            )
            .cloned()
            .collect::<IndexSet<_>>();

        self.colored_nodes = IndexSet::new();
        self.coalesced_nodes = Vec::new();
    }
}
