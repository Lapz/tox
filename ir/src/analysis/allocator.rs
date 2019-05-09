use crate::analysis::AnalysisState;
use crate::instructions::{BlockEnd, BlockID, Function, Instruction, Register};
#[cfg(feature = "graphviz")]
use petgraph::dot::{Config, Dot};
use petgraph::{graph::NodeIndex, stable_graph::StableGraph, Directed, Graph, Undirected};
use std::collections::{hash_map::Entry, HashMap, HashSet};
#[cfg(feature = "graphviz")]
use std::{
    fs::{self, File},
    io::{self, Write},
    process::Command,
};
use util::symbol::Symbols;

const REG_COUNT: usize = 3;

#[derive(Debug)]
pub struct Allocator<'a> {
    state: AnalysisState,
    move_list: HashMap<NodeIndex, HashSet<NodeIndex>>,
    work_list_moves: HashSet<NodeIndex>,
    freeze_worklist: HashSet<NodeIndex>,
    spill_worklist: HashSet<NodeIndex>,
    work_list: HashSet<NodeIndex>,
    initial_list: HashSet<NodeIndex>, // register all ready present
    simplify_work_list: HashSet<NodeIndex>,
    active_moves: HashSet<NodeIndex>,
    spilled_nodes: HashSet<NodeIndex>,
    select_stack: Vec<NodeIndex>,
    pre_colored: HashSet<NodeIndex>,
    coalesced_moves: HashSet<NodeIndex>,
    constrained_moves: HashSet<NodeIndex>,
    coalesced_nodes: HashSet<NodeIndex>,
    symbols: &'a mut Symbols<()>,
    alias: HashMap<NodeIndex, NodeIndex>,
    ok_colors: HashSet<usize>,
    colored_nodes: HashSet<NodeIndex>,
    color: HashMap<NodeIndex, usize>,
    frozen_moves: HashSet<NodeIndex>,
}

impl<'a> Allocator<'a> {
    pub fn new(symbols: &'a mut Symbols<()>, function: &Function) -> Self {
        let mut function_state = AnalysisState::new();
        function_state.init(&function);
        function_state.calculate_successors(function);
        function_state.calulate_live_out(function);

        let mut colors = HashSet::new(); //red // green // blue
        colors.insert(0);
        colors.insert(1);
        colors.insert(2);

        Self {
            state: function_state,
            move_list: HashMap::new(),
            work_list_moves: HashSet::new(),
            freeze_worklist: HashSet::new(),
            spill_worklist: HashSet::new(),
            work_list: HashSet::new(),
            initial_list: HashSet::new(),
            simplify_work_list: HashSet::new(),
            active_moves: HashSet::new(),
            spilled_nodes: HashSet::new(),
            select_stack: Vec::new(),
            pre_colored: HashSet::new(),
            coalesced_moves: HashSet::new(),
            constrained_moves: HashSet::new(),
            coalesced_nodes: HashSet::new(),
            alias: HashMap::new(),
            frozen_moves: HashSet::new(),
            ok_colors: colors,
            colored_nodes: HashSet::new(),
            color: HashMap::new(),
            symbols,
        }
    }
    pub fn build_interference_graphs(
        &mut self,
        function: &Function,
    ) -> Graph<Register, usize, Undirected> {
        let mut graph = Graph::new_undirected();
        // let mut graph = StableGraph::with_capacity(10,10);
        let mut mappings = HashMap::new();

        #[cfg(feature = "prettytable")]
        let mut data: Vec<Vec<String>> = Vec::new();
        #[cfg(feature = "prettytable")]
        {
            data.push(vec!["register".into(), "live_range".into()]);
        }
        for (id, block) in &function.blocks {
            let mut live = self.state.live_out[id].clone();

            for inst in block.instructions.iter().rev() {
                let used = inst.used();
                let def = inst.def();
                if inst.is_move() {
                    live = live
                        .difference(&used)
                        .cloned()
                        .collect::<HashSet<Register>>();

                    for n in def.union(&used) {
                        let node = if let Some(entry) = mappings.get(n) {
                            *entry
                        } else {
                            let node = graph.add_node(*n);

                            mappings.insert(*n, node);
                            node
                        };
                        let list = self.move_list.entry(node).or_insert(HashSet::new());
                        list.insert(node);

                        self.work_list_moves.insert(node);
                    }
                }

                live = live.union(&def).cloned().collect::<HashSet<Register>>();

                for d in &def {
                    let start_node = if let Some(entry) = mappings.get(d) {
                        *entry
                    } else {
                        let node = graph.add_node(*d);

                        mappings.insert(*d, node);
                        node
                    };
                    for l in &live {
                        let end_node = if let Some(entry) = mappings.get(l) {
                            *entry
                        } else {
                            let node = graph.add_node(*l);
                            mappings.insert(*l, node);
                            node
                        };

                        self.add_edge(end_node, start_node, &mut graph)
                    }
                }

                live = used
                    .union(&live.difference(&def).cloned().collect::<HashSet<_>>())
                    .cloned()
                    .collect::<HashSet<_>>()
            }
        }

        #[cfg(feature = "graphviz")]
        {
            let name = self.symbols.name(function.name);

            fs::create_dir(&format!("graphviz/{}", name));
            let file_name = format!("graphviz/{}/{}_initial_reg.dot", name, name);

            File::create(&file_name)
                .unwrap()
                .write(
                    Dot::with_config(&graph, &[Config::EdgeNoLabel])
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
                File::create(format!("graphviz/{}/{}_initial_reg.png", name, name)).unwrap();
            file.write(&output).unwrap();
        }

        graph
    }

    fn make_work_list(&mut self, graph: &mut Graph<Register, usize, Undirected>) {
        let mut initial_list = HashSet::new();

        std::mem::swap(&mut self.initial_list, &mut initial_list);
        for index in self.initial_list.iter() {
            if graph.edges(*index).count() >= REG_COUNT {
                self.spill_worklist.insert(*index);
            } else if self.move_related(*index) {
                self.freeze_worklist.insert(*index);
            } else {
                self.simplify_work_list.insert(*index);
            }
        }
    }

    fn move_related(&self, n: NodeIndex) -> bool {
        self.node_moves(n).is_empty()
    }

    fn node_moves(&self, n: NodeIndex) -> HashSet<NodeIndex> {
        if self.move_list.contains_key(&n) {
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

    fn simplify(&mut self, graph: &mut Graph<Register, usize, Undirected>) {
        if self.simplify_work_list.is_empty() {
            return;
        } else {
            let mut simplify_work_list = HashSet::new();

            std::mem::swap(&mut self.simplify_work_list, &mut simplify_work_list);

            let mut simplify_work_list = simplify_work_list.into_iter().collect::<Vec<_>>();

            let node = simplify_work_list.remove(0);

            self.select_stack.push(node);

            self.decrement_degree(node, graph);

            let mut simplify_work_list = simplify_work_list.into_iter().collect::<HashSet<_>>();

            std::mem::swap(&mut simplify_work_list, &mut self.simplify_work_list);
        }
    }

    fn decrement_degree(
        &mut self,
        index: NodeIndex,
        graph: &mut Graph<Register, usize, Undirected>,
    ) {
        let degrees = graph.edges(index).count();

        graph.remove_node(index);

        let mut nodes = vec![index];

        let mut edges_iter = graph.neighbors(index).detach();

        while let Some(node) = edges_iter.next_node(graph) {
            nodes.push(node)
        }

        if degrees as isize - 1 == REG_COUNT as isize {
            self.enable_moves(nodes);
            self.spill_worklist.remove(&index);
            if self.move_related(index) {
                self.freeze_worklist.insert(index);
            } else {
                self.simplify_work_list.insert(index);
            }
        }
    }

    fn enable_moves(&mut self, nodes: Vec<NodeIndex>) {
        for node in nodes {
            for m in self.node_moves(node) {
                if self.active_moves.contains(&m) {
                    self.active_moves.remove(&m);
                    self.work_list_moves.insert(m);
                }
            }
        }
    }

    fn add_work_list(
        &mut self,
        node: NodeIndex,
        graph: &mut Graph<Register, usize, Undirected>,
    ) {
        if !self.pre_colored.contains(&node)
            && !(self.move_related(node) && graph.edges(node).count() < REG_COUNT)
        {
            self.freeze_worklist.remove(&node);
            self.simplify_work_list.insert(node);
        }
    }

    fn add_edge(
        &mut self,
        u: NodeIndex,
        v: NodeIndex,
        graph: &mut Graph<Register, usize, Undirected>,
    ) {
        if graph.find_edge(u, v).is_none() && u != v {
            if !self.pre_colored.contains(&u) && !self.pre_colored.contains(&v) {
                graph.add_edge(u, v, 1);
            }
        }
    }

    ///implements the heuristic used for coalescing a precolored register
    //think of a better name
    fn ok(
        &self,
        t: NodeIndex,
        r: NodeIndex,
        graph: &Graph<Register, usize, Undirected>,
    ) -> bool {
        let neighbors_t = graph.neighbors(t).collect::<HashSet<_>>();
        let adj_set = graph
            .neighbors(r)
            .collect::<HashSet<_>>()
            .union(&neighbors_t)
            .cloned()
            .collect::<HashSet<_>>();
        graph.edges(t).count() < REG_COUNT
            || self.pre_colored.contains(&t)
            || adj_set.contains(&t)
            || adj_set.contains(&r)
    }

    /// Conservative implements the conservative coalescing heuristic.
    fn conservative(
        &mut self,
        nodes: Vec<NodeIndex>,
        graph: &mut Graph<Register, usize, Undirected>,
    ) -> bool {
        let mut k = 0;

        for node in nodes {
            if graph.edges(node).count() >= REG_COUNT {
                k += 1;
            }
        }

        k < REG_COUNT
    }

    fn adjacent(
        &mut self,
        node: NodeIndex,
        graph: &Graph<Register, usize, Undirected>,
    ) -> HashSet<NodeIndex> {
        let select_stack = self
            .select_stack
            .clone()
            .into_iter()
            .collect::<HashSet<_>>();
        let union = select_stack
            .union(&self.coalesced_nodes)
            .cloned()
            .collect::<HashSet<_>>();

        graph
            .neighbors(node)
            .collect::<HashSet<_>>()
            .difference(&union)
            .cloned()
            .collect::<HashSet<_>>()
    }

    fn combine(
        &mut self,
        u: NodeIndex,
        v: NodeIndex,
        graph: &mut Graph<Register, usize, Undirected>,
    ) {
        if self.freeze_worklist.contains(&v) {
            self.freeze_worklist.remove(&v);
        } else {
            self.spill_worklist.remove(&v);
        }
        self.coalesced_nodes.insert(v);

        self.alias.insert(v, u);

        self.move_list.entry(u).or_insert(HashSet::new());
        self.move_list.entry(v).or_insert(HashSet::new());

        let combined = self.move_list[&u]
            .union(&self.move_list[&v])
            .cloned()
            .collect::<HashSet<_>>();

        *self.move_list.get_mut(&u).unwrap() = combined;

        self.enable_moves(vec![v]);

        let adjacent = self.adjacent(v, graph);

        for t in adjacent {
            self.add_edge(t, u, graph);
            self.decrement_degree(t, graph);
        }

        if graph.edges(u).count() >= REG_COUNT && self.freeze_worklist.contains(&u) {
            self.freeze_worklist.remove(&u);
            self.spill_worklist.insert(u);
        }
    }

    fn coalesce(
        &mut self,
        function: &Function,
        graph: &mut Graph<Register, usize, Undirected>,
    ) {
        let mut work_list_moves = HashSet::new();

        std::mem::swap(&mut self.work_list_moves, &mut work_list_moves);

        let mut work_list_moves = work_list_moves.into_iter().collect::<Vec<_>>();
        let node = work_list_moves.remove(0);

        let x = self.get_alias(node.clone());
        let y = self.get_alias(node.clone());

        let (mut u, mut v) = (x, y);

        if self.pre_colored.contains(&x) {
            u = y;
            v = x;
        } else {
            u = x;
            v = y;
        }

        self.work_list_moves.remove(&node);

        let mut work_list_moves = work_list_moves.into_iter().collect::<HashSet<_>>();

        std::mem::swap(&mut work_list_moves, &mut self.work_list_moves);
        if u == v {
            self.coalesced_moves.insert(node);
            self.add_work_list(u, graph);
        } else if self.pre_colored.contains(&v) || is_adjcent(u, v, graph) {
            self.constrained_moves.insert(node);
            self.add_work_list(u, graph);
            self.add_work_list(v, graph)
        } else if self.pre_colored.contains(&u) && {
            let mut result = false;

            for t in graph.neighbors(v) {
                result = self.ok(t, u, graph);
            }
            result
        } || !self.pre_colored.contains(&u) && {
            let union = self
                .adjacent(u, graph)
                .union(&self.adjacent(v, graph))
                .cloned()
                .collect::<Vec<_>>();

            self.conservative(union, graph)
        } {
            self.coalesced_moves.insert(node);
            self.combine(u, v, graph);
            self.add_work_list(u, graph);
        } else {
            self.active_moves.insert(node);
        }
    }

    fn get_alias(&self, node: NodeIndex) -> NodeIndex {
        if self.coalesced_nodes.contains(&node) {
            self.get_alias(self.alias[&node])
        } else {
            node
        }
    }

    fn freeze(&mut self) {
        let mut freeze_work_list = HashSet::new();

        std::mem::swap(&mut self.freeze_worklist, &mut freeze_work_list);

        let mut freeze_work_list = freeze_work_list.into_iter().collect::<Vec<_>>();
        let node = freeze_work_list.remove(0);

        self.simplify_work_list.insert(node);

        let mut freeze_work_list = freeze_work_list.into_iter().collect::<HashSet<_>>();

        std::mem::swap(&mut freeze_work_list, &mut self.freeze_worklist);

        self.freeze_moves(node)
    }

    fn freeze_moves(&mut self, node: NodeIndex) {
        for node in self.node_moves(node) {
            let x = node.clone();
            let y = node.clone();
            let mut v = node.clone();

            if self.get_alias(y) == self.get_alias(node) {
                v = self.get_alias(x);
            } else {
                v = self.get_alias(y);
            }

            self.active_moves.remove(&node);

            self.frozen_moves.insert(node);

            if self.freeze_worklist.contains(&v) && self.node_moves(v).is_empty() {
                self.freeze_worklist.remove(&v);
                self.simplify_work_list.insert(v);
            }
        }
    }

    fn select_spill(&mut self) {
        let mut spill_work_list = HashSet::new();

        std::mem::swap(&mut self.spill_worklist, &mut spill_work_list);

        let mut spill_work_list = spill_work_list.into_iter().collect::<Vec<_>>();
        let node = spill_work_list.remove(0);

        self.spill_worklist.remove(&node);

        self.simplify_work_list.insert(node);

        let mut spill_work_list = spill_work_list.into_iter().collect::<HashSet<_>>();

        std::mem::swap(&mut spill_work_list, &mut self.spill_worklist);

        self.freeze_moves(node)
    }

    fn assign_colors(&mut self, graph: &Graph<Register, usize, Undirected>) {
        while !self.select_stack.is_empty() {
            let n = self.select_stack.remove(0);

            for node in graph.neighbors(n) {
                let union = self
                    .colored_nodes
                    .union(&self.pre_colored)
                    .cloned()
                    .collect::<HashSet<_>>();
                if union.contains(&self.get_alias(node)) {
                    self.ok_colors.remove(&self.color[&self.get_alias(node)]);
                }
            }

            if self.ok_colors.is_empty() {
                self.spilled_nodes.insert(n);
            } else {
                self.colored_nodes.insert(n);

                let mut ok_colors = HashSet::new();

                std::mem::swap(&mut self.ok_colors, &mut ok_colors);

                let mut ok_colors = ok_colors.into_iter().collect::<Vec<_>>();

                let color = ok_colors.remove(0);

                let mut ok_colors = ok_colors.into_iter().collect::<HashSet<_>>();

                std::mem::swap(&mut ok_colors, &mut self.ok_colors);

                self.color.insert(n, color);
            }
        }

        for node in &self.colored_nodes {
            *self.color.get_mut(node).unwrap() = self.color[&self.get_alias(*node)];
        }
    }

    pub fn allocate(&mut self, count: usize, function: &mut Function) {
        let mut graph = self.build_interference_graphs(function);
        #[cfg(feature = "graphviz")]
        {
            let name = self.symbols.name(function.name);

            fs::create_dir(&format!("graphviz/{}", name));
            let file_name = format!("graphviz/{}/{}_reg_{}.dot", name, name, count);

            File::create(&file_name)
                .unwrap()
                .write(
                    Dot::with_config(&graph, &[Config::EdgeNoLabel])
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
                File::create(format!("graphviz/{}/{}_reg_{}.png", name, name, count)).unwrap();
            file.write(&output).unwrap();
        }
        self.make_work_list(&mut graph);

        while {
            if !self.simplify_work_list.is_empty() {
                self.simplify(&mut graph);
            } else if !self.work_list_moves.is_empty() {
                self.coalesce(function, &mut graph);
            } else if !self.freeze_worklist.is_empty() {
                self.freeze()
            } else if !self.spill_worklist.is_empty() {
                self.select_spill();
            }

            !(self.simplify_work_list.is_empty()
                && self.work_list_moves.is_empty()
                && self.freeze_worklist.is_empty()
                && self.spill_worklist.is_empty())
        } {}

        self.assign_colors(&graph);

        if !self.spilled_nodes.is_empty() {
            self.rewrite_program(&mut graph, function);
            self.allocate(count + 1, function)
        }
    }

    fn rewrite_program(
        &mut self,
        graph: &mut Graph<Register, usize, Undirected>,
        function: &mut Function,
    ) {
        let mut new_temps = HashSet::new();
        for v in &self.spilled_nodes {
            let v = match graph.node_weight(*v) {
                Some(v) => v,
                None => continue,
            };

            for (_, block) in function.blocks.iter_mut() {
                let mut before = Vec::new(); //index of stores to place before
                let mut after = Vec::new(); //index of stores to place after

                for (i, instruction) in block.instructions.iter().enumerate() {
                    use crate::instructions::Instruction::*;
                    match instruction {
                        Array(ref dest, _) => {
                            if dest == v {
                                after.push(i);
                            }
                        }

                        Binary(ref dest, ref lhs, _, ref rhs) => {
                            if lhs == v {
                                before.push(i);
                            } else if rhs == v {
                                before.push(i);
                            }

                            if dest == v {
                                after.push(i);
                            }
                        }
                        Cast(ref dest, ref value, _) => {
                            if value == v {
                                before.push(i);;
                            }
                            if dest == v {
                                after.push(i);
                            }
                        }

                        Call(ref dest, _, ref args) => {
                            for arg in args {
                                if arg == v {
                                    before.push(i);
                                }
                            }

                            if dest == v {
                                after.push(i);
                            }
                        }

                        StatementStart => (),

                        StoreI(ref dest, _) => {
                            if dest == v {
                                after.push(i);
                            }
                        }

                        Store(ref dest, ref val) => {
                            if val == v {
                                before.push(i);;
                            }

                            if dest == v {
                                after.push(i);
                            }
                        }

                        Unary(ref dest, ref val, _) => {
                            if val == v {
                                before.push(i);
                            }

                            if dest == v {
                                after.push(i);
                            }
                        }

                        Return(ref val) => {
                            if val == v {
                                after.push(i);
                            }
                        }
                    }
                }

                for index in before {
                    let new_temp = Register::new();
                    block
                        .instructions
                        .insert(index, Instruction::Store(new_temp, *v));

                    new_temps.insert(new_temp);
                }

                for index in after {
                    let new_temp = Register::new();
                    block
                        .instructions
                        .insert(index + 1, Instruction::Store(new_temp, *v));

                    new_temps.insert(new_temp);
                }
            }
        }

        self.spilled_nodes = HashSet::new();

        self.initial_list = self
            .colored_nodes
            .union(
                &self
                    .coalesced_nodes
                    .union(&self.coalesced_nodes)
                    .cloned()
                    .collect::<HashSet<_>>(),
            )
            .cloned()
            .collect::<HashSet<_>>();

        self.colored_nodes = HashSet::new();
        self.coalesced_nodes = HashSet::new();
    }
}

fn is_adjcent(
    t: NodeIndex,
    r: NodeIndex,
    graph: &Graph<Register, usize, Undirected>,
) -> bool {
    let neighbors_t = graph.neighbors(t).collect::<HashSet<_>>();
    let adj_set = graph
        .neighbors(r)
        .collect::<HashSet<_>>()
        .union(&neighbors_t)
        .cloned()
        .collect::<HashSet<_>>();

    adj_set.contains(&t) && adj_set.contains(&r)
}
