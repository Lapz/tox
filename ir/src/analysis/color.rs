use crate::analysis::AnalysisState;
use crate::instructions::{Function, Instruction, Register, POINTER_WIDTH, STACK_POINTER};
use indexmap::{IndexMap, IndexSet};
use std::cmp::Ordering;

use petgraph::{graphmap::GraphMap, Undirected};

use util::symbol::Symbols;

/// store $to $from
#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct Move {
    from: Register,
    to: Register,
}

static mut ALLOC_COUNTER: usize = 0;
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
    /// the set of interference edges(u,v) in the graph;if (u,v)∈ adj_set,then (v, u) ∈ adj_set.
    pub(crate) adj_set: IndexSet<(Register, Register)>,
    /// adjacency list representation of the graph; for each non-precolored temporary u, adj_list[u] is the set of nodes that interfere with u.
    pub(crate) adj_list: IndexMap<Register, IndexSet<Register>>,
    pub(crate) mappings: IndexMap<Register, Register>,
    offset: usize,
    seen_inst: IndexSet<Instruction>,
    spilled_before: IndexSet<Register>,
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
}

macro_rules! hashmap {
    () => { IndexMap::new() };

    ( $($key:expr => $value:expr),* ) => {{
        let mut l = IndexMap::new();
        $(
            l.insert($key,$value);
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
            precolored: hashset!(STACK_POINTER),
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
            color: hashmap!(STACK_POINTER=>2),
            degree: IndexMap::new(),
            next_colour: 0,
            alias: IndexMap::new(),
            adj_set: IndexSet::new(),
            adj_list: IndexMap::new(),
            seen_inst: hashset!(),
            offset: 0,
            mappings: hashmap!(),
            spilled_before: hashset!(),
        }
    }

    pub fn allocate(&mut self) {
        unsafe {
            ALLOC_COUNTER += 1;
        }

        self.state = AnalysisState::new(self.function);

        self.build_graph();

        // self.check_invariants();

        self.make_work_list();

        #[cfg(feature = "graphviz")]
        self.dump_debug(self.function.name, unsafe { ALLOC_COUNTER });

        while {
            if !self.simplify_work_list.is_empty() {
                self.simpilfy();
            } else if !self.work_list_moves.is_empty() {
                self.colaesce()
            } else if !self.freeze_work_list.is_empty() {
                self.freeze();
            } else if !self.spill_work_list.is_empty() {
                self.select_spill()
            }

            // #[cfg(feature = "graphviz")]
            // self.dump_debug(self.function.name, count);

            !(self.simplify_work_list.is_empty()
                && self.work_list_moves.is_empty()
                && self.freeze_work_list.is_empty()
                && self.spill_work_list.is_empty())
        } {}

        self.assign_colors();

        if !self.spilled_nodes.is_empty() {
            self.rewrite_program();
            self.allocate();
        }

        #[cfg(feature = "graphviz")]
        self.dump_debug(self.function.name, unsafe { ALLOC_COUNTER });
    }

    pub fn build_graph(&mut self) {
        //init all the nodes
        for (_, block) in &self.function.blocks {
            for instruction in block.instructions.iter() {
                for node in instruction.def() {
                    self.adj_list.insert(node, IndexSet::new());
                    self.degree.insert(node, 0);
                }

                for node in instruction.used() {
                    self.adj_list.insert(node, IndexSet::new());
                    self.degree.insert(node, 0);
                }
            }
        }

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
                        if !self.adj_set.contains(&(*live, *def)) && live != def {
                            self.adj_set.insert((*live, *def));
                            self.adj_set.insert((*def, *live));

                            if !self.precolored.contains(live) {
                                self.adj_list.get_mut(live).unwrap().insert(*def);

                                *self.degree.get_mut(live).unwrap() += 1;
                            }

                            if !self.precolored.contains(def) {
                                self.adj_list.get_mut(def).unwrap().insert(*live);
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
                    .collect::<IndexSet<_>>();
            }
        }
    }

    // helper functions
    /// Add an edge between two register if they are not the same and edge between them is not already present in the graph
    fn add_edge(&mut self, u: Register, v: Register) {
        if self.adj_set.contains(&(u, v)) && u != v {
            self.adj_set.insert((u, v));
            self.adj_set.insert((v, u));

            if !self.precolored.contains(&u) {
                self.adj_list[&u].insert(v);
                self.degree[&u] += 1;
            }

            if !self.precolored.contains(&v) {
                self.adj_list[&v].insert(u);
                self.degree[&v] += 1;
            }
        }
    }

    fn make_work_list(&mut self) {
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

    /// find all the nodes that are adjacent to the selected one in the graph.
    fn adjacent(&self, n: Register) -> IndexSet<Register> {
        // if self.adj_list.get(&n).is_none() {
        //     union
        // } else {
        self.adj_list[&n]
            .difference(
                &self
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
                    .collect::<IndexSet<_>>(),
            )
            .cloned()
            .collect::<IndexSet<_>>()
        // }
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

    fn check_invariants(&self) {
        let mut all_work_list = Vec::new();

        all_work_list.extend(self.simplify_work_list.clone());
        all_work_list.extend(self.freeze_work_list.clone());
        all_work_list.extend(self.spill_work_list.clone());

        for node in all_work_list {
            assert!(self.degree_invariant(node), "degree_invariant check failed")
        }

        for node in &self.simplify_work_list {
            assert!(
                self.simplify_work_list_invariant(node),
                "Simplify worklist invariant failed"
            )
        }

        for node in &self.freeze_work_list {
            assert!(
                self.freeze_work_list_invariant(node),
                "Freeze worklist invariant check failed"
            )
        }

        for node in &self.spill_work_list {
            assert!(
                self.spill_work_list_invariant(node),
                "Spill worklist invariant check failed"
            )
        }
    }

    fn degree_invariant(&self, node: Register) -> bool {
        // degree(u) = |adj_list(u)∩ (precolored ∪ simplifyWorklist ∪ freezeWorklist ∪ spillWorklist)
        let mut all_work_list = Vec::new();

        all_work_list.extend(self.simplify_work_list.clone());
        all_work_list.extend(self.freeze_work_list.clone());
        all_work_list.extend(self.spill_work_list.clone());

        self.degree[&node]
            == self.adj_list[&node]
                .intersection(
                    &self
                        .precolored
                        .union(&all_work_list.into_iter().collect::<IndexSet<_>>())
                        .cloned()
                        .collect::<IndexSet<_>>(),
                )
                .cloned()
                .collect::<IndexSet<_>>()
                .len()
    }

    fn simplify_work_list_invariant(&self, node: &Register) -> bool {
        // (u ∈ freezeWorklist) ⇒ moveList[u] ∩ (activeMoves ∪ worklistMoves) ̸= {}
        self.degree[node] < NUMBER_REGISTER
            && self.move_list[node]
                .intersection(
                    &self
                        .active_moves
                        .union(&self.work_list_moves)
                        .cloned()
                        .collect::<IndexSet<_>>(),
                )
                .cloned()
                .collect::<IndexSet<_>>()
                .is_empty()
    }

    fn freeze_work_list_invariant(&self, node: &Register) -> bool {
        // (u ∈ freezeWorklist) ⇒ moveList[u] ∩ (activeMoves ∪ worklistMoves) ̸= {}
        self.degree[node] < NUMBER_REGISTER
            && !self.move_list[node]
                .intersection(
                    &self
                        .active_moves
                        .union(&self.work_list_moves)
                        .cloned()
                        .collect::<IndexSet<_>>(),
                )
                .cloned()
                .collect::<IndexSet<_>>()
                .is_empty()
    }

    fn spill_work_list_invariant(&self, node: &Register) -> bool {
        // (u ∈ spillWorklist) ⇒ degree(u) ≥ K
        self.degree[node] >= NUMBER_REGISTER
    }

    fn move_related(&self, n: Register) -> bool {
        !self.node_moves(n).is_empty()
    }

    fn simpilfy(&mut self) {
        let node = self.simplify_work_list.pop().unwrap();

        println!("simplifying node {:?}", node);

        self.select_stack.insert(node);

        let adjacent = self.adjacent(node);

        // assert!(self.degree[&node] ==adjacent.len(),"degree {} node {}\n adj_list {:?}, adjacent {:?}",self.degree[&node],node,self.adj_list[&node],adjacent);

        for neighbour in adjacent {
            self.decrement_degree(neighbour);
        }
    }

    fn decrement_degree(&mut self, node: Register) {
        let mut degree = self.degree[&node];

        println!("decreasing edge for node {} ", node);

        if degree != 0 {
            *self.degree.get_mut(&node).unwrap() -= 1;
            degree -= 1;
        }

        if degree == NUMBER_REGISTER {
            let mut nodes = self.adjacent(node);

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

    fn add_work_list(&mut self, node: Register) {
        if !self.precolored.contains(&node)
            && !(self.move_related(node) && self.degree[&node] < NUMBER_REGISTER)
        {
            self.freeze_work_list.remove_item(&node);
            self.simplify_work_list.push(node);
        }
    }

    fn spill_cost(&self, node: &Register) -> f64 {
        let degree = self.degree[node] as f64;

        let mut uses = 0.0;
        let mut defs = 0.0;

        for (id, _) in &self.function.blocks {
            if self.state.live_in[id].contains(node) {
                uses += 1.0;
            }
            if self.state.live_out[id].contains(node) {
                defs += 1.0;
            }
        }

        (uses + defs) / degree
    }

    ///implements the heuristic used for coalescing a precolored register
    //think of a better name
    fn ok(&self, t: Register, r: Register) -> bool {
        self.degree[&t] < NUMBER_REGISTER
            || self.precolored.contains(&t)
            || self.adj_set.contains(&(t, r))
    }

    /// Conservative implements the conservative coalescing heuristic.
    fn conservative(&mut self, nodes: Vec<Register>) -> bool {
        let mut k = 0;

        for node in nodes {
            if self.degree[&node] >= NUMBER_REGISTER {
                k += 1;
            }
        }

        k < NUMBER_REGISTER
    }

    fn colaesce(&mut self) {
        println!("colaesce called");

        let work_move = self.work_list_moves.pop().unwrap();

        let x = self.get_alias(work_move.to);

        let y = self.get_alias(work_move.from);

        let (u, v) = if self.precolored.contains(&y) {
            (y, x)
        } else {
            (x, y)
        };

        self.work_list_moves.remove(&work_move);

        if u == v {
            self.coalesced_moves.insert(work_move);
            self.add_work_list(u);
        } else if self.precolored.contains(&v) || self.adj_set.contains(&(u, v)) {
            self.constrained_moves.insert(work_move);

            self.add_work_list(u);
            self.add_work_list(v);
        }
        // u ∈ precolored ∧ (∀t ∈ Adjacent(v), OK(t, u)) ∨ u ̸∈ precolored ∧ Conservative(Adjacent(u) ∪ Adjacent(v))
        else if self.precolored.contains(&u) && {
            let mut result = true;

            for t in self.adjacent(v) {
                result = self.ok(t, u);
            }

            result
        } || !self.precolored.contains(&u)
            && self.conservative(
                self.adjacent(u)
                    .union(&self.adjacent(v))
                    .cloned()
                    .collect::<Vec<_>>(),
            )
        {
            self.coalesced_moves.insert(work_move);
            self.combine(u, v);
            self.add_work_list(u);
        } else {
            self.active_moves.insert(work_move);
        }
    }

    fn combine(&mut self, u: Register, v: Register) {
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

        for t in self.adjacent(v) {
            self.add_edge(t, u);
            self.decrement_degree(t);
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
        println!("freeze called");
        let node = self.freeze_work_list.pop().unwrap();

        self.simplify_work_list.push(node);

        self.freeze_moves(node);
    }

    fn freeze_moves(&mut self, node: Register) {
        let node_moves = self.node_moves(node).into_iter().collect::<Vec<_>>();

        for node_move in node_moves {
            let v = if self.get_alias(node_move.from) == self.get_alias(node) {
                self.get_alias(node_move.to)
            } else {
                self.get_alias(node_move.from)
            };

            self.active_moves.remove(&node_move);

            self.frozen_moves.push(node_move);

            if self.node_moves(v).is_empty() && self.degree[&v] < NUMBER_REGISTER {
                self.freeze_work_list.remove_item(&v);
                self.simplify_work_list.push(v);
            }
        }
    }

    fn select_spill(&mut self) {
        let mut costs = hashmap!();

        for node in &self.spill_work_list {
            costs.insert(*node, self.spill_cost(node));
        }

        self.spill_work_list
            .sort_by(|a, b| costs[a].partial_cmp(&costs[b]).unwrap_or(Ordering::Less));

        let register = self.spill_work_list.remove(0); // to di add a heuristic to get this

        self.simplify_work_list.push(register);

        self.freeze_moves(register);
    }

    fn assign_colors(&mut self) {
        println!("select stack {:?}", self.select_stack);

        while !self.select_stack.is_empty() {
            let node = self.select_stack.pop().unwrap();

            self.ok_colors = (0..NUMBER_REGISTER - 1).collect::<Vec<_>>();

            for neighbor in &self.adj_list[&node] {
                if self
                    .colored_nodes
                    .union(&self.precolored)
                    .cloned()
                    .collect::<IndexSet<_>>()
                    .contains(&self.get_alias(*neighbor))
                {
                    self.ok_colors
                        .remove_item(&self.color[&self.get_alias(*neighbor)]);
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
        }

        for node in &self.coalesced_nodes {
            let alias = self.get_alias(*node);

            let c = self.color.get(&alias);

            match c {
                Some(alias_colour) => {
                    self.color.insert(*node, *alias_colour);
                }
                None => (),
            }
        }
    }

    fn rewrite_program(&mut self) {
        use crate::instructions::{Instruction::*, Value, POINTER_WIDTH};

        let mut new_temps: IndexSet<Register> = IndexSet::new();

        //mapping of old to new
        let mut old_to_new = IndexMap::new();

        for spilled_node in &self.spilled_nodes {
            //allocate memory locations for each spilled_node.
            let stack_loc = *self
                .mappings
                .entry(*spilled_node)
                .or_insert(STACK_POINTER.offset_at(self.offset));
            self.offset += POINTER_WIDTH;

            println!(
                "spilled reg {} is stored on the stack at {}",
                spilled_node, stack_loc
            );
        }

        println!("{:#?}", self.function.blocks);

        for (_, block) in self.function.blocks.iter_mut() {
            let mut before = IndexSet::new(); //index of stores to place before
            let mut after = IndexSet::new(); //index of stores to place after
            for (i, instruction) in block.instructions.iter_mut().enumerate() {
                let defs = instruction.def();
                let uses = instruction.used();

                {
                    for used in uses {
                        if self.spilled_nodes.contains(&used)
                            && !self.spilled_before.contains(&used)
                        {
                            let new_temp = *old_to_new.entry(used).or_insert(Register::new());
                            new_temps.insert(new_temp);
                            old_to_new.insert(used, new_temp);

                            println!(
                                "reg {} used in {} is replaced as {}",
                                used, instruction, new_temp
                            );

                            before.insert((i, Instruction::Store(new_temp, used)));
                            instruction.rewrite_uses(used, new_temp);

                            // self.spilled_before.insert(used);
                        }
                    }
                }
                {
                    for def in defs {
                        if self.spilled_nodes.contains(&def) && !self.spilled_before.contains(&def)
                        {
                            let new_temp = *old_to_new.entry(def).or_insert(Register::new());
                            new_temps.insert(new_temp);
                            old_to_new.insert(def, new_temp);

                            println!(
                                "reg {} used in {} is replaced as {}",
                                def, instruction, new_temp
                            );

                            after
                                .insert((i + 1, Instruction::Store(def, new_temp)));
                            instruction.rewrite_def(new_temp);
                            // self.spilled_before.insert(def);
                        }
                    }
                }
            }

            let offset = before.len();

            for (i, (index, instruction)) in before.into_iter().enumerate() {
                block.instructions.insert(index + i, instruction);
            }

            for (i, (index, instruction)) in after.into_iter().enumerate() {
                block.instructions.insert(index + i + offset, instruction);
            }
        }

        self.spilled_nodes.clear();

        self.initial.extend(self.colored_nodes.clone());
        self.initial.extend(new_temps);

        self.colored_nodes = IndexSet::new();
        self.coalesced_nodes = Vec::new();

        println!("{:#?}", self.function.blocks);
        // panic!()
    }
}
