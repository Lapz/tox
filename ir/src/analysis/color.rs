use crate::analysis::AnalysisState;
use crate::instructions::{BlockEnd, BlockID, Function, Instruction, Register, STACK_POINTER};
use cfg_if::cfg_if;
use indexmap::{indexmap, indexset, IndexMap, IndexSet};
use std::cmp::Ordering;
use util::symbol::Symbols;

type Node = Register;

static mut ALLOC_COUNTER: usize = 0;

/// store $to $from
#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct Move {
    from: Register,
    to: Register,
}

pub enum Colour {
    Red,
    Blue,
    Green,
}
/// The max number of registers available
const K: usize = 3;
#[derive(Debug)]
pub struct Allocator<'a> {
    /// machine registers,preassigned a color
    pre_colored: IndexSet<Register>,
    /// temporary register,not precolored and not yet processed
    initial: IndexSet<Register>,
    /// list of low-degree non-move related nodes.
    simplify_work_list: IndexSet<Node>,
    ///low-degree move-related nodes
    freeze_work_list: IndexSet<Node>,
    /// nodes with a high degree
    spill_work_list: IndexSet<Node>,
    /// nodes makred for spilling during this round
    pub(crate) spilled_nodes: IndexSet<Node>,
    /// nodes makred for spilling during this round
    coalesced_nodes: IndexSet<Node>,
    /// nodes succesfully colored
    colored_nodes: IndexSet<Node>,
    /// stack containing temporarires removed from the graph
    select_stack: Vec<Register>,
    state: AnalysisState,
    function: &'a mut Function,
    /// moves enabled for possible coalescing
    move_list: IndexMap<Register, IndexSet<Move>>,
    /// moves that have been coalesced
    coalesced_moves: IndexSet<Move>,
    /// moves whose source and target interfere
    constrained_moves: IndexSet<Move>,
    /// moves that will no longer be considered for coalescing
    frozen_moves: IndexSet<Move>,
    /// moves enabled for possible coalescing
    work_list_moves: IndexSet<Move>,
    /// moves not yet ready for coalescing
    active_moves: IndexSet<Move>,
    /// A set of edges between nodes
    pub(crate) adj_set: IndexSet<(Node, Node)>,
    /// The neighbours for each node
    pub(crate) adj_list: IndexMap<Node, IndexSet<Node>>,
    /// the degree of each node
    degree: IndexMap<Node, usize>,
    ///when a move (u,v) has been coalesced, and v put in coalescedNodes,then alias(v) = u.
    alias: IndexMap<Register, Register>,
    pub(crate) color: IndexMap<Register, usize>,
    next_colour: usize,
    ok_colors: IndexSet<usize>,
    offset: usize,
    pub(crate) offsets: IndexMap<Register, usize>,
    pub(crate) symbols: &'a mut Symbols<()>,
    pub(crate) old_to_new: IndexMap<Register, Register>,
    spilled_scratch_temps: IndexSet<Register>,
    /// A mapping of spilled node and the mem location on where the spilled
    /// node is placed
    pub(crate) spill_mem_location: IndexMap<Register, Register>,
    pub name_counter: IndexMap<Register, usize>,
}

impl<'a> Allocator<'a> {
    pub fn new(symbols: &'a mut Symbols<()>, function: &'a mut Function) -> Self {
        let mut allocator = Self {
            function,
            symbols,
            state: AnalysisState::empty(),
            pre_colored: IndexSet::new(),
            initial: IndexSet::new(),
            simplify_work_list: IndexSet::new(),
            freeze_work_list: IndexSet::new(),
            spill_work_list: IndexSet::new(),
            spilled_nodes: IndexSet::new(),
            coalesced_nodes: IndexSet::new(),
            colored_nodes: IndexSet::new(),
            select_stack: Vec::new(),
            move_list: IndexMap::new(),
            work_list_moves: IndexSet::new(),
            adj_list: IndexMap::new(),
            adj_set: IndexSet::new(),
            degree: IndexMap::new(),
            coalesced_moves: IndexSet::new(),
            constrained_moves: IndexSet::new(),
            frozen_moves: IndexSet::new(),
            active_moves: IndexSet::new(),
            alias: IndexMap::new(),
            color: IndexMap::new(),
            next_colour: 0,
            ok_colors: IndexSet::new(),
            offsets: IndexMap::new(),
            offset: 0,
            old_to_new: IndexMap::new(),
            spilled_scratch_temps: IndexSet::new(),
            spill_mem_location: IndexMap::new(),
            name_counter: IndexMap::new(),
        };

        allocator.init_initial();

        allocator
    }

    pub fn allocate(&mut self) {
        unsafe {
            ALLOC_COUNTER += 1;
        }

        self.liveness_analysis();

        self.build();

        println!("{:?}",self.degree);


        unsafe {
             if ALLOC_COUNTER == 2 {
                 panic!();
             }
        }

        self.dump_debug(self.function.name, unsafe { ALLOC_COUNTER });

        self.make_work_list();

        while {
            if !self.simplify_work_list.is_empty() {
                self.simplify();
            } else if !self.work_list_moves.is_empty() {
                self.coalesce()
            } else if !self.freeze_work_list.is_empty() {
                self.freeze();
            } else if !self.spill_work_list.is_empty() {
                self.select_spill()
            }

            !(self.simplify_work_list.is_empty()
                && self.work_list_moves.is_empty()
                && self.freeze_work_list.is_empty()
                && self.spill_work_list.is_empty())
        } {}

        self.assign_colors();

        self.dump_debug(self.function.name, unsafe { ALLOC_COUNTER });

        if !self.spilled_nodes.is_empty() {
            self.rewrite_program();
            self.degree.clear();
            self.adj_list.clear();
            self.adj_set.clear();
            self.init_initial();
            self.allocate();
        }
    }

    fn init_initial(&mut self) {
        // TODO ADD FUNCTION PARAM

        // for (i, reg) in self.function.params.iter().enumerate() {
        //     self.color.insert(*reg, i);
        //     self.degree.insert(*reg, 0);
        //     self.adj_list.insert(*reg, IndexSet::new());
        // }

        for (_, block) in &self.function.blocks {
            for instruction in &block.instructions {
                let used = instruction.used();
                let defs = instruction.def();

                for use_ in used {
                    if !self.pre_colored.contains(&use_) {
                        self.initial.insert(use_);
                        self.degree.insert(use_, 0);
                        self.adj_list.insert(use_, IndexSet::new());
                    }
                }

                for def in defs {
                    if !self.pre_colored.contains(&def) {
                        self.initial.insert(def);
                        self.degree.insert(def, 0);
                        self.adj_list.insert(def, IndexSet::new());
                    }
                }
            }

            match block.end {
                BlockEnd::Return(ref v) => {
                    if !self.pre_colored.contains(v) {
                        self.initial.insert(*v);
                        self.degree.insert(*v, 0);
                        self.adj_list.insert(*v, IndexSet::new());
                    }
                }
                BlockEnd::Branch(ref v, _, _) => {
                    if !self.pre_colored.contains(v) {
                        self.initial.insert(*v);
                        self.degree.insert(*v, 0);
                        self.adj_list.insert(*v, IndexSet::new());
                    }
                }

                _ => {}
            }
        }
    }

    fn liveness_analysis(&mut self) {
        self.state = AnalysisState::new(self.function, self.symbols);
    }

    fn is_spillable(&self, reg: &Register) -> bool {
        if self.spilled_scratch_temps.contains(reg) {
            false
        } else {
            true
        }
    }

    pub(crate) fn new_name(&mut self, n: Register) -> Register {
        let i = *self.name_counter.entry(n).or_insert(0);

        self.name_counter[&n] += 1;

        let name = n.name(self.symbols);

        Register::Named(self.symbols.symbol(&format!("{}_{}", name, i)))
    }

    fn build(&mut self) {
        let mut function: Function = Function::dummy();

        std::mem::swap(&mut function, self.function);

        for (id, block) in &function.blocks {
            let mut live = self.state.live_out[id].clone();

            for instruction in block.instructions.iter().rev() {
                let use_ = instruction.used();
                let def = instruction.def();
                println!("{} live {:?}", id, live);

                if instruction.is_move() {
                    live = live.difference(&use_).cloned().collect::<IndexSet<_>>();
                    let reg_move = match instruction {
                        Instruction::Store(to, from) => Move {
                            to: *to,
                            from: *from,
                        },
                        _ => unreachable!(),
                    };

                    for n in def.union(&use_) {
                        self.move_list
                            .entry(*n)
                            .or_insert(indexset!())
                            .insert(reg_move);
                        self.work_list_moves.insert(reg_move);
                    }
                }

                live = live.union(&def).cloned().collect::<IndexSet<_>>();

                for d in &def {
                    for l in &live {
                        self.add_edge(*l, *d)
                    }
                }

                live = use_
                    .union(&(live.difference(&def).cloned().collect::<IndexSet<_>>()))
                    .cloned()
                    .collect::<IndexSet<_>>();
            }
        }

        std::mem::swap(&mut function, self.function);
    }

    fn add_edge(&mut self, u: Node, v: Node) {
        if !self.adj_set.contains(&(u, v)) && u != v {
            self.adj_set.insert((u, v));
            self.adj_set.insert((v, u));

            if !self.pre_colored.contains(&u) {
                self.adj_list.entry(u).or_insert(indexset!()).insert(v);
                *self.degree.entry(u).or_insert(0) += 1;
            }

            if !self.pre_colored.contains(&v) {
                self.adj_list.entry(v).or_insert(indexset!()).insert(u);
                *self.degree.entry(v).or_insert(0) += 1;
            }
        }
    }

    fn make_work_list(&mut self) {
        println!("initial: {:?}", self.initial);
        while !self.initial.is_empty() {
            let n = self.initial.pop().unwrap();

            if *self.degree.get(&n).unwrap_or(&0) >= K {
                self.spill_work_list.insert(n);
            } else if self.move_related(n) {
                self.freeze_work_list.insert(n);
            } else {
                self.simplify_work_list.insert(n);
            }
        }
    }

    fn adjacent(&self, n: Node) -> IndexSet<Node> {
        self.adj_list
            .get(&n)
            .unwrap_or(&IndexSet::new())
            .difference(
                &self
                    .select_stack
                    .clone()
                    .into_iter()
                    .collect::<IndexSet<_>>()
                    .union(&self.coalesced_nodes)
                    .cloned()
                    .collect::<IndexSet<_>>(),
            )
            .cloned()
            .collect::<IndexSet<_>>()
    }

    fn node_moves(&self, n: Node) -> IndexSet<Move> {
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

    fn move_related(&self, n: Node) -> bool {
        !(self.node_moves(n).is_empty())
    }

    fn simplify(&mut self) {
        println!("simplify called");
        let n = self.simplify_work_list.pop().unwrap();

        self.select_stack.push(n);

        for m in self.adjacent(n) {
            self.decrement_degree(m);
        }
    }

    fn decrement_degree(&mut self, m: Node) {
        let d = self.degree[&m];

        if d != 0 {
            self.degree[&m] -= 1;
        }

        if d == K {
            let mut nodes = self.adjacent(m);

            nodes.insert(m);
            self.enable_moves(nodes);

            self.spill_work_list.remove(&m);

            if self.move_related(m) {
                self.freeze_work_list.insert(m);
            } else {
                self.simplify_work_list.insert(m);
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

    fn coalesce(&mut self) {
        println!("colaesce called");

        let m = self.work_list_moves.pop().unwrap();

        let x = self.get_alias(m.to);

        let y = self.get_alias(m.from);

        let (u, v) = if self.pre_colored.contains(&y) {
            (y, x)
        } else {
            (x, y)
        };

        println!("u:{} v:{}", u, v);

        if u == v {
            self.coalesced_moves.insert(m);
            self.add_work_list(u);
        } else if self.pre_colored.contains(&v) || self.adj_set.contains(&(u, v)) {
            self.constrained_moves.insert(m);
            self.add_work_list(u);
            self.add_work_list(v);
        } else if (self.pre_colored.contains(&u) && self.check_ok(u, v))
            || (!self.pre_colored.contains(&u)
                && self.conservative(
                    self.adjacent(u)
                        .into_iter()
                        .chain(self.adjacent(v).into_iter())
                        .collect::<IndexSet<_>>(),
                ))
        {
            self.coalesced_moves.insert(m);

            self.combine(u, v);
            self.add_work_list(u)
        } else {
            self.active_moves.insert(m);
        }
    }

    fn add_work_list(&mut self, u: Node) {
        if !self.pre_colored.contains(&u) && !self.move_related(u) && self.degree[&u] < K {
            self.freeze_work_list.insert(u);
            self.simplify_work_list.insert(u);
        }
    }

    fn check_ok(&self, u: Node, v: Node) -> bool {
        for t in self.adjacent(v) {
            if !self.ok(t, u) {
                return false;
            }
        }
        true
    }

    fn ok(&self, t: Node, r: Node) -> bool {
        self.degree[&t] < K || self.pre_colored.contains(&t) || self.adj_set.contains(&(t, r))
    }

    fn conservative(&self, nodes: IndexSet<Node>) -> bool {
        let mut k = 0;

        for n in nodes {
            if self.degree[&n] >= K {
                k += 1;
            }
        }

        return k < K;
    }

    fn get_alias(&self, n: Node) -> Node {
        if self.coalesced_nodes.contains(&n) {
            self.get_alias(self.alias[&n])
        } else {
            n
        }
    }

    fn combine(&mut self, u: Node, v: Node) {
        if self.freeze_work_list.contains(&v) {
            self.freeze_work_list.insert(v);
        } else {
            self.spill_work_list.insert(v);
        }

        self.coalesced_nodes.insert(v);

        self.alias.insert(v, u);

        let move_list_v = self.move_list[&v].clone();

        self.move_list[&u].extend(move_list_v);

        for t in self.adjacent(v) {
            self.add_edge(t, u);
            self.decrement_degree(t);
        }

        if *self.degree.get(&u).unwrap_or(&0) >= K && self.freeze_work_list.contains(&u) {
            self.freeze_work_list.remove(&u);
            self.spill_work_list.insert(u);
        }
    }

    fn freeze(&mut self) {
        println!("freeze called");
        let node = self.freeze_work_list.pop().unwrap();

        self.simplify_work_list.insert(node);

        self.freeze_moves(node);
    }

    fn freeze_moves(&mut self, u: Node) {
        for m in self.node_moves(u) {
            let x = m.to;
            let y = m.from;

            let v = if self.get_alias(y) == self.get_alias(u) {
                self.get_alias(x)
            } else {
                self.get_alias(y)
            };

            self.active_moves.remove(&m);

            self.frozen_moves.insert(m);

            if self.node_moves(v).is_empty() && *self.degree.get(&v).unwrap_or(&0) < K {
                self.freeze_work_list.remove(&v);
                self.simplify_work_list.insert(v);
            }
        }
    }

    fn select_spill(&mut self) {
        println!("select spill called");
        let mut costs = indexmap!();

        let mut spill_work_list = IndexSet::new();

        for node in &self.spill_work_list {
            if !self.is_spillable(node) {
                println!(" {} is not spillable", node);
                // continue;
            }

            costs.insert(*node, self.spill_cost(node));
            spill_work_list.insert(*node);
        }

        spill_work_list.sort_by(|a, b| costs[a].partial_cmp(&costs[b]).unwrap_or(Ordering::Less));

        let register = spill_work_list.swap_remove_index(0).unwrap(); // to di add a heuristic to get this
                                                                      //
        self.spill_work_list.remove(&register);
        self.simplify_work_list.insert(register);

        self.freeze_moves(register);
    }

    fn spill_cost(&self, node: &Register) -> f64 {
        let degree = *self.degree.get(node).unwrap_or(&0) as f64;

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

    fn assign_colors(&mut self) {
        println!("assign colours");

        let mut ok_colors = (0..=K - 1).collect::<IndexSet<_>>();

        while !self.select_stack.is_empty() {
            let n = self.select_stack.pop().unwrap();

            for w in self.adj_list.get(&n).unwrap_or(&IndexSet::new()) {
                let coloured = self
                    .colored_nodes
                    .union(&self.pre_colored)
                    .collect::<IndexSet<_>>();
                if coloured.contains(&self.get_alias(*w)) {
                    ok_colors.remove(&self.color[&self.get_alias(*w)]);
                }
            }

            if ok_colors.is_empty() {
                self.spilled_nodes.insert(n);
            } else {
                if self.next_colour + 1 > ok_colors.len() - 1 {
                    self.next_colour = 0;
                }

                self.colored_nodes.insert(n);

                let c = ok_colors.pop().unwrap();

                self.color.insert(n, c);

                ok_colors.insert(c);
            }
        }
    }

    fn rewrite_program(&mut self) {
        use crate::instructions::{Instruction::*, Value, POINTER_WIDTH};

        let mut new_temps: IndexSet<Register> = IndexSet::new();

        // mapping of old to new
        //
        println!("spilled nodes{:?}", self.spilled_nodes);

        for spilled_node in &self.spilled_nodes {
            // allocate memory locations for each spilled_node.

            self.spill_mem_location.insert(*spilled_node,Register::new());
            self.offsets.insert(*spilled_node, self.offset);
            self.offset += POINTER_WIDTH;
        }

        println!("mem_locs {:?}",self.spill_mem_location);

        let mut blocks = IndexMap::new();

        std::mem::swap(&mut blocks, &mut self.function.blocks);

        for (_, block) in blocks.iter_mut() {
            let mut new_instructions = Vec::new();

            let mut instructions = Vec::new();

            std::mem::swap(&mut instructions, &mut block.instructions);

            

            for instruction in instructions.into_iter() {
                self.rewrite_instruction(instruction, &mut new_instructions, &mut new_temps);
            }

            std::mem::swap(&mut block.instructions, &mut new_instructions);
        }

        std::mem::swap(&mut blocks, &mut self.function.blocks);

        let file_name = format!(
            "graphviz/{name}/{name}_alloc_{count}.tox",
            name = self.symbols.name(self.function.name),
            count = unsafe { ALLOC_COUNTER },
        );

        let mut printer = crate::printer::Printer::new(&self.symbols);

        printer
            .print_function(
                &self.function,
                &mut std::fs::File::create(file_name).unwrap(),
            )
            .unwrap();

        self.spilled_nodes.clear();

        self.initial = self
            .colored_nodes
            .union(
                &self
                    .coalesced_nodes
                    .union(&new_temps)
                    .cloned()
                    .collect::<IndexSet<_>>(),
            )
            .cloned()
            .collect::<IndexSet<_>>();

        self.colored_nodes = IndexSet::new();
        self.coalesced_nodes = IndexSet::new();
        self.spilled_scratch_temps.extend(new_temps.iter());
        self.old_to_new.clear();
    }
}
