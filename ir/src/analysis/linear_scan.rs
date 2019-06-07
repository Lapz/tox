use crate::analysis::{AnalysisState, Interval};
use crate::instructions::{
    BlockID, Function, Instruction, Register, POINTER_WIDTH, RAX, RBP, RES, STACK_POINTER,
};
use indexmap::{IndexMap, IndexSet};

use petgraph::{graphmap::GraphMap, Undirected};

use std::hash;
use util::symbol::Symbols;

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct StackLocation {
    offset: usize,
    interval: Interval,
}

const MAX_REGISTER: usize = 3;
#[derive(Debug)]
pub struct Allocator<'a> {
    state: AnalysisState,
    function: &'a mut Function,
    ranges: IndexSet<Interval>,
    active: IndexMap<Register, Interval>,
    free_registers: IndexMap<Register, Interval>,
    pub(crate) symbols: &'a mut Symbols<()>,
    location: IndexMap<BlockID, IndexMap<Register, StackLocation>>,
    current_register: usize,
    offset: usize,
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
        Self {
            state: AnalysisState::new(function),
            symbols,
            function,
            active: hashmap!(),
            ranges: hashset!(),
            current_register: 10,
            free_registers: hashmap!(RAX => Interval::default(),RES => Interval::default(),RBP => Interval::default()),
            location: hashmap!(),
            offset: 0,
        }
    }

    pub fn allocate(&mut self) {
        let mut mappings = IndexMap::new();
        let mut blocks = Vec::new();

        std::mem::swap(&mut blocks, &mut self.function.blocks);

        for (id, _) in &blocks {
            self.location.insert(*id, hashmap!());
            let mut intervals = IndexMap::new();

            std::mem::swap(&mut intervals, &mut self.state.intervals[id]);
            // println!("unsorted {:#?}", intervals);
            intervals.sort_by(|_, v1, _, v2| v1.start.cmp(&v2.start));

            for (reg, interval) in &intervals {
                self.expire(*reg, *interval);

                if self.active.len() == MAX_REGISTER {
                    self.spill_at_interval(*reg, *interval, *id)
                } else {
                    if let Some((free_reg, _)) = self.free_registers.pop() {
                        self.active.insert(free_reg, *interval);

                        mappings
                            .entry(*reg)
                            .or_insert(IndexSet::new())
                            .insert(free_reg);

                        self.active.sort_by(|_, v1, _, v2| v1.end.cmp(&v2.end));
                    }
                }
            }

            std::mem::swap(&mut self.state.intervals[id], &mut intervals);
        }

        std::mem::swap(&mut self.function.blocks, &mut blocks);

        println!("spilled {:#?}", self.location);
        println!("active {:#?}", self.active);
    }

    fn expire(&mut self, reg: Register, interval: Interval) {
        self.active.sort_by(|_, v1, _, v2| v1.end.cmp(&v2.end));

        while !self.active.is_empty() {
            let (active_reg, active_interval) = self.active.swap_remove_index(0).unwrap();

            if active_interval.end >= interval.start {
                self.active.insert(active_reg, active_interval);
                return;
            }

            self.free_registers.insert(active_reg, active_interval);
        }
    }

    fn spill_at_interval(&mut self, reg: Register, interval: Interval, block_id: BlockID) {
        let (spill_register, spill_interval) = self.active.pop().unwrap();

        if spill_interval.end > interval.end {
            self.free_registers.insert(reg, spill_interval);

            self.location[&block_id].insert(
                spill_register,
                StackLocation {
                    interval: spill_interval,
                    offset: self.offset,
                },
            );

            self.offset += POINTER_WIDTH;

            self.active.remove(&spill_register);

            self.active.sort_keys();
        } else {
            self.location[&block_id].insert(
                reg,
                StackLocation {
                    interval: spill_interval,
                    offset: self.offset,
                },
            );

            self.offset += POINTER_WIDTH;
        }
    }
}
