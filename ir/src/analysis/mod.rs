mod allocator;
mod analysis;
mod color;
mod dead_code;

use crate::analysis::allocator::Allocator;
use crate::instructions::{BlockID, Register};
use std::collections::{HashMap, HashSet};
use util::symbol::{Symbol, Symbols};
#[derive(Debug, Clone, Default)]
pub struct Analysis {
    state: HashMap<Symbol, AnalysisState>,
}
#[derive(Debug, Clone, Copy)]
pub struct Interval {
    start: usize,
    end: usize,
}

impl Interval {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

impl std::fmt::Display for Interval {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl Analysis {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, key: Symbol, state: AnalysisState) {
        self.state.insert(key, state);
    }
}

#[derive(Debug, Clone, Default)]
pub struct AnalysisState {
    pub used_defined: HashMap<BlockID, (HashSet<Register>, HashSet<Register>)>,
    pub successors: HashMap<BlockID, HashSet<BlockID>>,
    pub predecessors: HashMap<BlockID, HashSet<BlockID>>,
    pub live_in: HashMap<BlockID, HashSet<Register>>,
    pub live_out: HashMap<BlockID, HashSet<Register>>,
    pub intervals: HashMap<Register, Interval>,
}

pub fn optimizations(symbols: &mut Symbols<()>, p: crate::instructions::Program) {
    let mut state = Analysis::new();

    for function in &p.functions {
        let mut allocator = Allocator::new(symbols, function);
        allocator.build_interference_graphs(function);
        allocator.allocate(0, function);
    }
}
