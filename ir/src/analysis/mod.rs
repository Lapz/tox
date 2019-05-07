mod analysis;
mod dead_code;
mod color;

use crate::instructions::{BlockID, Register};
use std::collections::{HashMap, HashSet};
use util::symbol::Symbol;

#[derive(Debug, Clone, Default)]
pub struct Analysis {
    state: HashMap<Symbol, AnalysisState>,
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
    pub live_now: HashMap<BlockID, HashSet<Register>>,
}

pub fn optimizations(p: crate::instructions::Program) {
    let mut state = Analysis::new();

    for function in &p.functions {
        let mut function_state = AnalysisState::new();
        function_state.init(&function);
        function_state.calculate_successors(function);
        function_state.calulate_live_out(function);
        function_state.calulate_live_now(function);
        state.insert(function.name, function_state)
    }
}
