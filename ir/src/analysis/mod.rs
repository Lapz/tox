mod analysis;
// mod color;
mod color2;
mod dead_code;
mod linear_scan;

#[cfg(feature = "graphviz")]
mod debug;
#[cfg(not(feature = "linear_scan"))]
use crate::analysis::color2::Allocator;
#[cfg(feature = "linear_scan")]
use crate::analysis::linear_scan::Allocator;
use crate::instructions::{BlockID, Function, Register};
use indexmap::map::IndexMap;
use indexmap::set::IndexSet;
use std::collections::HashMap;
use std::fs::File;
use util::symbol::{Symbol, Symbols};
#[derive(Debug, Clone, Default)]
pub struct Analysis {
    state: HashMap<Symbol, AnalysisState>,
}
#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq, PartialOrd, Ord, Default)]
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
    pub used_defined: HashMap<BlockID, (IndexSet<Register>, IndexSet<Register>)>,
    pub successors: HashMap<BlockID, IndexSet<BlockID>>,
    pub predecessors: HashMap<BlockID, IndexSet<BlockID>>,
    pub live_in: HashMap<BlockID, IndexSet<Register>>,
    pub live_out: HashMap<BlockID, IndexSet<Register>>,
    pub live_now: HashMap<BlockID, IndexSet<Register>>,
    pub intervals: IndexMap<BlockID, IndexMap<Register, Interval>>,
}

impl AnalysisState {
    pub fn new(function: &Function) -> Self {
        let mut state = Self::default();

        state.init(&function);
        state.calculate_successors(function);
        state.calulate_live_out(function);
         state.calculate_live_now(function);
        state.calculate_live_intervals(function);
       
        state
    }

    pub fn empty() -> Self {
        Self::default()
    }
}

pub fn optimizations(symbols: &mut Symbols<()>, p: &mut crate::instructions::Program) {
    for function in &mut p.functions {
        {
            let mut allocator = {
                #[cfg(feature = "graphviz")]
                {
                    linear_scan::Allocator::new(symbols, function)
                }
                #[cfg(not(feature = "graphviz"))]
                {
                    color2::Allocator::new(function)
                }
            };

            allocator.allocate()

            // function.registers = allocator.color;
            // function.stack_locs = allocator.mappings;

            // println!("{:?}", allocator)
        }

        let file_name = format!(
            "graphviz/{name}/{name}_after_reg.tox",
            name = symbols.name(function.name)
        );

        let mut printer = crate::printer::Printer::new(&symbols);

        printer
            .print_function(&function, &mut File::create(file_name).unwrap())
            .unwrap();
    }
}
