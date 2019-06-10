mod analysis;
mod color;
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
use std::fs::File;
use util::symbol::{Symbol, Symbols};
#[derive(Debug, Clone, Default)]
pub struct Analysis {
    state: IndexMap<Symbol, AnalysisState>,
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
    pub used_defined: IndexMap<BlockID, (IndexSet<Register>, IndexSet<Register>)>,
    pub successors: IndexMap<BlockID, IndexSet<BlockID>>,
    pub predecessors: IndexMap<BlockID, IndexSet<BlockID>>,
    pub live_in: IndexMap<BlockID, IndexSet<Register>>,
    pub live_out: IndexMap<BlockID, IndexSet<Register>>,
    pub live_now: IndexMap<BlockID, IndexSet<Register>>,
    pub dominator: IndexMap<BlockID, IndexSet<BlockID>>,
    pub frontier: IndexMap<BlockID, IndexSet<BlockID>>,
    pub intervals: IndexMap<BlockID, IndexMap<Register, Interval>>,
    pub ssa_blocks: IndexMap<Register, IndexSet<BlockID>>,
    pub global_regs: IndexSet<Register>,
    pub name_counter:IndexMap<Register,usize>,
    pub stack_counter:IndexMap<Register,Vec<usize>>,
}

impl AnalysisState {
    pub fn new(function: &mut Function,symbols: &mut Symbols<()>) -> Self {
        let mut state = Self::default();

        state.init(&function);
        state.calculate_successors(function);

        // state.calulate_live_out(function);

        // state.calculate_live_now(function);
        state.find_dominance(function);
        state.find_dominance_frontier(function);
        // state.calculate_live_intervals(function);

        state.find_global_names(function);
        state.rewrite_code(function);
        state.rename_registers(function,symbols);
        state.rename(BlockID(3),function,symbols);
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
                    linear_scan::Allocator::new(symbols, function)
                }
            };

            // allocator.allocate()

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
#[cfg(feature = "graphviz")]
    p.graphviz(symbols).unwrap();
}
