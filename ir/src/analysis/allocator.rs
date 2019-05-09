use crate::analysis::AnalysisState;
use crate::instructions::{BlockEnd, BlockID, Function, Instruction, Register};
use indexmap::set::IndexSet;
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
    active: HashSet<Register>,
    free: HashSet<Register>,
    symbols: &'a mut Symbols<()>,
}

impl<'a> Allocator<'a> {
    pub fn new(symbols: &'a mut Symbols<()>, state: AnalysisState) -> Self {
        Self {
            state,
            symbols,
            active: HashSet::new(),
            free: HashSet::new(),
        }
    }

    pub fn allocate(&mut self, count: usize, function: &mut Function) {
        // for (reg, interval) in &self.state.intervals {
        //     for j in self.active.clone() {
        //         if &self.state.intervals[&j].end >= &self.state.intervals[reg].start {
        //             return;
        //         }

        //         self.active.remove(&j);

        //         self.free.insert(j);
        //     }

        //     if self.active.len() == NUMBER_REGISTER {
        //         //    self.spill(/)
        //     } else {
        //         self.free.insert(*reg);
        //     }
        // }
    }
}
