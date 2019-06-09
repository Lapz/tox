#![feature(vec_remove_item)]
#![feature(const_fn)]
#[cfg(feature = "prettytable")]
extern crate text_tables;
pub mod analysis;
#[cfg(feature = "graphviz")]
pub mod graphviz;
pub mod instructions;
pub mod printer;
pub mod ssa;
pub const POINTER_SIZE: usize = 8;
pub const FUNCTION_SIZE: usize = 8;

pub use analysis::optimizations;

pub use indexmap::{IndexMap, IndexSet};
