//! This library provides common items that are used throughout the tox project
extern crate ansi_term;

pub mod macros;
pub mod pos;
pub mod symbol;
pub mod env;
pub mod types;
pub mod emmiter;

static mut UNIQUE_COUNT: u64 = 0;

#[derive(Clone, Debug, PartialEq)]
pub struct Unique(pub u64);

impl Unique {
    pub fn new() -> Self {
        let value = unsafe { UNIQUE_COUNT };
        unsafe { UNIQUE_COUNT += 1 };
        Unique(value)
    }
}
