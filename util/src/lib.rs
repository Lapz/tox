//! This library provides common items that are used throughout the tox project
extern crate ansi_term;
extern crate fnv;
pub mod emmiter;
pub mod macros;
pub mod pos;
pub mod symbol;

static mut UNIQUE_COUNT: u64 = 0;

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Unique(pub u64);

impl Unique {
    pub fn new() -> Self {
        let value = unsafe { UNIQUE_COUNT };
        unsafe { UNIQUE_COUNT += 1 };
        Unique(value)
    }
}


pub fn print_err(err:String) {
    use ansi_term::Colour::{Fixed, Red};
    println!("{}: {}",Red.bold().paint("Runtime Error"),
     Fixed(252).bold().paint(err)
   );
}