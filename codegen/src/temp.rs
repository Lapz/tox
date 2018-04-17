// use syntax::ast::Ident;
use std::fmt::{self, Debug, Display};
use util::symbol::{Symbol, Symbols};
// use std::io::{self, Write};

/// A Label represents an address in assembly language.
pub type Label = Symbol;

/// A Temporary address in assembly language.
#[derive(Clone, Copy, PartialEq, PartialOrd, Hash)]
pub struct Temp(pub u32);

static mut TEMP_COUNT: u32 = 0;
static mut LABEL_COUNT: u32 = 0;

pub fn new_label(symbols: &mut Symbols<(u32, bool)>) -> Symbol {
    unsafe { symbols.symbol(&format!("L{}", LABEL_COUNT)) }
}

impl Temp {
    /// Makes a new temp with a given Ident.
    /// Warning: avoid repeated calls with the same name.
    pub fn new() -> Self {
        let value = unsafe { TEMP_COUNT };
        unsafe { TEMP_COUNT += 1 };
        Temp(value)
    }
}

impl Debug for Temp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "t{}", self.0)
    }
}

impl Display for Temp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "t{}", self.0)
    }
}
