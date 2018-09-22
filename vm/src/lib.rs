//! The VM module it contains two sub projects
//! a ```VM``` and an ```Assembler``` for the tasm langauage

#[macro_use]
extern crate nom;

#[macro_use]
mod macros;
mod assembler;
#[cfg(feature = "debug")]
mod debug;
mod opcode;
mod vm;

pub use assembler::Assembler;
pub use vm::VM;
