#[macro_use]
extern crate nom;

#[macro_use]
mod macros;
mod assembler;
mod debug;
mod opcode;
mod vm;

pub use assembler::file;
pub use vm::VM;
pub use nom::types::CompleteStr;