#[macro_use]
extern crate nom;

#[macro_use]
mod macros;
mod assembler;
mod debug;
mod opcode;
mod vm;

pub use assembler::file;
pub use nom::types::CompleteStr;
pub use vm::VM;
