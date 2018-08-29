#[macro_use]
extern crate nom;

#[macro_use]
mod macros;
mod assembler;
mod chunks;
mod opcode;
mod vm;

pub use assembler::file;
pub use vm::VM;
pub use nom::types::CompleteStr;