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
