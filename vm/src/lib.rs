//! The VM module it contains two sub projects
//! a ```VM``` and an ```Assembler``` for the tasm langauage

#[macro_use]
extern crate nom;

extern crate opcode;

extern crate util;

#[macro_use]
mod macros;
mod chunk;
mod value;
mod vm;
pub use chunk::Chunk;
pub use value::Value;
pub use vm::VM;

#[derive(Debug)]
pub struct Function {
    pub name: ::util::symbol::Symbol,
    pub body: Chunk,
    pub locals: ::std::collections::HashMap<::util::symbol::Symbol, usize>,
}
