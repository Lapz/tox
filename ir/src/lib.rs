#[cfg(feature = "graphviz")]
pub mod graphviz;
pub mod instructions;
pub mod printer;

pub const POINTER_SIZE: usize = 8;
pub const FUNCTION_SIZE: usize = 8;
