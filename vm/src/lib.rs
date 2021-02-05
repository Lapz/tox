#[macro_use]
mod macros;
mod chunks;
mod codegen;
mod db;
mod ir;
mod native;
mod object;
mod value;
mod vm;
pub use db::{CodegenDatabase, CodegenDatabaseStorage};
pub use vm::VM;
#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
