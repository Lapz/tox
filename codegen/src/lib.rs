extern crate syntax;
extern crate util;
extern crate vm;

mod gen_bytecode;
mod label;

pub use gen_bytecode::Compiler;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
