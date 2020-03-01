mod ctx;
mod db;
mod hir;
mod infer;
mod lower;
mod resolver;
mod ty;
mod util;

pub use db::{HirDatabase, HirDatabaseStorage, InternDatabaseStorage};
pub use salsa;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
