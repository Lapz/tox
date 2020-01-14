mod db;
mod hir;
mod infer;
mod lower;
mod ty;

pub use db::DatabaseImpl;
pub use lower::lower_ast;
pub use salsa;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
