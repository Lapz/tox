mod ctx;
mod db;
mod hir;
mod lower;

pub use db::DatabaseImpl;
pub use lower::lower_ast;
pub use salsa;
use syntax::SyntaxNode;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
