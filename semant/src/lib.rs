mod ctx;
mod db;
mod hir;
mod lower;

pub use lower::lower_ast;
use syntax::SyntaxNode;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
