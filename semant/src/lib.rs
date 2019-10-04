mod ctx;
mod hir;
mod lower;

use syntax::SyntaxNode;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
