use crate::parser::Parser;
use crate::T;

use crate::{Span, SyntaxKind::*, Token};

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Span<Token>>,
{
    pub(crate) fn parse_visibility(&mut self) {
        self.start_node(VISIBILITY);

        self.expect(EXPORT_KW, "Expected `export`");

        self.finish_node();
    }
}

#[cfg(test)]
mod test {
    use crate::utils::parse;
    use syntax::{FnDefOwner, VisibilityOwner};
    #[test]
    fn test_visibility() {
        let source_file = parse("export fn main(){}").parse_program();

        let func = source_file.functions().nth(0).unwrap();

        assert!(func.visibility().is_some())
    }
    #[test]
    fn test_visibility_not_present() {
        let source_file = parse("fn main(){}").parse_program();

        let func = source_file.functions().nth(0).unwrap();

        assert!(func.visibility().is_none())
    }

    test_parser! {parse_pub_function,"export fn main(){}"}
}
