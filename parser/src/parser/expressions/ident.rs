use syntax::T;

use crate::parser::pratt::PrefixParser;
use crate::parser::Parser;
use crate::SyntaxKind::*;

#[derive(Debug)]
pub struct IdentParselet;

impl PrefixParser for IdentParselet {
    fn parse(&self, parser: &mut Parser) {
        parser.start_node(IDENT_EXPR);
        parser.start_node(NAME);

        parser.expect(IDENT);

        if parser.at(T![::]) {
            parser.bump();
            parser.parse_type_args();
        };

        parser.finish_node();
        parser.finish_node();
    }
}

#[cfg(test)]
mod test {
    test_parser! {
        parse_ident_expr,"fn main(){a;}"
    }
    test_parser! {
        parse_generic_ident_expr,"fn main(){a::<i32>;}"
    }
}
