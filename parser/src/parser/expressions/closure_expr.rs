use syntax::T;

use crate::parser::Parser;

use crate::SyntaxKind::*;

use crate::parser::PrefixParser;

#[derive(Debug)]
pub struct ClosureParselet;

impl<'a> Parser<'a> {
    pub(crate) fn parse_closure_expr(&mut self) {
        self.start_node(CLOSURE_EXPR);

        self.parse_func_params(T![|]);

        if self.current() == T![->] {
            self.parse_return_type();
        }

        self.parse_block();

        self.finish_node()
    }
}

impl PrefixParser for ClosureParselet {
    fn parse(&self, parser: &mut Parser) {
        parser.parse_closure_expr();
    }
}

#[cfg(test)]
mod tests {
    test_parser! {parse_simple_closure_expr,r"
        fn main() {
            let a = |x,y| {
                return x+y;
            }
    }"}
    test_parser! {parse_closure_with_types,"fn main() {
        let a = |x,y| -> i32 {
            return x+y; 
        }
    }"}
    test_parser! {parse_empty_closure_expr,"fn main() {
        let a = || {};
    }"}
    test_parser! {parse_free_closure_expr,"fn main() {
        || {};
    }"}
}
