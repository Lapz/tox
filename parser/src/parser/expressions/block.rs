use syntax::T;

use crate::parser::{pratt::Precedence, Parser, Restrictions};

use crate::SyntaxKind::*;

impl<'a> Parser<'a> {
    pub(crate) fn parse_block(&mut self) {
        self.start_node(BLOCK_EXPR);
        self.start_node(BLOCK);

        self.expect(T!["{"]);

        while !self.at(EOF) && !self.at(T!["}"]) {
            match self.current() {
                T![let] => {
                    // self.start_node(EXPR_STMT);
                    self.parse_let_expr();
                    // self.finish_node();
                }
                T![if] => {
                    self.start_node(EXPR_STMT);
                    self.parse_if_expr();
                    self.finish_node();
                }
                T![do] => {
                    self.start_node(EXPR_STMT);
                    self.parse_do_expr();
                    self.finish_node();
                }
                T![while] => {
                    self.start_node(EXPR_STMT);
                    self.parse_while_expr();
                    self.finish_node();
                }
                T![return] => {
                    self.start_node(EXPR_STMT);
                    self.parse_return_expr();
                    self.finish_node();
                }
                T![break] => {
                    self.start_node(EXPR_STMT);
                    self.parse_break_expr();
                    self.finish_node();
                }
                T![continue] => {
                    self.start_node(EXPR_STMT);
                    self.parse_continue_expr();
                    self.finish_node();
                }
                T![for] => {
                    self.start_node(EXPR_STMT);
                    self.parse_for_expr();
                    self.finish_node();
                }
                T![match] => {
                    self.start_node(EXPR_STMT);
                    self.parse_match_expr();
                    self.finish_node();
                }
                T![|] => {
                    self.start_node(EXPR_STMT);
                    self.parse_closure_expr();
                    self.finish_node();
                }
                T!["//"] | T!["/*"] => {
                    while self.at(T!["//"]) {
                        self.bump();
                    }
                    continue;
                }
                T!["{"] => {
                    self.start_node(EXPR_STMT);
                    self.parse_block();
                    self.finish_node();
                }

                _ => {
                    self.start_node(EXPR_STMT);
                    self.parse_expression(Precedence::Assignment, Restrictions::default());
                    self.finish_node();
                }
            }

            if self.at(T![;]) {
                self.bump();
            }
        }

        self.expect(T!["}"]);

        self.finish_node();
        self.finish_node()
    }
}

#[cfg(test)]
mod tests {
    test_parser! {parse_free_block, "fn main() {{}}"}
    test_parser! {parse_nested_block,"fn main() {{{}}}"}
    test_parser! {parse_block_with_statements,"fn main() {{
        let x = 10;
        break;
        continue;
    } let y =10;}"}
}
