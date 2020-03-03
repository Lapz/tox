use syntax::T;

use crate::parser::Parser;

use crate::SyntaxKind::*;

impl<'a> Parser<'a> {
    pub(crate) fn parse_enum(&mut self, checkpoint: rowan::Checkpoint) {
        self.start_node_at(checkpoint, ENUM_DEF);

        self.expect(T![enum], "Expected `enum`");

        self.ident();

        if self.at(T![<]) {
            self.parse_type_params(false);
        }

        self.parse_enum_variants();

        self.finish_node()
    }

    fn parse_enum_variants(&mut self) {
        self.start_node(ENUM_VARIANT_LIST);

        self.expect(T!["{"], "Expected `{`");

        while !self.at(EOF) && !self.at(T!["}"]) {
            self.parse_enum_variant();
            if !self.at(T!["}"]) && !self.expected(T![,]) {
                break;
            }
        }

        self.expect(T!["}"], "Expected `}`");

        self.finish_node();
    }

    fn parse_enum_variant(&mut self) {
        self.start_node(ENUM_VARIANT);

        self.ident();

        if self.at(T!["("]) {
            self.parse_enum_variant_types()
        }

        self.finish_node();
    }

    fn parse_enum_variant_types(&mut self) {
        self.bump(); // Eat the "("
        while !self.at(EOF) && !self.at(T![")"]) {
            self.parse_type();

            if !self.at(T![")"]) && !self.expected(T![,]) {
                break;
            }
        }

        self.expect(T![")"], "Expected `>` to close type params");
    }
}

#[cfg(test)]
mod test {
    test_parser! {parse_empty_enum,"enum Foo {}"}
    test_parser! {parse_enum_with_variants,"enum Foo {A,B,C}"}
    test_parser! {parse_enum_variants_trailing_comma,"enum Foo {A,B,C,}"}
    test_parser! {parse_enum_variants_types,"enum Foo { A([i32]),B(i32),C((i32,i32)),}"}
    test_parser! {parse_enum_generic,"enum Result<T,E> { Ok(T),Err(E)}"}
}
