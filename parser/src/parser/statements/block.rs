use crate::T;

use crate::parser::Parser;

use crate::{Span, SyntaxKind::*, Token};

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Span<Token>>,
{
    pub(crate) fn parse_block(&mut self) {
        self.start_node(BLOCK);

        self.expect(T!["{"], "Expected `{`");

        self.expect(T!["}"], "Expected `}`");

        self.finish_node()
    }
}

#[cfg(test)]
mod test {

    use crate::utils::parse;
    use syntax::{ClassDefOwner, NamedFieldsOwner};
    #[test]
    fn test_class_fields() {
        let source_file = parse("class Person { name:String; surname:String;}").parse_program();

        let class = source_file.classes().nth(0).unwrap();

        assert_eq!(class.fields().count(), 2)
    }
    test_parser! {parse_empty_class,"class Foo {}"}
    test_parser! {parse_class_generic,"class Result<T,E> {}"}
    test_parser! {parse_class_fields,"class Person { name:String; surname:String;}"}
    test_parser! {parse_class_fields_methods,"class Person { name:String; surname:String; fn hello(self) {}}"}
    test_parser! {parse_class_methods,"class Person { name:String; surname:String; fn new() -> Person {}}"}
}
