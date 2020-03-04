use crate::parser::Parser;
use syntax::T;

use crate::SyntaxKind::*;

impl<'a> Parser<'a> {
    pub(crate) fn parse_visibility(&mut self) {
        self.start_node(VISIBILITY);
        self.expect(T![export]);
        self.finish_node();
    }
}

#[cfg(test)]
mod test {
    use crate::utils::parse;
    use syntax::{FnDefOwner, VisibilityOwner};
    #[test]
    fn test_visibility() {
        let source_file = parse("export fn main(){}");

        let func = source_file.functions().next().unwrap();

        assert!(func.visibility().is_some())
    }
    #[test]
    fn test_visibility_not_present() {
        let source_file = parse("fn main(){}");

        let func = source_file.functions().next().unwrap();

        assert!(func.visibility().is_none())
    }

    test_parser! {parse_pub_function,"export fn main() {}"}
    test_parser! {parse_pub_alias,"export type Foo=i32;"}
    test_parser! {parse_pub_enum,"export enum Bar{};"}
    test_parser! {parse_pub_class,"export class Foo{ bar:string;};"}
}
