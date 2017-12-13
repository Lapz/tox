#[cfg(test)]

mod test {
    use types::Type;
    use inference::{analyse,ExpressionType};
    use pos::WithPos;
    use ast::statement::Statement;

    fn get_ast(input: &str) -> Vec<WithPos<Statement>> {
        use lexer::Lexer;
        use parser::Parser;
        use symbol::Symbols;
        


        let tokens = Lexer::new(input).lex().unwrap();
        let mut symbols = Symbols::new();
        Parser::new(tokens, &mut symbols).parse().unwrap()
    }
    
    #[test]
    fn is_int() {
        let input = "123+456;";
        assert_eq!(analyse(&get_ast(input)[0]).unwrap(),ExpressionType {
            exp:(),
            ty: Type::Int,
        });
    }

    #[test]
    fn is_float() {
        let input = "123.0+456.0;";
        assert_eq!(analyse(&get_ast(input)[0]).unwrap(),ExpressionType {
            exp:(),
            ty: Type::Float,
        });
    }

    #[test]
    #[should_panic]
    fn float_int() {
        let input = "123.0+456";
        analyse(&get_ast(input)[0]).unwrap();
    }


}