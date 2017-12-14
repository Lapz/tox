#[cfg(test)]

mod test {
    use types::Type;
    use inference::{analyse, ExpressionType};
    use pos::WithPos;
    use ast::statement::Statement;
    use std::rc::Rc;
    use symbol::SymbolFactory;
    use env::Env;

    fn get_ast(input: &str,strings:Rc<SymbolFactory>) -> Vec<WithPos<Statement>> {
        use lexer::Lexer;
        use parser::Parser;
        use symbol::Symbols;
        

        let tokens = Lexer::new(input).lex().unwrap();
     
        let mut symbols = Symbols::new(strings);
        Parser::new(tokens, &mut symbols).parse().unwrap()
    }

    #[test]
    fn is_int() {
        let input = "123+456;";
        let strings = Rc::new(SymbolFactory::new());
        let mut env = Env::new(&strings);
        assert_eq!(
            analyse(&get_ast(input,strings)[0],&mut env).unwrap(),
            ExpressionType {
                exp: (),
                ty: Type::Int,
            }
        );
    }

    #[test]
    fn is_float() {
        let input = "123.0+456.0;";
        let strings = Rc::new(SymbolFactory::new());
        let mut  env = Env::new(&strings);
        assert_eq!(
            analyse(&get_ast(input,strings)[0],&mut env).unwrap(),
            ExpressionType {
                exp: (),
                ty: Type::Float,
            }
        );
    }

    #[test]
    #[should_panic]
    fn float_int() {
        let input = "123.0+456";
        let strings = Rc::new(SymbolFactory::new());
        let mut  env = Env::new(&strings);
        analyse(&get_ast(input,strings)[0],&mut env).unwrap();
    }

}
