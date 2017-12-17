#[cfg(test)]

mod test {
    use types::Type;
    use inference::{analyse, ExpressionType};
    use pos::WithPos;
    use ast::statement::Statement;
    use std::rc::Rc;
    use symbol::SymbolFactory;
    use env::Env;

    fn get_ast(input: &str, strings: Rc<SymbolFactory>) -> Vec<WithPos<Statement>> {
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
            analyse(&get_ast(input, strings), &mut env).unwrap(),
            vec![
                ExpressionType {
                    exp: (),
                    ty: Type::Int,
                },
            ]
        );
    }

    #[test]
    fn is_float() {
        let input = "123.0+456.0;";
        let strings = Rc::new(SymbolFactory::new());
        let mut env = Env::new(&strings);
        assert_eq!(
            analyse(&get_ast(input, strings), &mut env).unwrap(),
            vec![
                ExpressionType {
                    exp: (),
                    ty: Type::Float,
                },
            ]
        );
    }

    #[test]
    #[should_panic]
    fn float_int() {
        let input = "123.0+456;";
        let strings = Rc::new(SymbolFactory::new());
        let mut env = Env::new(&strings);
        analyse(&get_ast(input, strings), &mut env).unwrap();
    }

    #[test]
    #[should_panic]
    fn int_str() {
        let input = "10+\"h\";";
        let strings = Rc::new(SymbolFactory::new());
        let mut env = Env::new(&strings);
        analyse(&get_ast(input, strings), &mut env).unwrap();
    }

    #[test]
    #[should_panic]
    fn float_str() {
        let input = "10.0+\"h\";";
        let strings = Rc::new(SymbolFactory::new());
        let mut env = Env::new(&strings);
        analyse(&get_ast(input, strings), &mut env).unwrap();
    }

    #[test]
    fn assign_types() {
        let input =
            "var a = 10; var b = 10.0; var c = nil; var d = \"h\"; var e = true; var f = false; ";
        let strings = Rc::new(SymbolFactory::new());
        let mut env = Env::new(&strings);
        assert_eq!(
            analyse(&get_ast(input, strings), &mut env).unwrap(),
            vec![
                ExpressionType {
                    exp: (),
                    ty: Type::Int,
                },
                ExpressionType {
                    exp: (),
                    ty: Type::Float,
                },
                ExpressionType {
                    exp: (),
                    ty: Type::Nil,
                },
                ExpressionType {
                    exp: (),
                    ty: Type::Str,
                },
                ExpressionType {
                    exp: (),
                    ty: Type::Bool,
                },
                ExpressionType {
                    exp: (),
                    ty: Type::Bool,
                },
            ]
        );
    }

}
