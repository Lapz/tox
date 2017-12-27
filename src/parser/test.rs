#[cfg(test)]
mod test {
    use lexer::Lexer;
    use parser::Parser;
    use ast::expr::*;
    use ast::statement::*;
    use pos::{Postition, WithPos};
    use symbol::{Symbol, SymbolFactory, Symbols};
    use std::rc::Rc;

    #[test]
    fn types() {
        let input = "var a:int = 10;";
        let tokens = Lexer::new(input).lex().unwrap();
        let strings = Rc::new(SymbolFactory::new());
        let mut symbols = Symbols::new(strings);
        let ast = Parser::new(tokens, &mut symbols).parse().unwrap();

        let expected = vec![
            WithPos::new(
                Statement::Var(
                    Symbol(2),
                    WithPos::new(
                        Expression::Literal(Literal::Int(10)),
                        Postition {
                            line: 1,
                            column: 13,
                            absolute: 12,
                        },
                    ),
                    Some(Symbol(3)),
                ),
                Postition {
                    line: 1,
                    column: 1,
                    absolute: 0,
                },
            ),
        ];

        assert_eq!(expected, ast);
    }

    #[test]
    fn function_types() {
        let input = "fun add(a:int,b:int) {return a+b;}";
        let tokens = Lexer::new(input).lex().unwrap();
        let strings = Rc::new(SymbolFactory::new());
        let mut symbols = Symbols::new(strings);
        let ast = Parser::new(tokens, &mut symbols).parse();
        assert!(ast.is_ok())
    }

    #[test]
    fn type_alias() {
        let input = "type Int = str; type Array = int;";
        let tokens = Lexer::new(input).lex().unwrap();
        let strings = Rc::new(SymbolFactory::new());
        let mut symbols = Symbols::new(strings);
        let ast = Parser::new(tokens, &mut symbols).parse();
        assert!(ast.is_ok())
    }

    #[test]
    fn class_instance() {
        let input = "class Person {name:str,surname:str,age:int;fun hello(a:int,b:int){nil;}}
        var Louis = Person{name:\"Louis\",surname:\"Pratt\",age:10};";
        let tokens = Lexer::new(input).lex().unwrap();
        let strings = Rc::new(SymbolFactory::new());
        let mut symbols = Symbols::new(strings);
        let ast = Parser::new(tokens, &mut symbols).parse();
        assert!(ast.is_ok())
    }

    #[test]
    fn class_acess() {
        let input = "class Person {name:str,surname:str,age:int;fun hello(a:int,b:int){nil;}}
        var Louis = Person{name:\"Louis\",surname:\"Pratt\",age:10}; Louis.name;";
        let tokens = Lexer::new(input).lex().unwrap();
        let strings = Rc::new(SymbolFactory::new());
        let mut symbols = Symbols::new(strings);
        let ast = Parser::new(tokens, &mut symbols).parse();
        assert!(ast.is_ok())
    }

    #[test]
    fn class_assign() {
        let input = "class Person {name:str,surname:str,age:int;fun hello(a:int,b:int){nil;}}
        var Louis = Person{name:\"Louis\",surname:\"Pratt\",age:10}; Louis.name = 10;";
        let tokens = Lexer::new(input).lex().unwrap();
        let strings = Rc::new(SymbolFactory::new());
        let mut symbols = Symbols::new(strings);
        let ast = Parser::new(tokens, &mut symbols).parse();
        assert!(ast.is_ok())
    }

    #[test]
    fn empty_class_instance() {
        let input = "class Person {name:str,surname:str,age:int;fun hello(a:int,b:int){nil;}}
        var Louis = Person{};";
        let tokens = Lexer::new(input).lex().unwrap();
        let strings = Rc::new(SymbolFactory::new());
        let mut symbols = Symbols::new(strings);
        let ast = Parser::new(tokens, &mut symbols).parse();
        assert!(ast.is_ok())
    }

    #[test]
    fn function_expr() {
        let input = "var add = fun (a:int,b:int) -> int {a+b;};";
        let tokens = Lexer::new(input).lex().unwrap();
        let strings = Rc::new(SymbolFactory::new());
        let mut symbols = Symbols::new(strings);
        let ast = Parser::new(tokens, &mut symbols).parse();
        assert!(ast.is_ok())
    }

    #[test]
    fn function_return() {
        let input = "fun add(a:int,b:int) -> int { return a+b;}";
        let tokens = Lexer::new(input).lex().unwrap();
        let strings = Rc::new(SymbolFactory::new());
        let mut symbols = Symbols::new(strings);
        let ast = Parser::new(tokens, &mut symbols).parse();
        assert!(ast.is_ok())
    }

    #[test]
    fn class_methods() {
        let input = "class Foo {
            fun bar(a:int,b:int) -> int {
                a+b;
            }
        }";
        let tokens = Lexer::new(input).lex().unwrap();
        let strings = Rc::new(SymbolFactory::new());
        let mut symbols = Symbols::new(strings);
        let ast = Parser::new(tokens, &mut symbols).parse();
        assert!(ast.is_ok())
    }

    #[test]
    fn class_properties() {
        let input = "class Person {
            name:str;
            fun hello(){
                nil;
                }
            }";
        let tokens = Lexer::new(input).lex().unwrap();
        let strings = Rc::new(SymbolFactory::new());
        let mut symbols = Symbols::new(strings);
        let ast = Parser::new(tokens, &mut symbols).parse();
        assert!(ast.is_ok())
    }

    #[test]
    fn class_properties_multiple() {
        let input = "class Person {
            name:str,surname:str,age:int;
            fun hello(){
                nil;
                }
        }";
        let tokens = Lexer::new(input).lex().unwrap();
        let strings = Rc::new(SymbolFactory::new());
        let mut symbols = Symbols::new(strings);
        let ast = Parser::new(tokens, &mut symbols).parse();
        assert!(ast.is_ok())
    }

    #[test]
    fn do_while_statement() {
        let input = "do {print 10 ;} while (true)";
        let tokens = Lexer::new(input).lex().unwrap();
        let strings = Rc::new(SymbolFactory::new());
        let mut symbols = Symbols::new(strings);
        let ast = Parser::new(tokens, &mut symbols).parse();
        assert!(ast.is_ok())
    }

    #[test]
    fn for_statement() {
        let input = "for (var i = 0; i < 2; i = i + 1) print i;";
        let tokens = Lexer::new(input).lex().unwrap();
        let strings = Rc::new(SymbolFactory::new());
        let mut symbols = Symbols::new(strings);
        let ast = Parser::new(tokens, &mut symbols).parse();
        assert!(ast.is_ok())
    }

    #[test]
    fn class_statement() {
        let input = "class Foo {}";
        let tokens = Lexer::new(input).lex().unwrap();
        let strings = Rc::new(SymbolFactory::new());
        let mut symbols = Symbols::new(strings);
        let ast = Parser::new(tokens, &mut symbols).parse().unwrap();

        let name = Symbol(2);

        let expected = WithPos::new(
            Statement::Class {
                name,
                methods: vec![],
                properties: vec![],
            },
            Postition {
                line: 1,
                column: 1,
                absolute: 0,
            },
        );

        assert_eq!(ast, vec![expected]);
    }

    #[test]
    fn while_statement() {
        let input = "while (!true) {
            print \"true\";
            break;continue;
            }";
        let tokens = Lexer::new(input).lex().unwrap();
        let strings = Rc::new(SymbolFactory::new());

        let mut symbols = Symbols::new(strings);
        let ast = Parser::new(tokens, &mut symbols).parse();
         assert!(ast.is_ok())
    }

    #[test]
    fn if_stmt() {
        let input = "if (true) {}";

        let tokens = Lexer::new(input).lex().unwrap();
        let strings = Rc::new(SymbolFactory::new());

        let mut symbols = Symbols::new(strings);
        let ast = Parser::new(tokens, &mut symbols).parse().unwrap();

        let expected = WithPos::new(
            Statement::IfStmt {
                condition: WithPos::new(
                    Expression::Literal(Literal::True(true)),
                    Postition {
                        line: 1,
                        column: 5,
                        absolute: 4,
                    },
                ),
                then_branch: Box::new(WithPos::new(
                    Statement::Block(vec![]),
                    Postition {
                        line: 1,
                        column: 11,
                        absolute: 10,
                    },
                )),
                else_branch: None,
            },
            Postition {
                line: 1,
                column: 1,
                absolute: 0,
            },
        );

        assert_eq!(ast, vec![expected]);
    }

    #[test]
    fn block() {
        let input = "{}";

        let tokens = Lexer::new(input).lex().unwrap();
        let strings = Rc::new(SymbolFactory::new());

        let mut symbols = Symbols::new(strings);
        let ast = Parser::new(tokens, &mut symbols).parse().unwrap();

        let expected = WithPos::new(
            Statement::Block(vec![]),
            Postition {
                line: 1,
                column: 1,
                absolute: 0,
            },
        );

        assert_eq!(ast, vec![expected]);
    }

    #[test]
    fn array() {
        let input = "[10,12,13];";

        let tokens = Lexer::new(input).lex().unwrap();
        let strings = Rc::new(SymbolFactory::new());

        let mut symbols = Symbols::new(strings);
        let ast = Parser::new(tokens, &mut symbols).parse().unwrap();

        let array = WithPos::new(
            Expression::Array {
                items: vec![
                    WithPos::new(
                        Expression::Literal(Literal::Int(10)),
                        Postition {
                            line: 1,
                            column: 2,
                            absolute: 1,
                        },
                    ),
                    WithPos::new(
                        Expression::Literal(Literal::Int(12)),
                        Postition {
                            line: 1,
                            column: 5,
                            absolute: 4,
                        },
                    ),
                    WithPos::new(
                        Expression::Literal(Literal::Int(13)),
                        Postition {
                            line: 1,
                            column: 8,
                            absolute: 7,
                        },
                    ),
                ],
            },
            Postition {
                line: 1,
                column: 1,
                absolute: 0,
            },
        );

        let expected = WithPos::new(
            Statement::ExpressionStmt(array),
            Postition {
                line: 1,
                column: 11,
                absolute: 10,
            },
        );

        assert_eq!(ast, vec![expected]);
    }

    #[test]
    fn indexing() {
        let input = "a[2+1];";

        let tokens = Lexer::new(input).lex().unwrap();
        let strings = Rc::new(SymbolFactory::new());

        let mut symbols = Symbols::new(strings);
        let ast = Parser::new(tokens, &mut symbols).parse().unwrap();

        let index = WithPos::new(
            Expression::IndexExpr {
                target: Box::new(WithPos::new(
                    Expression::Var(Symbol(2), VariableUseHandle(0)),
                    Postition {
                        line: 1,
                        column: 1,
                        absolute: 0,
                    },
                )),
                index: Box::new(WithPos::new(
                    Expression::Binary {
                        left_expr: Box::new(WithPos::new(
                            Expression::Literal(Literal::Int(2)),
                            Postition {
                                line: 1,
                                column: 3,
                                absolute: 2,
                            },
                        )),
                        operator: Operator::Plus,
                        right_expr: Box::new(WithPos::new(
                            Expression::Literal(Literal::Int(1)),
                            Postition {
                                line: 1,
                                column: 5,
                                absolute: 4,
                            },
                        )),
                    },
                    Postition {
                        line: 1,
                        column: 4,
                        absolute: 3,
                    },
                )),
            },
            Postition {
                line: 1,
                column: 6,
                absolute: 5,
            },
        );

        let expected = WithPos::new(
            Statement::ExpressionStmt(index),
            Postition {
                line: 1,
                column: 7,
                absolute: 6,
            },
        );

        assert_eq!(ast, vec![expected]);
    }

    #[test]
    fn print() {
        let input = "print 9+9 ;";

        let tokens = Lexer::new(input).lex().unwrap();
        let strings = Rc::new(SymbolFactory::new());

        let mut symbols = Symbols::new(strings);
        let ast = Parser::new(tokens, &mut symbols).parse().unwrap();

        let expr = WithPos::new(
            Expression::Binary {
                left_expr: Box::new(WithPos::new(
                    Expression::Literal(Literal::Int(9)),
                    Postition {
                        line: 1,
                        column: 7,
                        absolute: 6,
                    },
                )),
                operator: Operator::Plus,
                right_expr: Box::new(WithPos::new(
                    Expression::Literal(Literal::Int(9)),
                    Postition {
                        line: 1,
                        column: 9,
                        absolute: 8,
                    },
                )),
            },
            Postition {
                line: 1,
                column: 8,
                absolute: 7,
            },
        );

        let expected = WithPos::new(
            Statement::Print(expr),
            Postition {
                line: 1,
                column: 1,
                absolute: 0,
            },
        );
        assert_eq!(ast, vec![expected]);
    }

    #[test]
    fn literal() {
        let input = "123;";
        let strings = Rc::new(SymbolFactory::new());

        let tokens = Lexer::new(input).lex().unwrap();

        let mut symbols = Symbols::new(strings);
        let ast = Parser::new(tokens, &mut symbols).parse().unwrap();

        let expected = vec![
            WithPos::new(
                Statement::ExpressionStmt(WithPos::new(
                    Expression::Literal(Literal::Int(123)),
                    Postition {
                        line: 1,
                        column: 1,
                        absolute: 0,
                    },
                )),
                Postition {
                    line: 1,
                    column: 4,
                    absolute: 3,
                },
            ),
        ];

        assert_eq!(expected, ast);
    }

    #[test]
    fn unclosed_group() {
        let input = "(123";
        let tokens = Lexer::new(input).lex().unwrap();
        let strings = Rc::new(SymbolFactory::new());

        let mut symbols = Symbols::new(strings);
        let ast = Parser::new(tokens, &mut symbols).parse();
        assert!(ast.is_err());
    }

    #[test]
    fn unary_with_no_operand() {
        let input = "-<5";
        let strings = Rc::new(SymbolFactory::new());

        let tokens = Lexer::new(input).lex().unwrap();

        let mut symbols = Symbols::new(strings);
        let ast = Parser::new(tokens, &mut symbols).parse();

        assert!(ast.is_err());
    }

    #[test]
    fn brackets() {
        let input = "-123*(45.67);";

        let tokens = Lexer::new(input).lex().unwrap();
        let strings = Rc::new(SymbolFactory::new());

        let mut symbols = Symbols::new(strings);
        let ast = Parser::new(tokens, &mut symbols).parse().unwrap();

        let expected = vec![
            WithPos::new(
                Statement::ExpressionStmt(WithPos::new(
                    Expression::Binary {
                        left_expr: Box::new(WithPos::new(
                            Expression::Unary {
                                operator: UnaryOperator::Minus,
                                expr: Box::new(WithPos::new(
                                    Expression::Literal(Literal::Int(123)),
                                    Postition {
                                        line: 1,
                                        column: 2,
                                        absolute: 1,
                                    },
                                )),
                            },
                            Postition {
                                line: 1,
                                column: 1,
                                absolute: 0,
                            },
                        )),
                        operator: Operator::Star,
                        right_expr: Box::new(WithPos::new(
                            Expression::Grouping {
                                expr: Box::new(WithPos::new(
                                    Expression::Literal(Literal::Float(45.67)),
                                    Postition {
                                        line: 1,
                                        column: 7,
                                        absolute: 6,
                                    },
                                )),
                            },
                            Postition {
                                line: 1,
                                column: 12,
                                absolute: 11,
                            },
                        )),
                    },
                    Postition {
                        line: 1,
                        column: 5,
                        absolute: 4,
                    },
                )),
                Postition {
                    line: 1,
                    column: 13,
                    absolute: 12,
                },
            ),
        ];

        assert_eq!(expected, ast);
    }

    #[test]
    fn precedence_add_mul() {
        let input = "123+456*789;";
        let strings = Rc::new(SymbolFactory::new());

        let tokens = Lexer::new(input).lex().unwrap();

        let mut symbols = Symbols::new(strings);
        let ast = Parser::new(tokens, &mut symbols).parse().unwrap();

        let expected = vec![
            WithPos {
                node: Statement::ExpressionStmt(WithPos {
                    node: Expression::Binary {
                        left_expr: Box::new(WithPos {
                            node: Expression::Literal(Literal::Int(123)),
                            pos: Postition {
                                line: 1,
                                column: 1,
                                absolute: 0,
                            },
                        }),
                        operator: Operator::Plus,
                        right_expr: Box::new(WithPos {
                            node: Expression::Binary {
                                left_expr: Box::new(WithPos {
                                    node: Expression::Literal(Literal::Int(456)),
                                    pos: Postition {
                                        line: 1,
                                        column: 5,
                                        absolute: 4,
                                    },
                                }),
                                operator: Operator::Star,
                                right_expr: Box::new(WithPos {
                                    node: Expression::Literal(Literal::Int(789)),
                                    pos: Postition {
                                        line: 1,
                                        column: 9,
                                        absolute: 8,
                                    },
                                }),
                            },
                            pos: Postition {
                                line: 1,
                                column: 8,
                                absolute: 7,
                            },
                        }),
                    },
                    pos: Postition {
                        line: 1,
                        column: 4,
                        absolute: 3,
                    },
                }),
                pos: Postition {
                    line: 1,
                    column: 12,
                    absolute: 11,
                },
            },
        ];

        assert_eq!(expected, ast);
    }

    #[test]
    fn precedence_group() {
        let input = "123+(45.76*789-3);";
        let strings = Rc::new(SymbolFactory::new());

        let tokens = Lexer::new(input).lex().unwrap();

        let mut symbols = Symbols::new(strings);
        let ast = Parser::new(tokens, &mut symbols).parse().unwrap();

        let expected = vec![
            WithPos::new(
                Statement::ExpressionStmt(WithPos::new(
                    Expression::Binary {
                        left_expr: Box::new(WithPos::new(
                            Expression::Literal(Literal::Int(123)),
                            Postition {
                                line: 1,
                                column: 1,
                                absolute: 0,
                            },
                        )),
                        operator: Operator::Plus,
                        right_expr: Box::new(WithPos::new(
                            Expression::Grouping {
                                expr: Box::new(WithPos::new(
                                    Expression::Binary {
                                        left_expr: Box::new(WithPos::new(
                                            Expression::Binary {
                                                left_expr: Box::new(WithPos::new(
                                                    Expression::Literal(Literal::Float(45.76)),
                                                    Postition {
                                                        line: 1,
                                                        column: 6,
                                                        absolute: 5,
                                                    },
                                                )),
                                                operator: Operator::Star,
                                                right_expr: Box::new(WithPos::new(
                                                    Expression::Literal(Literal::Int(789)),
                                                    Postition {
                                                        line: 1,
                                                        column: 12,
                                                        absolute: 11,
                                                    },
                                                )),
                                            },
                                            Postition {
                                                line: 1,
                                                column: 11,
                                                absolute: 10,
                                            },
                                        )),
                                        operator: Operator::Minus,
                                        right_expr: Box::new(WithPos::new(
                                            Expression::Literal(Literal::Int(3)),
                                            Postition {
                                                line: 1,
                                                column: 16,
                                                absolute: 15,
                                            },
                                        )),
                                    },
                                    Postition {
                                        line: 1,
                                        column: 15,
                                        absolute: 14,
                                    },
                                )),
                            },
                            Postition {
                                line: 1,
                                column: 17,
                                absolute: 16,
                            },
                        )),
                    },
                    Postition {
                        line: 1,
                        column: 4,
                        absolute: 3,
                    },
                )),
                Postition {
                    line: 1,
                    column: 18,
                    absolute: 17,
                },
            ),
        ];

        assert_eq!(expected, ast);
    }

    #[test]
    fn precedence_mul_add() {
        let input = "123*456+789;";

        let tokens = Lexer::new(input).lex().unwrap();
        let strings = Rc::new(SymbolFactory::new());

        let mut symbols = Symbols::new(strings);
        let ast = Parser::new(tokens, &mut symbols).parse().unwrap();

        let expected = vec![
            WithPos::new(
                Statement::ExpressionStmt(WithPos::new(
                    Expression::Binary {
                        left_expr: Box::new(WithPos::new(
                            Expression::Binary {
                                left_expr: Box::new(WithPos::new(
                                    Expression::Literal(Literal::Int(123)),
                                    Postition {
                                        line: 1,
                                        column: 1,
                                        absolute: 0,
                                    },
                                )),
                                operator: Operator::Star,
                                right_expr: Box::new(WithPos::new(
                                    Expression::Literal(Literal::Int(456)),
                                    Postition {
                                        line: 1,
                                        column: 5,
                                        absolute: 4,
                                    },
                                )),
                            },
                            Postition {
                                line: 1,
                                column: 4,
                                absolute: 3,
                            },
                        )),
                        operator: Operator::Plus,
                        right_expr: Box::new(WithPos::new(
                            Expression::Literal(Literal::Int(789)),
                            Postition {
                                line: 1,
                                column: 9,
                                absolute: 8,
                            },
                        )),
                    },
                    Postition {
                        line: 1,
                        column: 8,
                        absolute: 7,
                    },
                )),
                Postition {
                    line: 1,
                    column: 12,
                    absolute: 11,
                },
            ),
        ];

        assert_eq!(expected, ast);
    }

    #[test]
    fn precedence_mul_mul() {
        let input = "123*456*789;";

        let tokens = Lexer::new(input).lex().unwrap();
        let strings = Rc::new(SymbolFactory::new());

        let mut symbols = Symbols::new(strings);
        let ast = Parser::new(tokens, &mut symbols).parse().unwrap();

        let expected = vec![
            WithPos::new(
                Statement::ExpressionStmt(WithPos::new(
                    Expression::Binary {
                        left_expr: Box::new(WithPos::new(
                            Expression::Binary {
                                left_expr: Box::new(WithPos::new(
                                    Expression::Literal(Literal::Int(123)),
                                    Postition {
                                        line: 1,
                                        column: 1,
                                        absolute: 0,
                                    },
                                )),
                                operator: Operator::Star,
                                right_expr: Box::new(WithPos::new(
                                    Expression::Literal(Literal::Int(456)),
                                    Postition {
                                        line: 1,
                                        column: 5,
                                        absolute: 4,
                                    },
                                )),
                            },
                            Postition {
                                line: 1,
                                column: 4,
                                absolute: 3,
                            },
                        )),
                        operator: Operator::Star,
                        right_expr: Box::new(WithPos::new(
                            Expression::Literal(Literal::Int(789)),
                            Postition {
                                line: 1,
                                column: 9,
                                absolute: 8,
                            },
                        )),
                    },
                    Postition {
                        line: 1,
                        column: 8,
                        absolute: 7,
                    },
                )),
                Postition {
                    line: 1,
                    column: 12,
                    absolute: 11,
                },
            ),
        ];

        assert_eq!(expected, ast);
    }

    #[test]
    fn precedence_mul_add_unary() {
        let input = "-123*456+789;";
        let tokens = Lexer::new(input).lex().unwrap();
        let strings = Rc::new(SymbolFactory::new());

        let mut symbols = Symbols::new(strings);
        let ast = Parser::new(tokens, &mut symbols).parse().unwrap();

        let expected = vec![
            WithPos {
                node: Statement::ExpressionStmt(WithPos {
                    node: Expression::Binary {
                        left_expr: Box::new(WithPos {
                            node: Expression::Binary {
                                left_expr: Box::new(WithPos {
                                    node: Expression::Unary {
                                        operator: UnaryOperator::Minus,
                                        expr: Box::new(WithPos {
                                            node: Expression::Literal(Literal::Int(123)),
                                            pos: Postition {
                                                line: 1,
                                                column: 2,
                                                absolute: 1,
                                            },
                                        }),
                                    },
                                    pos: Postition {
                                        line: 1,
                                        column: 1,
                                        absolute: 0,
                                    },
                                }),
                                operator: Operator::Star,
                                right_expr: Box::new(WithPos {
                                    node: Expression::Literal(Literal::Int(456)),
                                    pos: Postition {
                                        line: 1,
                                        column: 6,
                                        absolute: 5,
                                    },
                                }),
                            },
                            pos: Postition {
                                line: 1,
                                column: 5,
                                absolute: 4,
                            },
                        }),
                        operator: Operator::Plus,
                        right_expr: Box::new(WithPos {
                            node: Expression::Literal(Literal::Int(789)),
                            pos: Postition {
                                line: 1,
                                column: 10,
                                absolute: 9,
                            },
                        }),
                    },
                    pos: Postition {
                        line: 1,
                        column: 9,
                        absolute: 8,
                    },
                }),
                pos: Postition {
                    line: 1,
                    column: 13,
                    absolute: 12,
                },
            },
        ];

        assert_eq!(expected, ast);
    }

}