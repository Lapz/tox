
#[cfg(test)]
mod test {
    use lexer::Lexer;
    use parser::Parser;
    use pprint::PrettyPrint;
    use ast::statements::*;
    use ast::expression::*;

    #[test]
    fn for_statement() {
        let input = "for (var i = 0; i < 2; i = i + 1)print(i);";
        let (tokens, _) = Lexer::new(input).get_tokens();

        let parsed = Parser::new(tokens).parse().unwrap();


        let expected = Statement::Block(vec![
            Statement::Var(Variable("i".to_owned()), Expression::Literal(Literal::Number(0.0))),
            Statement::WhileStmt(Box::new(While {
                condition: Expression::Binary(Box::new(Binary {
                    left_expr: Expression::Var(Variable("i".to_owned()), VariableUseHandle(0)),
                    operator: Operator::LessThan,
                    right_expr: Expression::Literal(Literal::Number(2.0)),
                })),
                body: Statement::Block(vec![
                    Statement::ExpressionStmt(Expression::Call(Box::new(Call {
                        callee: Expression::Var(Variable("print".to_owned()), VariableUseHandle(4)),
                        arguments: vec![
                            Expression::Var(Variable("i".to_owned()), VariableUseHandle(5)),
                        ],
                    }))),
                    Statement::ExpressionStmt(Expression::Assign(Box::new(Assign {
                        handle: VariableUseHandle(3),
                        name: Variable("i".to_owned()),
                        kind: AssignOperator::Equal,
                        value: Expression::Binary(Box::new(Binary {
                            left_expr: Expression::Var(
                                Variable("i".to_owned()),
                                VariableUseHandle(2),
                            ),
                            operator: Operator::Plus,
                            right_expr: Expression::Literal(Literal::Number(1.0)),
                        })),
                    }))),
                ]),
            })),
        ]);

        assert_eq!(parsed, vec![expected]);
    }

    #[test]
    fn break_statement() {
        let input = "while (!true) {
            print(\"true\");
            break;
            }";
        let (tokens, _) = Lexer::new(input).get_tokens();

        let parsed = Parser::new(tokens).parse().unwrap();

        let expected = Statement::WhileStmt(Box::new(While {
            condition: Expression::Unary(Box::new(Unary {
                operator: UnaryOperator::Bang,
                expr: Expression::Literal(Literal::True(true)),
            })),
            body: Statement::Block(vec![
                Statement::ExpressionStmt(Expression::Call(Box::new(Call {
                    callee: Expression::Var(Variable("print".to_owned()), VariableUseHandle(0)),
                    arguments: vec![Expression::Literal(Literal::Str("true".to_owned()))],
                }))),
                Statement::Break,
            ]),
        }));

        assert_eq!(parsed, vec![expected]);
    }

    #[test]
    fn while_statement() {
        let input = "while (!true) {
            print(\"true\");
            }";
        let (tokens, _) = Lexer::new(input).get_tokens();

        let parsed = Parser::new(tokens).parse().unwrap();

        let expected = Statement::WhileStmt(Box::new(While {
            condition: Expression::Unary(Box::new(Unary {
                operator: UnaryOperator::Bang,
                expr: Expression::Literal(Literal::True(true)),
            })),
            body: Statement::Block(vec![
                Statement::ExpressionStmt(Expression::Call(Box::new(Call {
                    callee: Expression::Var(Variable("print".to_owned()), VariableUseHandle(0)),
                    arguments: vec![Expression::Literal(Literal::Str("true".to_owned()))],
                }))),
            ]),
        }));

        assert_eq!(parsed, vec![expected]);
    }


    #[test]
    fn pprint() {
        let input = "pprint 9+9 ;";

        let (tokens, _) = Lexer::new(input).get_tokens();

        let parsed = Parser::new(tokens).parse().unwrap();

        let expected = Statement::PPrint(Expression::Binary(Box::new(Binary {
            left_expr: Expression::Literal(Literal::Number(9.0)),
            operator: Operator::Plus,
            right_expr: Expression::Literal(Literal::Number(9.0)),
        })));

        assert_eq!(parsed, vec![expected]);
    }

    #[test]
    fn if_stmt() {
        let input = "
        if (10 < 5) {
            print(10);
        }";

        let (tokens, _) = Lexer::new(input).get_tokens();

        let parsed = Parser::new(tokens).parse().unwrap();



        let call = Call {
            callee: Expression::Var(Variable("print".to_owned()), VariableUseHandle(0)),
            arguments: vec![Expression::Literal(Literal::Number(10.0))],
        };


        let expected = Statement::IfStmt(Box::new(If {
            condition: Expression::Binary(Box::new(Binary {
                left_expr: Expression::Literal(Literal::Number(10.0)),
                operator: Operator::LessThan,
                right_expr: Expression::Literal(Literal::Number(5.0)),
            })),
            then_branch: Statement::Block(
                vec![Statement::ExpressionStmt(Expression::Call(Box::new(call)))],
            ),
            else_branch: None,
        }));

        assert_eq!(parsed, vec![expected]);
    }

    #[test]
    fn block() {
        let input = "{}";

        let (tokens, _) = Lexer::new(input).get_tokens();

        let parsed = Parser::new(tokens).parse().unwrap();

        let expected = Statement::Block(vec![]);

        assert_eq!(parsed, vec![expected]);
    }

    #[test]
    fn function_call() {
        let input = "clock();len(\"hello\",25);";

        let (tokens, _) = Lexer::new(input).get_tokens();

        let parsed = Parser::new(tokens).parse().unwrap();

        let clock_fun = Statement::ExpressionStmt(Expression::Call(Box::new(Call {
            callee: Expression::Var(Variable("clock".to_owned()), VariableUseHandle(0)),
            arguments: vec![],
        })));

        let len_fun = Statement::ExpressionStmt(Expression::Call(Box::new(Call {
            callee: Expression::Var(Variable("len".to_owned()), VariableUseHandle(1)),
            arguments: vec![
                Expression::Literal(Literal::Str("hello".to_owned())),
                Expression::Literal(Literal::Number(25f64)),
            ],
        })));


        assert_eq!(parsed, vec![clock_fun, len_fun]);
    }

    #[test]
    fn array() {
        let input = "[10,12,13];";

        let (tokens, _) = Lexer::new(input).get_tokens();

        let parsed = Parser::new(tokens).parse().unwrap();

        let expected = vec![
            Expression::Literal(Literal::Number(10f64)),
            Expression::Literal(Literal::Number(12f64)),
            Expression::Literal(Literal::Number(13f64)),
        ];

        assert_eq!(
            parsed,
            vec![
                Statement::ExpressionStmt(Expression::Array(Array { items: expected })),
            ]
        );
    }

    #[test]
    fn indexing() {
        let input = "a[2+1];";

        let (tokens, _) = Lexer::new(input).get_tokens();

        let parsed = Parser::new(tokens).parse().unwrap();


        let expected = IndexExpr {
            target: Expression::Var(Variable("a".to_owned()), VariableUseHandle(0)),
            index: Expression::Binary(Box::new(Binary {
                left_expr: Expression::Literal(Literal::Number(2f64)),
                operator: Operator::Plus,
                right_expr: Expression::Literal(Literal::Number(1f64)),
            })),
        };

        assert_eq!(
            parsed,
            vec![
                Statement::ExpressionStmt(Expression::IndexExpr(Box::new(expected))),
            ]
        );
    }



    #[test]
    fn print() {
        let input = "print(9+9);";

        let (tokens, _) = Lexer::new(input).get_tokens();

        let parsed = Parser::new(tokens).parse().unwrap();

        let call = Call {
            callee: Expression::Var(Variable("print".to_owned()), VariableUseHandle(0)),
            arguments: vec![
                Expression::Binary(Box::new(Binary {
                    left_expr: Expression::Literal(Literal::Number(9.0)),
                    operator: Operator::Plus,
                    right_expr: Expression::Literal(Literal::Number(9.0)),
                })),
            ],
        };



        let expected = Statement::ExpressionStmt(Expression::Call(Box::new(call)));

        assert_eq!(parsed, vec![expected]);
    }

    #[test]
    fn literal() {
        let input = "123;";

        let (tokens, _) = Lexer::new(input).get_tokens();

        let pprinted = Parser::new(tokens)
            .parse_single_expression()
            .unwrap()
            .pprint();

        assert_eq!(pprinted, " 123 ");
    }

    #[test]
    fn unclosed_group() {
        let input = "(123";
        let (tokens, _) = Lexer::new(input).get_tokens();



        let ast = Parser::new(tokens).parse();

        assert!(ast.is_err());
    }

    #[test]
    fn unary_with_no_operand() {
        let input = "-<5";

        let (tokens, _) = Lexer::new(input).get_tokens();



        let ast = Parser::new(tokens).parse();

        assert!(ast.is_err());
    }

    #[test]
    fn binary() {
        let input = "123+456;";

        let (tokens, _) = Lexer::new(input).get_tokens();

        let pprinted = Parser::new(tokens)
            .parse_single_expression()
            .unwrap()
            .pprint();

        assert_eq!(pprinted, "(+  123  456 )");
    }

    #[test]
    fn brackets() {
        let input = "-123*(45.67);";

        let (tokens, _) = Lexer::new(input).get_tokens();

        let pprinted = Parser::new(tokens)
            .parse_single_expression()
            .unwrap()
            .pprint();

        assert_eq!(pprinted, "(*(-  123 )( group 45.67 ))");
    }

    #[test]
    fn precedence_add_mul() {
        let input = "123+456*789;";

        let (tokens, _) = Lexer::new(input).get_tokens();

        let pprinted = Parser::new(tokens)
            .parse_single_expression()
            .unwrap()
            .pprint();

        assert_eq!(pprinted, "(+  123 (* 456  789 ))");
    }


    #[test]
    fn precedence_group() {
        let input = "123+(45.76*789-3);";

        let (tokens, _) = Lexer::new(input).get_tokens();

        let pprinted = Parser::new(tokens)
            .parse_single_expression()
            .unwrap()
            .pprint();

        assert_eq!(pprinted, "(+  123 ( group(- (* 45.76  789 ) 3 )))");
    }

    #[test]
    fn precedence_mul_add() {
        let input = "123*456+789;";

        let (tokens, _) = Lexer::new(input).get_tokens();

        let pprinted = Parser::new(tokens)
            .parse_single_expression()
            .unwrap()
            .pprint();

        assert_eq!(pprinted, "(+ (* 123  456 ) 789 )");
    }

    #[test]
    fn precedence_mul_mul() {
        let input = "123*456*789;";

        let (tokens, _) = Lexer::new(input).get_tokens();

        let pprinted = Parser::new(tokens)
            .parse_single_expression()
            .unwrap()
            .pprint();

        assert_eq!(pprinted, "(*(* 123  456 ) 789 )");
    }

    #[test]
    fn precedence_mul_add_unary() {
        let input = "-123*456+789;";

        let (tokens, _) = Lexer::new(input).get_tokens();




        let pprinted = Parser::new(tokens)
            .parse_single_expression()
            .unwrap()
            .pprint();

        assert_eq!(pprinted, "(+ (*(-  123 ) 456 ) 789 )");
    }



}
