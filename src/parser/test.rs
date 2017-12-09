#[cfg(test)]
mod test {
    use lexer::Lexer;
    use parser::Parser;
    use ast::expr::*;
    use ast::statement::*;
    use pos::{Postition, WithPos};

    // #[test]
    // fn for_statement() {
    //     let input = "for (var i = 0; i < 2; i = i + 1)print(i);";
    //     let tokens = Lexer::new(input).lex().unwrap();

    //     let ast = Parser::new(tokens).parse().unwrap();


    //     let expected = Statement::Block(vec![
    //         Statement::Var(Variable("i"), Expression::Literal(Literal::Int(0))),
    //         Statement::WhileStmt(Box::new(While {
    //             condition: Expression::Binary(Box::new(Binary {
    //                 left_expr: Expression::Var(Variable("i"), VariableUseHandle(0)),
    //                 operator: Operator::LessThan,
    //                 right_expr: Expression::Literal(Literal::Int(2)),
    //             })),
    //             body: Statement::Block(vec![
    //                 Statement::ExpressionStmt(Expression::Call(Box::new(Call {
    //                     callee: Expression::Var(Variable("print"), VariableUseHandle(4)),
    //                     arguments: vec![
    //                         Expression::Var(Variable("i"), VariableUseHandle(5)),
    //                     ],
    //                 }))),
    //                 Statement::ExpressionStmt(Expression::Assign(Box::new(Assign {
    //                     handle: VariableUseHandle(3),
    //                     name: Variable("i"),
    //                     kind: AssignOperator::Equal,
    //                     value: Expression::Binary(Box::new(Binary {
    //                         left_expr: Expression::Var(
    //                             Variable("i"),
    //                             VariableUseHandle(2),
    //                         ),
    //                         operator: Operator::Plus,
    //                         right_expr: Expression::Literal(Literal::Int(1)),
    //                     })),
    //                 }))),
    //             ]),
    //         })),
    //     ]);

    //     assert_eq!(ast, vec![expected]);
    // }

     #[test]
    fn class_statement() {
        let input = "class Foo {}";
        let tokens = Lexer::new(input).lex().unwrap();

        let ast = Parser::new(tokens).parse().unwrap();

        let name = Variable("Foo");
        
        let expected = WithPos::new(Statement::Class{
            name,
            methods:vec![],
        },Postition{line:1,column:1,absolute:0});

        assert_eq!(ast, vec![expected]);
    }

    

    #[test]
    fn break_statement() {
        let input = "while (!true) {
            print(\"true\");
            break;
            }";
        let tokens = Lexer::new(input).lex().unwrap();

        let ast = Parser::new(tokens).parse().unwrap();

        let call = WithPos::new(
            Statement::ExpressionStmt(Expression::Call {
                callee: Box::new(Expression::Var(Variable("print"), VariableUseHandle(0))),
                arguments: vec![Expression::Literal(Literal::Str("true".to_owned()))],
            }),
            Postition {
                line: 2,
                column: 26,
                absolute: 41,
            },
        );

        let break_statement = WithPos::new(Statement::Break,Postition{line:3,column:13,absolute:55});

        let body = WithPos::new(Statement::Block(vec![call,break_statement]),Postition{
            line:1,
            column:15,
            absolute:14
        });

        let expected = WithPos::new(Statement::WhileStmt{
            condition: Expression::Unary {
                operator: UnaryOperator::Bang,
                expr: Box::new(Expression::Literal(Literal::True(true))),
            },
            body: Box::new(body),
        },Postition{line:1,column:1,absolute:0});

        assert_eq!(ast, vec![expected]);
    }



    #[test]
    fn continue_statement() {
        let input = "while (!true) {
            print(\"true\");
            continue;
            }";
        let tokens = Lexer::new(input).lex().unwrap();

        let ast = Parser::new(tokens).parse().unwrap();

        let call = WithPos::new(
            Statement::ExpressionStmt(Expression::Call {
                callee: Box::new(Expression::Var(Variable("print"), VariableUseHandle(0))),
                arguments: vec![Expression::Literal(Literal::Str("true".to_owned()))],
            }),
            Postition {
                line: 2,
                column: 26,
                absolute: 41,
            },
        );

        let break_statement = WithPos::new(Statement::Continue,Postition{line:3,column:13,absolute:55});

        let body = WithPos::new(Statement::Block(vec![call,break_statement]),Postition{
            line:1,
            column:15,
            absolute:14
        });

        let expected = WithPos::new(Statement::WhileStmt{
            condition: Expression::Unary {
                operator: UnaryOperator::Bang,
                expr: Box::new(Expression::Literal(Literal::True(true))),
            },
            body: Box::new(body),
        },Postition{line:1,column:1,absolute:0});

        assert_eq!(ast, vec![expected]);
    }

    #[test]
    fn while_statement() {
        let input = "while (!true) {
            print(\"true\");
            }";
        let tokens = Lexer::new(input).lex().unwrap();

        let ast = Parser::new(tokens).parse().unwrap();

        let call = WithPos::new(
            Statement::ExpressionStmt(Expression::Call {
                callee: Box::new(Expression::Var(Variable("print"), VariableUseHandle(0))),
                arguments: vec![Expression::Literal(Literal::Str("true".to_owned()))],
            }),
            Postition {
                line: 2,
                column: 26,
                absolute: 41,
            },
        );

        let body = WithPos::new(Statement::Block(vec![call]),Postition{
            line:1,
            column:15,
            absolute:14
        });

        let expected = WithPos::new(Statement::WhileStmt{
            condition: Expression::Unary {
                operator: UnaryOperator::Bang,
                expr: Box::new(Expression::Literal(Literal::True(true))),
            },
            body: Box::new(body),
        },Postition{line:1,column:1,absolute:0});

        assert_eq!(ast, vec![expected]);
    }


    #[test]
    fn if_stmt() {
        let input = "if (10 < 5) {
            print(10);
        }";

        let tokens = Lexer::new(input).lex().unwrap();

        let ast = Parser::new(tokens).parse().unwrap();

        let call = Expression::Call {
            callee: Box::new(Expression::Var(Variable("print"), VariableUseHandle(0))),
            arguments: vec![Expression::Literal(Literal::Int(10))],
        };

        let expected = WithPos::new(
            Statement::IfStmt {
                condition: Expression::Binary {
                    left_expr: Box::new(Expression::Literal(Literal::Int(10))),
                    operator: Operator::LessThan,
                    right_expr: Box::new(Expression::Literal(Literal::Int(5))),
                },
                then_branch: Box::new(WithPos::new(
                    Statement::Block(vec![
                        WithPos::new(
                            Statement::ExpressionStmt(call),
                            Postition {
                                line: 2,
                                column: 22,
                                absolute: 35,
                            },
                        ),
                    ]),
                    Postition {
                        line: 1,
                        column: 13,
                        absolute: 12,
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

        let ast = Parser::new(tokens).parse().unwrap();

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
    fn function_call() {
        let input = "clock();len(\"hello\",25);";

        let tokens = Lexer::new(input).lex().unwrap();

        let ast = Parser::new(tokens).parse().unwrap();

        let clock_fun = WithPos::new(
            Statement::ExpressionStmt(Expression::Call {
                callee: Box::new(Expression::Var(Variable("clock"), VariableUseHandle(0))),
                arguments: vec![],
            }),
            Postition {
                line: 1,
                column: 8,
                absolute: 7,
            },
        );

        let len_fun = WithPos::new(
            Statement::ExpressionStmt(Expression::Call {
                callee: Box::new(Expression::Var(Variable("len"), VariableUseHandle(1))),
                arguments: vec![
                    Expression::Literal(Literal::Str("hello".to_owned())),
                    Expression::Literal(Literal::Int(25)),
                ],
            }),
            Postition {
                line: 1,
                column: 24,
                absolute: 23,
            },
        );


        assert_eq!(ast, vec![clock_fun, len_fun]);
    }

    #[test]
    fn array() {
        let input = "[10,12,13];";

        let tokens = Lexer::new(input).lex().unwrap();

        let ast = Parser::new(tokens).parse().unwrap();

        let array = Expression::Array {
            items: vec![
                Expression::Literal(Literal::Int(10)),
                Expression::Literal(Literal::Int(12)),
                Expression::Literal(Literal::Int(13)),
            ],
        };

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

        let ast = Parser::new(tokens).parse().unwrap();


        let index = Expression::IndexExpr {
            target: Box::new(Expression::Var(Variable("a"), VariableUseHandle(0))),
            index: Box::new(Expression::Binary {
                left_expr: Box::new(Expression::Literal(Literal::Int(2))),
                operator: Operator::Plus,
                right_expr: Box::new(Expression::Literal(Literal::Int(1))),
            }),
        };

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
        let input = "print(9+9);";

        let tokens = Lexer::new(input).lex().unwrap();

        let ast = Parser::new(tokens).parse().unwrap();

        let call = Expression::Call {
            callee: Box::new(Expression::Var(Variable("print"), VariableUseHandle(0))),
            arguments: vec![
                Expression::Binary {
                    left_expr: Box::new(Expression::Literal(Literal::Int(9))),
                    operator: Operator::Plus,
                    right_expr: Box::new(Expression::Literal(Literal::Int(9))),
                },
            ],
        };

        let expected = WithPos::new(
            Statement::ExpressionStmt(call),
            Postition {
                line: 1,
                column: 11,
                absolute: 10,
            },
        );
        assert_eq!(ast, vec![expected]);
    }

    #[test]
    fn literal() {
        let input = "123;";

        let tokens = Lexer::new(input).lex().unwrap();

        let ast = Parser::new(tokens).parse().unwrap();

        let expected = vec![
            WithPos::new(
                Statement::ExpressionStmt(Expression::Literal(Literal::Int(123))),
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
        let ast = Parser::new(tokens).parse();
        assert!(ast.is_err());
    }

    #[test]
    fn unary_with_no_operand() {
        let input = "-<5";

        let tokens = Lexer::new(input).lex().unwrap();

        let ast = Parser::new(tokens).parse();

        assert!(ast.is_err());
    }

    #[test]
    fn binary() {
        let input = "123+456;";

        let tokens = Lexer::new(input).lex().unwrap();

        let ast = Parser::new(tokens).parse().unwrap();

        let expected = vec![
            WithPos::new(
                Statement::ExpressionStmt(Expression::Binary {
                    left_expr: Box::new(Expression::Literal(Literal::Int(123))),
                    operator: Operator::Plus,
                    right_expr: Box::new(Expression::Literal(Literal::Int(456))),
                }),
                Postition {
                    line: 1,
                    column: 8,
                    absolute: 7,
                },
            ),
        ];

        assert_eq!(expected, ast);
    }

    #[test]
    fn brackets() {
        let input = "-123*(45.67);";

        let tokens = Lexer::new(input).lex().unwrap();

        let ast = Parser::new(tokens).parse().unwrap();

        let expected = vec![
            WithPos::new(
                Statement::ExpressionStmt(Expression::Binary {
                    left_expr: Box::new(Expression::Unary {
                        operator: UnaryOperator::Minus,
                        expr: Box::new(Expression::Literal(Literal::Int(123))),
                    }),
                    operator: Operator::Star,
                    right_expr: Box::new(Expression::Grouping {
                        expr: Box::new(Expression::Literal(Literal::Float(45.67))),
                    }),
                }),
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

        let tokens = Lexer::new(input).lex().unwrap();

        let ast = Parser::new(tokens).parse().unwrap();

        let expected = vec![
            WithPos::new(
                Statement::ExpressionStmt(Expression::Binary {
                    left_expr: Box::new(Expression::Literal(Literal::Int(123))),
                    operator: Operator::Plus,
                    right_expr: Box::new(Expression::Binary {
                        left_expr: Box::new(Expression::Literal(Literal::Int(456))),
                        operator: Operator::Star,
                        right_expr: Box::new(Expression::Literal(Literal::Int(789))),
                    }),
                }),
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
    fn precedence_group() {
        let input = "123+(45.76*789-3);";

        let tokens = Lexer::new(input).lex().unwrap();

        let ast = Parser::new(tokens).parse().unwrap();

        let expected = vec![
            WithPos::new(
                Statement::ExpressionStmt(Expression::Binary {
                    left_expr: Box::new(Expression::Literal(Literal::Int(123))),
                    operator: Operator::Plus,
                    right_expr: Box::new(Expression::Grouping {
                        expr: Box::new(Expression::Binary {
                            left_expr: Box::new(Expression::Binary {
                                left_expr: Box::new(Expression::Literal(Literal::Float(45.76))),
                                operator: Operator::Star,
                                right_expr: Box::new(Expression::Literal(Literal::Int(789))),
                            }),
                            operator: Operator::Minus,
                            right_expr: Box::new(Expression::Literal(Literal::Int(3))),
                        }),
                    }),
                }),
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

        let ast = Parser::new(tokens).parse().unwrap();

        let expected = vec![
            WithPos::new(
                Statement::ExpressionStmt(Expression::Binary {
                    left_expr: Box::new(Expression::Binary {
                        left_expr: Box::new(Expression::Literal(Literal::Int(123))),
                        operator: Operator::Star,
                        right_expr: Box::new(Expression::Literal(Literal::Int(456))),
                    }),
                    operator: Operator::Plus,
                    right_expr: Box::new(Expression::Literal(Literal::Int(789))),
                }),
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

        let ast = Parser::new(tokens).parse().unwrap();

        let expected = vec![
            WithPos::new(
                Statement::ExpressionStmt(Expression::Binary {
                    left_expr: Box::new(Expression::Binary {
                        left_expr: Box::new(Expression::Literal(Literal::Int(123))),
                        operator: Operator::Star,
                        right_expr: Box::new(Expression::Literal(Literal::Int(456))),
                    }),
                    operator: Operator::Star,
                    right_expr: Box::new(Expression::Literal(Literal::Int(789))),
                }),
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

        let ast = Parser::new(tokens).parse().unwrap();


        let expected = vec![
            WithPos::new(
                Statement::ExpressionStmt(Expression::Binary {
                    left_expr: Box::new(Expression::Binary {
                        left_expr: Box::new(Expression::Unary {
                            operator: UnaryOperator::Minus,
                            expr: Box::new(Expression::Literal(Literal::Int(123))),
                        }),
                        operator: Operator::Star,
                        right_expr: Box::new(Expression::Literal(Literal::Int(456))),
                    }),
                    operator: Operator::Plus,
                    right_expr: Box::new(Expression::Literal(Literal::Int(789))),
                }),
                Postition {
                    line: 1,
                    column: 13,
                    absolute: 12,
                },
            ),
        ];

        assert_eq!(expected, ast);
    }

}
