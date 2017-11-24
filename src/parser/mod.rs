mod test;
use token::{Token, TokenType};
use std::vec::IntoIter;
use std::iter::Peekable;
use errors::parser::ParserError;
use ast::expression::*;
use ast::statements::*;

#[derive(Clone, Debug)]
pub struct Parser<'a> {
    tokens: Peekable<IntoIter<Token<'a>>>,
    loop_depth: i32,
    variable_use_maker: VariableUseMaker,
}

fn error_with_line(message: &str, line: &i32, column: &i32) -> String {
    format!("{} on line {} column {} ", message, line, column)
}

impl <'a> Parser <'a> {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens: tokens.into_iter().peekable(),
            loop_depth: 0,
            variable_use_maker: VariableUseMaker::new(),
        }
    }


    pub fn parse_single_expression(&mut self) -> Result<Expression, ParserError> {
        match self.expression() {
            Ok(expression) => Ok(expression),
            Err(e) => Err(e),
        }
    }


    // Functions that are helper methods with the parser
    fn peek_check(&mut self, token_to_check: TokenType) -> bool {
        match self.tokens.peek() {
            Some(&Token { ref token, .. }) => {
                if token == &token_to_check {
                    return true;
                }

                false
            },
            None => false,
        }
    }

    fn advance_check(
        &mut self,
        token_to_check: TokenType,
        message: &str,
    ) -> Result<(), ParserError> {
        match self.tokens.next().map(|t| t) {
            Some(Token {
                ref token,
                ref column,
                ref line,
            }) => {
                if token == &token_to_check {
                    return Ok(());
                }

                Err(ParserError::Expected(
                    format!("{} on line {}, column {}", message.to_string(), line, column),
                ))
            },

            None => Err(ParserError::EOF(format!("Expected {:?} but found EOF", token_to_check))),
        }
    }





    pub fn parse(&mut self) -> Result<Vec<Statement>, Vec<ParserError>> {
        let mut statements = vec![];

        let mut errors = vec![];

        while self.tokens.peek() != None {
            match self.declaration() {
                Ok(expression) => statements.push(expression),
                Err(e) => {
                    match self.synchronize() {
                        Ok(()) => (),
                        Err(e) => errors.push(e),
                    }
                    errors.push(e)
                },
            }
        }

        if errors.is_empty() {
            Ok(statements)
        } else {
            Err(errors)
        }
    }




    // Parsing statements
    fn declaration(&mut self) -> Result<Statement, ParserError> {
        match self.tokens.peek().map(|t| &t.token) {
            Some(&TokenType::CLASS) => {
                self.tokens.next(); // Consumes the CLASS;
                self.class_declaration()
            },
            Some(&TokenType::FUNCTION) => {
                self.tokens.next(); // Consumes the FUNCTION;
                self.function("function")
            },
            Some(&TokenType::VAR) => {
                self.tokens.next(); // Consumes the VAR
                self.var_declaration()
            },

            Some(_) => self.statement(),
            None => {
                let error = "Expected a statement or a variable decleration but instead an \
                             unexpected EOF"
                    .to_string();
                Err(ParserError::EOF(error))
            },
        }
    }

    fn statement(&mut self) -> Result<Statement, ParserError> {
        match self.tokens.peek().map(|t| &t.token) {
            Some(&TokenType::BREAK) => {
                self.tokens.next();
                self.break_statement()
            },

            Some(&TokenType::CONTINUE) => {
                self.tokens.next();
                self.continue_statement()
            },

            Some(&TokenType::RETURN) => {
                self.tokens.next(); // Consumes the return
                self.return_statement()
            },

            Some(&TokenType::PPRINT) => {
                self.tokens.next(); // Consumes the print
                self.pprint_statement()
            },

            Some(&TokenType::IF) => {
                self.tokens.next(); // Consumes the if
                self.if_statement()
            },

            Some(&TokenType::DO) => {
                self.tokens.next();
                self.do_statement()
            },

            Some(&TokenType::WHILE) => {
                self.tokens.next(); // Consumes the while
                self.while_statement()
            },

            Some(&TokenType::FOR) => {
                self.tokens.next(); // Consumes the for
                self.for_statement()
            },

            Some(&TokenType::LBRACE) => {
                self.tokens.next(); // Consumes the {
                let statements = self.block()?;

                Ok(Statement::Block(statements))
            },

            Some(_) => self.expression_statement(),

            None => Err(ParserError::EOF("Unexpected end of file".to_string())),
        }
    }

    fn class_declaration(&mut self) -> Result<Statement, ParserError> {
        let name = match self.tokens.next() {
            Some(Token {
                token: TokenType::IDENTIFIER(ref ident),
                ..
            }) => Variable(ident.to_owned()),

            Some(Token {
                ref line,
                ref column,
                ..
            }) => {
                return Err(ParserError::Expected(
                    format!("Expected a class name on line {} column {}", line, column),
                ))
            },

            None => {
                let error = "Expected a class name but instead found an EOF".to_owned();
                return Err(ParserError::EOF(error));
            },
        };
        self.advance_check(TokenType::LBRACE, "Expect '{' before class body")?;
        let mut methods = vec![];

        while !self.peek_check(TokenType::RBRACE) && !(self.tokens.peek() == None) {
            methods.push(self.function("method")?);
        }
        self.advance_check(TokenType::RBRACE, "Expect '}' after class body")?;

        Ok(Statement::Class(Class { name, methods }))
    }

    fn function(&mut self, kind: &str) -> Result<Statement, ParserError> {
        let name = match self.tokens.next() {
            Some(Token {
                token: TokenType::IDENTIFIER(ref ident),
                ..
            }) => Variable(ident.to_owned()),

            Some(Token {
                ref line,
                ref column,
                ..
            }) => {
                return Err(ParserError::Expected(
                    format!("Expected a {} name on line { } column {}", kind, line, column),
                ))
            },

            None => {
                let error = "Expected a variable name but instead found an EOF".to_string();

                return Err(ParserError::EOF(error));
            },
        }; // Store the name as a variable


        Ok(Statement::Function(Box::new(Function {
            name,
            function: self.function_body(kind)?,
        })))





        //     Ok(Statement::Function(Box::new(func)))
    }


    fn function_body(&mut self, kind: &str) -> Result<Expression, ParserError> {
        self.advance_check(TokenType::LPAREN, "Expected '(' ")?;

        let mut parameters = vec![];



        if self.tokens.peek().map(|t| &t.token) != Some(&TokenType::RPAREN) {
            while {
                if parameters.len() >= 32 {
                    println!("Cannot have more than 32 arguments")
                };

                let identifier = match self.tokens.next().map(|t| t.token) {
                    Some(TokenType::IDENTIFIER(ref s)) => Variable(s.to_owned()),

                    _ => return Err(ParserError::Expected("Expected a parameter name".to_owned())),
                };


                parameters.push(identifier);



                self.tokens.peek().map(|t| &t.token) == Some(&TokenType::COMMA)
                    && self.tokens.next().map(|t| t.token) == Some(TokenType::COMMA)
            } {}
        }



        self.advance_check(TokenType::RPAREN, "Expected ')' after parameters.")?;


        self.advance_check(TokenType::LBRACE, &format!("Expected '{{' before {} body.", kind))?;

        let body = self.block()?;



        Ok(Expression::Func(Box::new(Func { parameters, body })))
    }

    fn var_declaration(&mut self) -> Result<Statement, ParserError> {
        let name = match self.tokens.next() {
            Some(Token {
                token: TokenType::IDENTIFIER(ref ident),
                ..
            }) => Variable(ident.to_owned()),
            Some(Token {
                ref line,
                ref column,
                ..
            }) => {
                return Err(ParserError::Expected(
                    error_with_line("Expected a variable name.", line, column),
                ))
            },

            None => {
                let error = "Expected a variable name but instead found an EOF".to_string();

                return Err(ParserError::EOF(error));
            },
        };


        if self.peek_check(TokenType::SEMICOLON) {
            self.tokens.next();
            let expr = Expression::Literal(Literal::Nil);

            return Ok(Statement::Var(name, expr));
        };





        let expr = match self.tokens.next() {
            Some(Token {
                token: TokenType::ASSIGN,
                ..
            }) => self.expression()?,

            Some(Token {
                ref line,
                ref column,
                ..
            }) => {
                let error = error_with_line("Expected ';' after expression.", line, column);
                return Err(ParserError::Expected(error));
            },

            None => {
                let error = "Expected an assignment or ';' but instead found EOF".to_string();

                return Err(ParserError::EOF(error));
            },
        };

        self.advance_check(TokenType::SEMICOLON, "Expect ';' after variable decleration.")?;

        Ok(Statement::Var(name, expr))
    }

    fn break_statement(&mut self) -> Result<Statement, ParserError> {
        if !(self.loop_depth >= 0) {
            let error = "Must be inside a loop to use break".to_owned();


            return Err(ParserError::Break(error));
        }

        self.advance_check(TokenType::SEMICOLON, "Expected ';' after 'break'")?;

        Ok(Statement::Break)
    }

    fn continue_statement(&mut self) -> Result<Statement, ParserError> {
        if !(self.loop_depth >= 0) {
            let error = "Must be inside a loop to use 'continue'".to_owned();


            return Err(ParserError::Break(error));
        }

        self.advance_check(TokenType::SEMICOLON, "Expected ';' after 'continue'")?;

        Ok(Statement::Continue)
    }




    fn for_statement(&mut self) -> Result<Statement, ParserError> {
        self.advance_check(TokenType::LPAREN, "Expected '(' after 'for'")?;

        let mut initializer = None;

        match self.tokens.peek().map(|t| &t.token) {
            Some(&TokenType::SEMICOLON) => {
                self.tokens.next();
                ()
            },
            Some(&TokenType::VAR) => {
                self.tokens.next();
                initializer = Some(self.var_declaration()?);
            },

            Some(_) => {
                initializer = Some(self.expression_statement()?);
            },

            None => {
                let error = "Expected a semicolon or initializer for 'for' loopbut found EOF \
                             insted"
                    .to_string();

                return Err(ParserError::EOF(error));
            },
        };


        let mut condition = None;


        if !self.peek_check(TokenType::SEMICOLON) {
            condition = Some(self.expression()?)
        }

        self.advance_check(TokenType::SEMICOLON, "Expected ';' after loop condition .")?;

        let mut increment = None;


        if !self.peek_check(TokenType::RPAREN) {
            increment = Some(self.expression()?)
        }

        self.advance_check(TokenType::RPAREN, "Expected ')' after for clauses.")?;

        self.loop_depth += 1;


        let mut body = match self.statement() {
            Ok(expression) => expression,
            Err(e) => return Err(e),
        };

        if increment != None {
            body = Statement::Block(vec![body, Statement::ExpressionStmt(increment.unwrap())])
        }

        if condition == None {
            condition = Some(Expression::Literal(Literal::True(true)));
        }

        let _while = Box::new(While {
            condition: condition.unwrap(),
            body,
        });

        body = Statement::WhileStmt(_while);



        if initializer != None {
            let mut stmts = vec![];
            stmts.push(initializer.unwrap());
            stmts.push(body);

            body = Statement::Block(stmts);
        }

        self.loop_depth -= 1;


        Ok(body)
    }

    fn do_statement(&mut self) -> Result<Statement, ParserError> {
        let do_thing = self.statement()?;

        self.advance_check(TokenType::WHILE, "Expected while after 'do' condition")?;

        self.advance_check(TokenType::LPAREN, "Expected '(' after while'")?;

        let condition = self.expression()?;

        self.advance_check(TokenType::RPAREN, "Expected ')' after 'while'")?;


        let do_statement = Box::new(Do {
            do_thing,
            condition,
        });

        Ok(Statement::DoStmt(do_statement))
    }

    fn while_statement(&mut self) -> Result<Statement, ParserError> {
        self.advance_check(TokenType::LPAREN, "Expected '(' after while'")?;

        let condition = self.expression()?;

        self.advance_check(TokenType::RPAREN, "Expected ')' after 'while'")?;

        self.loop_depth += 1;

        let body = self.statement()?;

        let while_statement = Statement::WhileStmt(Box::new(While { condition, body }));

        self.loop_depth -= 1;

        Ok(while_statement)
    }

    fn if_statement(&mut self) -> Result<Statement, ParserError> {
        self.advance_check(TokenType::LPAREN, "Expected '(' after 'if'")?;

        let condition = self.expression()?;

        self.advance_check(TokenType::RPAREN, "Expected ')' after 'if'")?;



        let then_branch = self.statement()?;

        if self.peek_check(TokenType::ELSE) {
            self.tokens.next();


            let else_branch = Some(self.statement()?);

            return Ok(Statement::IfStmt(Box::new(If {
                condition,
                then_branch,
                else_branch,
            })));
        };



        let else_branch = None;

        self.loop_depth -= 1;
        Ok(Statement::IfStmt(Box::new(If {
            condition,
            then_branch,
            else_branch,
        })))
    }


    fn return_statement(&mut self) -> Result<Statement, ParserError> {
        let mut value: Option<Expression> = None;

        if !self.peek_check(TokenType::COLON) {
            value = Some(self.expression()?)
        }

        self.advance_check(TokenType::SEMICOLON, "Expect ';' after return value")?;

        Ok(Statement::Return(value))
    }

    fn pprint_statement(&mut self) -> Result<Statement, ParserError> {
        let expr = self.expression()?;

        self.advance_check(TokenType::SEMICOLON, "Expected ';' after print statement value")?;

        Ok(Statement::PPrint(expr))
    }

    fn expression_statement(&mut self) -> Result<Statement, ParserError> {
        let expr = self.expression()?;
        self.advance_check(TokenType::SEMICOLON, "Expected ';' after an expression")?;

        Ok(Statement::ExpressionStmt(expr))
    }

    fn block(&mut self) -> Result<Vec<Statement>, ParserError> {
        let mut statements = vec![];

        while self.tokens.peek().map(|t| &t.token) != Some(&TokenType::RBRACE) {
            statements.push(self.declaration()?);
        }

        self.advance_check(TokenType::RBRACE, "Expected '}' after block")?;
        Ok(statements)
    }



    // Parsing expressions
    fn expression(&mut self) -> Result<Expression, ParserError> {
        // expression → comma
        self.assignment()
    }




    fn assignment(&mut self) -> Result<Expression, ParserError> {
        // assignment → identifier "=" assignment
        //            | logic_or ;

        let expr = self.ternary()?;


        match self.tokens.peek().map(|t| &t.token) {
            Some(&TokenType::ASSIGN) |
            Some(&TokenType::PLUSASSIGN) |
            Some(&TokenType::MINUSASSIGN) |
            Some(&TokenType::STARASSIGN) => {
                let kind = get_assign_type(&self.tokens.next().map(|t| t.token));

                let value = self.assignment()?;

                match expr {
                    Expression::Var(variable, _) => Ok(Expression::Assign(Box::new(Assign {
                        handle: self.variable_use_maker.next(),
                        name: variable,
                        value,
                        kind,
                    }))),

                    Expression::Get(ref get) => Ok(Expression::Set(Box::new(Set {
                        object: get.object.clone(),
                        name: get.name.clone(),
                        value,
                        handle: self.variable_use_maker.next(),
                    }))),

                    _ => Err(ParserError::IllegalExpression(
                        "Error at '=': Invalid assignment target.".to_owned(),
                    )),
                }
            },

            None => Err(ParserError::EOF("Expected an ';' but instead found an EOF".to_string())),
            Some(_) => Ok(expr),
        }
    }


    fn ternary(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.or()?;

        while self.tokens.peek().map(|t| &t.token) == Some(&TokenType::QUESTION) {
            match self.tokens.next().map(|t| t.token) {
                Some(TokenType::QUESTION) => {
                    let then_branch = self.expression()?;

                    self.advance_check(
                        TokenType::COLON,
                        "Expected ':' after lhs ternary condition.",
                    )?;

                    let else_branch = self.ternary()?; // Allows nesting of ternarys


                    let ternary_expr = Ternary {
                        condition: expr,
                        then_branch,
                        else_branch,
                    };

                    expr = Expression::Ternary(Box::new(ternary_expr))
                },

                None => unreachable!(),

                _ => unreachable!(),
            }
        }

        Ok(expr)
    }

    fn or(&mut self) -> Result<Expression, ParserError> {
        // logic_or    → logic_and ( "or" logic_and )*
        let mut expr = self.and()?;

        while self.tokens.peek().map(|t| &t.token) == Some(&TokenType::OR) {
            match self.tokens.next().map(|t| t.token) {
                Some(TokenType::OR) => {
                    let operator = LogicOperator::Or;

                    let right = self.and()?;

                    expr = Expression::Logical(Box::new(Logical {
                        left: expr,
                        operator,
                        right,
                    }))
                },
                None => return Err(ParserError::EOF("Unexpected end of file".to_string())),
                _ => return Err(ParserError::IllegalExpression(String::from("Invalid expression"))),
            }
        }

        Ok(expr)
    }

    fn and(&mut self) -> Result<Expression, ParserError> {
        // logic_and   → equality ( "and" equality )*

        let mut expr = self.equality()?;



        while self.tokens.peek().map(|t| &t.token) == Some(&TokenType::AND) {
            match self.tokens.next().map(|t| t.token) {
                Some(TokenType::AND) => {
                    let operator = LogicOperator::And;

                    let right = self.equality()?;

                    expr = Expression::Logical(Box::new(Logical {
                        left: expr,
                        operator,
                        right,
                    }))
                },
                None => unreachable!(),

                _ => unreachable!(),
            }
        }

        Ok(expr)
    }








    fn equality(&mut self) -> Result<Expression, ParserError> {
        // equality → comparison ( ( "!=" | "==" ) comparison )*

        let mut expr: Expression = self.comparison()?;


        while self.tokens.peek().map(|t| &t.token) == Some(&TokenType::EQUALTO)
            || self.tokens.peek().map(|t| &t.token) == Some(&TokenType::BANGEQUAL)
        {
            match self.tokens.next().map(|t| t.token) {
                Some(TokenType::EQUALTO) => {
                    let right_expr = self.comparison()?;

                    expr = Expression::Binary(Box::new(Binary {
                        left_expr: expr,
                        operator: Operator::EqualEqual,
                        right_expr: right_expr,
                    }))
                },

                Some(TokenType::BANGEQUAL) => {
                    let right_expr = self.comparison()?;

                    expr = Expression::Binary(Box::new(Binary {
                        left_expr: expr,
                        operator: Operator::BangEqual,
                        right_expr: right_expr,
                    }))
                },

                None => unreachable!(),

                _ => unreachable!(),
            }
        }

        Ok(expr)
    }


    fn comparison(&mut self) -> Result<Expression, ParserError> {
        // comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )*

        let mut expr = self.term()?;


        while self.tokens.peek().map(|t| &t.token) == Some(&TokenType::GREATERTHAN)
            || self.tokens.peek().map(|t| &t.token) == Some(&TokenType::GREATERTHANEQUAL)
            || self.tokens.peek().map(|t| &t.token) == Some(&TokenType::LESSTHAN)
            || self.tokens.peek().map(|t| &t.token) == Some(&TokenType::LESSTHANEQUAL)
        {
            match self.tokens.next().map(|t| t.token) {
                Some(TokenType::GREATERTHAN) => {
                    let right_expr = self.term()?;

                    expr = Expression::Binary(Box::new(Binary {
                        left_expr: expr,
                        operator: Operator::GreaterThan,
                        right_expr,
                    }));
                },

                Some(TokenType::GREATERTHANEQUAL) => {
                    let right_expr = self.term()?;

                    expr = Expression::Binary(Box::new(Binary {
                        left_expr: expr,
                        operator: Operator::GreaterThanEqual,
                        right_expr,
                    }));
                },
                Some(TokenType::LESSTHAN) => {
                    let right_expr = self.term()?;

                    expr = Expression::Binary(Box::new(Binary {
                        left_expr: expr,
                        operator: Operator::LessThan,
                        right_expr,
                    }));
                },

                Some(TokenType::LESSTHANEQUAL) => {
                    let right_expr = self.term()?;

                    expr = Expression::Binary(Box::new(Binary {
                        left_expr: expr,
                        operator: Operator::LessThanEqual,
                        right_expr,
                    }));
                },

                None => unreachable!(),

                _ => unreachable!(),
            }
        }
        Ok(expr)
    }


    fn term(&mut self) -> Result<Expression, ParserError> {
        //  factor ( ( "-" | "+" ) factor )*

        let mut expr = self.factor()?;

        while self.tokens.peek().map(|t| &t.token) == Some(&TokenType::MINUS)
            || self.tokens.peek().map(|t| &t.token) == Some(&TokenType::PLUS)
        {
            match self.tokens.next().map(|t| t.token) {
                Some(TokenType::MINUS) => {
                    let right_expr = self.factor()?;

                    expr = Expression::Binary(Box::new(Binary {
                        left_expr: expr,
                        operator: Operator::Minus,
                        right_expr,
                    }))
                },

                Some(TokenType::PLUS) => {
                    let right_expr = self.factor()?;

                    expr = Expression::Binary(Box::new(Binary {
                        left_expr: expr,
                        operator: Operator::Plus,
                        right_expr,
                    }));
                },

                None => unreachable!(),

                _ => unreachable!(),
            }
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expression, ParserError> {
        // unary  ( "/" | "*" ) unary )

        let mut expr = self.exponent()?;

        while self.tokens.peek().map(|t| &t.token) == Some(&TokenType::SLASH)
            || self.tokens.peek().map(|t| &t.token) == Some(&TokenType::STAR)
            || self.tokens.peek().map(|t| &t.token) == Some(&TokenType::MODULO)
        {
            match self.tokens.next().map(|t| t.token) {
                Some(TokenType::SLASH) => {
                    let right_expr = self.exponent()?;

                    expr = Expression::Binary(Box::new(Binary {
                        left_expr: expr,
                        operator: Operator::Divide,
                        right_expr,
                    }));
                },

                Some(TokenType::MODULO) => {
                    let right_expr = self.exponent()?;
                    expr = Expression::Binary(Box::new(Binary {
                        left_expr: expr,
                        operator: Operator::Modulo,
                        right_expr,
                    }));
                },

                Some(TokenType::STAR) => {
                    let right_expr = self.exponent()?;

                    expr = Expression::Binary(Box::new(Binary {
                        left_expr: expr,
                        operator: Operator::Star,
                        right_expr,
                    }));
                },

                _ => unreachable!(),
            }
        }

        Ok(expr)
    }

    fn exponent(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.unary()?;

        while self.tokens.peek().map(|t| &t.token) == Some(&TokenType::EXPONENTIAL) {
            match self.tokens.next().map(|t| t.token) {
                Some(TokenType::EXPONENTIAL) => {
                    let right_expr = self.unary()?;

                    expr = Expression::Binary(Box::new(Binary {
                        left_expr: expr,
                        operator: Operator::Exponential,
                        right_expr,
                    }));
                },

                _ => unreachable!(),
            }
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expression, ParserError> {
        // ( "!" | "-" ) unary | primary

        match self.tokens.peek().map(|t| &t.token) {
            Some(&TokenType::BANG) => {
                self.tokens.next();


                // Actually consumes the token

                let expr = self.unary()?;

                Ok(Expression::Unary(Box::new(Unary {
                    operator: UnaryOperator::Bang,
                    expr,
                })))
            },

            Some(&TokenType::MINUS) => {
                self.tokens.next();

                let expr = self.unary()?;
                Ok(Expression::Unary(Box::new(Unary {
                    operator: UnaryOperator::Minus,
                    expr,
                })))
            },

            Some(_) => self.call(),

            None => Err(ParserError::EOF(
                "Expected a '!', '-' or a primary but instead found an EOF ".to_string(),
            )),
        }
    }


    fn call(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.primary()?;


        if self.tokens.peek().map(|t| &t.token) == Some(&TokenType::LBRACKET) {
            self.tokens.next();
            expr = self.index_expr(expr)?;
        } else {
            loop {
                if self.tokens.peek().map(|t| &t.token) == Some(&TokenType::LPAREN) {
                    self.tokens.next();
                    expr = self.finish_call(expr)?;
                } else if self.tokens.peek().map(|t| &t.token) == Some(&TokenType::DOT) {
                    self.tokens.next(); // Consume the dot
                    let name = match self.tokens.next() {
                        // TODO refractor into a function
                        Some(Token {
                            token: TokenType::IDENTIFIER(ref ident),
                            ..
                        }) => Variable(ident.to_owned()),

                        Some(Token {
                            ref line,
                            ref column,
                            ..
                        }) => {
                            return Err(ParserError::Expected(
                                format!("Expected a class name on line {} column {}", line, column),
                            ))
                        },

                        None => {
                            let error = "Expected a class name but instead found an EOF".to_owned();
                            return Err(ParserError::EOF(error));
                        },
                    };

                    expr = Expression::Get(Box::new(Get { object: expr, name ,handle:self.variable_use_maker.next()}));
                } else {
                    break;
                }
            }
        }

        Ok(expr)
    }



    fn index_expr(&mut self, target: Expression) -> Result<Expression, ParserError> {
        let index = self.expression()?;
        self.advance_check(TokenType::RBRACKET, "Expected ']' to close an index expression")?;


        Ok(Expression::IndexExpr(Box::new(IndexExpr { target, index })))
    }


    fn finish_call(&mut self, callee: Expression) -> Result<Expression, ParserError> {
        let mut arguments: Vec<Expression> = vec![];


        if !self.peek_check(TokenType::RPAREN) {
            while {
                if arguments.len() >= 32 {
                    println!("Cannot have more than 32 arguments.");
                }
                arguments.push(self.expression()?);
                self.tokens.peek().map(|t| &t.token) == Some(&TokenType::COMMA)
                    && self.tokens.next().map(|t| t.token) == Some(TokenType::COMMA)
            } {}
        }

        self.advance_check(TokenType::RPAREN, "Expected ')' after arguments.")?;

        Ok(Expression::Call(Box::new(Call { callee, arguments })))
    }


    fn primary(&mut self) -> Result<Expression, ParserError> {
        // NUMBER | STRING | "false" | "true" | "nil" | "(" expression ")"



        match self.tokens.next() {
            Some(Token {
                ref token,
                ref column,
                ref line,
            }) => match *token {
                TokenType::INT(i) => Ok(Expression::Literal(Literal::Number(i))),

                TokenType::STRING(ref string) => {
                    Ok(Expression::Literal(Literal::Str(string.to_owned())))
                },

                TokenType::TRUE(_) => Ok(Expression::Literal(Literal::True(true))),

                TokenType::FALSE(_) => Ok(Expression::Literal(Literal::False(false))),

                TokenType::NIL => Ok(Expression::Literal(Literal::Nil)),

                TokenType::FUNCTION => self.function_body("function"),

                TokenType::IDENTIFIER(ref s) => {
                    Ok(Expression::Var(Variable(s.to_owned()), self.variable_use_maker.next()))
                },
                TokenType::THIS => Ok(Expression::This(self.variable_use_maker.next())),

                TokenType::LBRACKET => {
                    let mut items: Vec<Expression> = vec![];

                    if self.peek_check(TokenType::RBRACKET) {
                        self.tokens.next();
                        return Ok(Expression::Array(Array { items }));
                    }

                    while {
                        items.push(self.expression()?);
                        self.tokens.peek().map(|t| &t.token) == Some(&TokenType::COMMA)
                            && self.tokens.next().map(|t| t.token) == Some(TokenType::COMMA)
                    } {}

                    self.advance_check(
                        TokenType::RBRACKET,
                        "Expected a ']' to close the brackets .",
                    )?;


                    Ok(Expression::Array(Array { items }))
                },
                TokenType::LBRACE => {
                    let mut items: Vec<(Expression, Expression)> = vec![];


                    if self.peek_check(TokenType::RBRACE) {
                        self.tokens.next();
                        return Err(ParserError::IllegalExpression(
                            "An empty dict is not a valid expression".to_owned(),
                        ));
                    }

                    while {
                        let left = self.expression()?;
                        self.advance_check(TokenType::COLON, "Expected a ':' after dict key ")?;
                        let right = self.expression()?;

                        items.push((left, right));
                        self.tokens.peek().map(|t| &t.token) == Some(&TokenType::COMMA)
                            && self.tokens.next().map(|t| t.token) == Some(TokenType::COMMA)
                    } {}


                    self.advance_check(TokenType::RBRACE, "Expected a '}' to close a dictionary.")?;

                    Ok(Expression::Dict(Dictionary { items }))
                },

                TokenType::LPAREN => {
                    let expr = match self.expression() {
                        Ok(expression) => expression,
                        Err(e) => return Err(e),
                    };

                    self.advance_check(TokenType::RPAREN, "Expected a ')' to close the brackets.")?;


                    Ok(Expression::Grouping(Box::new(Grouping { expr })))
                },

                _ => {
                    let error = error_with_line(
                        &format!("Error at '{}': Expect expression", token),
                        line,
                        column,
                    );

                    Err(ParserError::IllegalExpression(error))
                },
            },

            None => Err(ParserError::EOF(
                "Unexpected end of file when parsing an expression".to_string(),
            )),
        }
    }

    fn synchronize(&mut self) -> Result<(), ParserError> {
        self.tokens.next();

        while self.tokens.peek() != None {
            match self.tokens.next().map(|t| t.token) {
                Some(TokenType::CLASS) |
                Some(TokenType::FUNCTION) |
                Some(TokenType::IDENTIFIER(_)) |
                Some(TokenType::FOR) |
                Some(TokenType::IF) |
                Some(TokenType::WHILE) |
                Some(TokenType::RETURN) => break,
                None => unreachable!(),
                _ => self.tokens.next(),
            };
        }

        Ok(())
    }
}
