mod test;
use token::{Token, TokenType};
use std::iter::Peekable;
use std::vec::IntoIter;
use pos::Postition;
use ast::expr::*;
use ast::statement::*;
use pos::WithPos;
use symbol::{Symbol, Symbols};
#[derive(Debug)]
pub struct Parser<'a> {
    tokens: Peekable<IntoIter<Token<'a>>>,
    loop_depth: i32,
    pub symbols: &'a mut Symbols<'a, ()>,
    variable_use_maker: VariableUseMaker,
}

use std::fmt::{Display, Formatter};
use std::fmt;


#[derive(Clone, Debug)]
pub enum ParserError<'a> {
    NotExpected(String),
    IllegalToken(String),
    IllegalExpression(String),
    EOF,
    EOFExpected(TokenType<'a>),
    EOFMany(Vec<TokenType<'a>>),
    MissingCloseBracket,
    Expected(String),
    Break(String),
}

impl<'a> Display for ParserError<'a> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            ParserError::NotExpected(ref e)
            | ParserError::IllegalToken(ref e)
            | ParserError::Expected(ref e)
            | ParserError::IllegalExpression(ref e)
            | ParserError::Break(ref e) => write!(f, "{}", e),
            ParserError::MissingCloseBracket => write!(f, " ')' was expected but not found "),
            ParserError::EOF => write!(f, "Unexpected end of file"),
            ParserError::EOFExpected(ref e) => {
                write!(f, "Expected {} but instead found the EOF", e)
            }
            ParserError::EOFMany(ref many) => {
                write!(f, "Expected {:?} but instead found EOF", many)
            }
        }
    }
}


impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token<'a>>, symbols: &'a mut Symbols<'a, ()>) -> Self {
        Parser {
            tokens: tokens.into_iter().peekable(),
            symbols,
            loop_depth: 0,
            variable_use_maker: VariableUseMaker::new(),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<WithPos<Statement>>, Vec<ParserError<'a>>> {
        let mut statements = vec![];

        let mut errors = vec![];

        while self.peek(|token| token != &TokenType::EOF) {
            match self.declaration() {
                Ok(statement) => statements.push(statement),
                Err(e) => {
                    errors.push(e);
                    self.synchronize();
                }
            }
        }

        if errors.is_empty() {
            Ok(statements)
        } else {
            Err(errors)
        }
    }

    pub fn parse_single(&mut self) -> Result<WithPos<Expression>, ParserError<'a>> {
        let pos = self.tokens.peek().map(|t| t.pos).unwrap();

        Ok(WithPos::new(self.expression()?, pos))
    }

    pub fn synchronize(&mut self) {
        self.advance();

        while self.peek(|token| token == &TokenType::EOF) {
            match self.advance().map(|t| t.token) {
                Some(TokenType::CLASS)
                | Some(TokenType::FUNCTION)
                | Some(TokenType::IDENTIFIER(_))
                | Some(TokenType::FOR)
                | Some(TokenType::IF)
                | Some(TokenType::WHILE)
                | Some(TokenType::RETURN) => break,
                None => unreachable!(),
                _ => self.advance(),
            };
        }
    }


    fn error(&self, message: &str, pos: Postition) -> String {
        format!("{} on {}", message, pos)
    }


    fn peek<F>(&mut self, mut check: F) -> bool
    where
        F: FnMut(&TokenType<'a>) -> bool,
    {
        self.tokens
            .peek()
            .map_or(false, |token| check(&token.token))
    }

    fn recognise(&mut self, token: TokenType<'a>) -> bool {
        if self.peek(|peeked| peeked == &token) {
            return true;
        }

        false
    }

    fn matched(&mut self, tokens: Vec<TokenType<'a>>) -> bool {
        let mut found = false;

        for token in tokens {
            if self.peek(|peeked| peeked == &token) {
                found = true;
            }
        }

        if found {
            true
        } else {
            false
        }
    }

    fn advance(&mut self) -> Option<Token<'a>> {
        self.tokens.next()
    }


    fn token_type(&mut self) -> TokenType<'a> {
        self.advance().map(|t| t.token).unwrap()
    }

    fn consume(&mut self, token_to_check: TokenType<'a>, msg: &str) -> Result<(), ParserError<'a>> {
        match self.advance() {
            Some(Token { ref token, ref pos }) => {
                if token == &token_to_check {
                    return Ok(());
                }

                Err(ParserError::Expected(self.error(msg, *pos)))
            }
            None => Err(ParserError::EOF),
        }
    }

    fn consume_get_pos(
        &mut self,
        token_to_check: TokenType<'a>,
        msg: &str,
    ) -> Result<Postition, ParserError<'a>> {
        match self.advance() {
            Some(Token { ref token, ref pos }) => {
                if token == &token_to_check {
                    return Ok(*pos);
                }

                Err(ParserError::Expected(self.error(msg, *pos)))
            }
            None => Err(ParserError::EOF),
        }
    }

    fn consume_name(&mut self, msg: &str) -> Result<Symbol, ParserError<'a>> {
        match self.advance() {
            Some(Token {
                token: TokenType::IDENTIFIER(ref ident),
                ..
            }) => Ok(self.symbols.symbol(ident)),
            Some(Token { ref pos, .. }) => Err(ParserError::Expected(self.error(msg, *pos))),
            None => Err(ParserError::EOF),
        }
    }

    fn get_pos(&mut self) -> Postition {
        match self.advance() {
            Some(Token { ref pos, .. }) => return *pos,
            _ => unreachable!(),
        }
    }
}



// Statements
impl<'a> Parser<'a> {
    fn declaration(&mut self) -> Result<WithPos<Statement>, ParserError<'a>> {
        if self.recognise(TokenType::VAR) {
            self.var_declaration()
        } else if self.recognise(TokenType::FUNCTION) {
            self.function("function")
        } else if self.recognise(TokenType::CLASS) {
            self.class_declaration()
        } else {
            self.statement()
        }
    }

    fn statement(&mut self) -> Result<WithPos<Statement>, ParserError<'a>> {
        if self.recognise(TokenType::LBRACE) {
            self.block()
        } else if self.recognise(TokenType::BREAK) {
            self.break_statement()
        } else if self.recognise(TokenType::CONTINUE) {
            self.continue_statement()
        } else if self.recognise(TokenType::RETURN) {
            self.return_statement()
        } else if self.recognise(TokenType::IF) {
            self.if_statement()
        } else if self.recognise(TokenType::DO) {
            self.do_statement()
        } else if self.recognise(TokenType::WHILE) {
            self.while_statement()
        } else if self.recognise(TokenType::FOR) {
            self.for_statement()
        } else {
            self.expression_statement()
        }
    }




    fn expression_statement(&mut self) -> Result<WithPos<Statement>, ParserError<'a>> {
        let expr = self.expression()?;

        let pos = self.consume_get_pos(TokenType::SEMICOLON, "Expected \';\' after value.")?;

        Ok(WithPos::new(Statement::ExpressionStmt(expr), pos))
    }

    fn function(&mut self, kind: &str) -> Result<WithPos<Statement>, ParserError<'a>> {
        let func_pos = self.get_pos();
        let name = self.consume_name(&format!("Expected a {} name", kind))?;
        Ok(WithPos::new(
            Statement::Function {
                name,
                body: self.fun_body(kind)?,
            },
            func_pos,
        ))
    }


    // Keyword statements

    fn break_statement(&mut self) -> Result<WithPos<Statement>, ParserError<'a>> {
        let break_pos = self.get_pos();

        if !(self.loop_depth >= 0) {
            let error = "Must be inside a loop to use break".to_owned();
            return Err(ParserError::Break(error));
        }

        self.consume(TokenType::SEMICOLON, "Expected ';' after 'break'")?;

        Ok(WithPos::new(Statement::Break, break_pos))
    }

    fn continue_statement(&mut self) -> Result<WithPos<Statement>, ParserError<'a>> {
        let cont_pos = self.get_pos();

        if !(self.loop_depth >= 0) {
            let error = "Must be inside a loop to use 'continue'".to_owned();

            return Err(ParserError::Break(error));
        }

        self.consume(TokenType::SEMICOLON, "Expected ';' after 'continue'")?;

        Ok(WithPos::new(Statement::Continue, cont_pos))
    }

    // Control Flow Statements


    fn for_statement(&mut self) -> Result<WithPos<Statement>, ParserError<'a>> {
        let for_pos = self.get_pos();
        self.consume(TokenType::LPAREN, "Expected '(' after 'for'")?;

        let mut initi = None;

        if self.recognise(TokenType::SEMICOLON) {
            self.advance();
        } else if self.recognise(TokenType::VAR) {
            initi = Some(self.var_declaration()?);
        } else {
            initi = Some(self.expression_statement()?);
        }

        let mut condition = None;

        if !self.recognise(TokenType::SEMICOLON) {
            condition = Some(self.expression()?);
        }

        self.consume(TokenType::SEMICOLON, "Expected ';' after loop condition .")?;

        let mut increment = None;

        if !self.recognise(TokenType::RPAREN) {
            increment = Some(self.expression()?);
        }

        let increment_pos =
            self.consume_get_pos(TokenType::RPAREN, "Expected ')' after for clauses.")?;

        self.loop_depth += 1;

        let mut body = self.statement()?;
        let body_pos = body.pos.clone();

        if increment != None {
            body = WithPos::new(
                Statement::Block(vec![
                    body,
                    WithPos::new(Statement::ExpressionStmt(increment.unwrap()), increment_pos),
                ]),
                body_pos,
            );
        } else if condition == None {
            condition = Some(Expression::Literal(Literal::True(true)));
        }

        body = WithPos::new(
            Statement::WhileStmt {
                condition: condition.unwrap(),
                body: Box::new(body),
            },
            body_pos,
        );

        if initi != None {
            let mut statements = vec![];

            statements.push(initi.unwrap());
            statements.push(body);

            body = WithPos::new(Statement::Block(statements), for_pos)
        }

        self.loop_depth -= 1;

        Ok(body)
    }

    fn do_statement(&mut self) -> Result<WithPos<Statement>, ParserError<'a>> {
        let do_pos = self.get_pos();

        let body = self.statement()?;

        self.consume(TokenType::WHILE, "Expected while after 'do' condition")?;

        self.consume(TokenType::LPAREN, "Expected '(' after while'")?;

        let condition = self.expression()?;

        self.consume(TokenType::RPAREN, "Expected ')' after 'while'")?;


        let do_statement = Statement::DoStmt {
            body: Box::new(body),
            condition,
        };

        Ok(WithPos::new(do_statement, do_pos))
    }

    fn while_statement(&mut self) -> Result<WithPos<Statement>, ParserError<'a>> {
        let while_pos = self.get_pos(); // Eats the while;

        self.consume(TokenType::LPAREN, "Expected '(' after while'")?;

        let condition = self.expression()?;

        self.consume(TokenType::RPAREN, "Expected ')' after 'while'")?;

        self.loop_depth += 1;

        let body = self.statement()?;

        let while_st = Statement::WhileStmt {
            condition,
            body: Box::new(body),
        };

        self.loop_depth -= 1;

        Ok(WithPos::new(while_st, while_pos))
    }

    fn if_statement(&mut self) -> Result<WithPos<Statement>, ParserError<'a>> {
        let if_pos = self.get_pos(); // Eats the if ;

        self.consume(TokenType::LPAREN, "Expected a \'(\' after \'if\'")?;

        let condition = self.expression()?;

        self.consume(TokenType::RPAREN, "Expected ')' after 'if'")?;

        let then_branch = Box::new(self.statement()?);
        let mut else_branch = None;

        if self.recognise(TokenType::ELSE) {
            else_branch = Some(Box::new(self.statement()?));

            return Ok(WithPos::new(
                Statement::IfStmt {
                    condition,
                    then_branch,
                    else_branch,
                },
                if_pos,
            ));
        }

        Ok(WithPos::new(
            Statement::IfStmt {
                condition,
                then_branch,
                else_branch,
            },
            if_pos,
        ))
    }


    fn return_statement(&mut self) -> Result<WithPos<Statement>, ParserError<'a>> {
        let pos = self.get_pos();

        let mut value = None;

        if !self.recognise(TokenType::COLON) {
            value = Some(self.expression()?);
        }

        self.consume(TokenType::SEMICOLON, "Expected a ")?;

        Ok(WithPos::new(Statement::Return(value), pos))
    }

    fn block(&mut self) -> Result<WithPos<Statement>, ParserError<'a>> {
        let pos = self.get_pos();

        let mut statement = vec![];

        while !self.recognise(TokenType::RBRACE) {
            statement.push(self.declaration()?);
        }

        self.consume(TokenType::RBRACE, "Expected a \'}\' after block.")?;

        Ok(WithPos::new(Statement::Block(statement), pos))
    }


    fn class_declaration(&mut self) -> Result<WithPos<Statement>, ParserError<'a>> {
        let class_pos = self.get_pos();
        let name = self.consume_name("Expected a class name")?;

        self.consume(TokenType::LBRACE, "Expect \'{ \' before class body")?;

        let mut methods = vec![];

        while !self.recognise(TokenType::RBRACE) {
            methods.push(self.function("method")?);
        }

        self.consume(TokenType::RBRACE, "Expect \'}\'' after class body")?;

        Ok(WithPos::new(Statement::Class { methods, name }, class_pos))
    }



    fn var_declaration(&mut self) -> Result<WithPos<Statement>, ParserError<'a>> {
        let var_pos = self.get_pos();
        let name = self.consume_name("Expected an IDENTIFIER after a \'var\' ")?;
        let mut var_type = None;

        if self.recognise(TokenType::SEMICOLON) {
            self.advance();

            let value = Expression::Literal(Literal::Nil);

            return Ok(WithPos::new(Statement::Var(name, value,None), var_pos));
        }

        if self.recognise(TokenType::COLON) {
            self.advance();

            var_type = get_type(self.token_type());

            if var_type.is_none() {
                return Err(ParserError::Expected(self.error("Expected a proper type", var_pos)));
            }

        }

        if self.matched(vec![
            TokenType::ASSIGN,
            TokenType::MINUSASSIGN,
            TokenType::PLUSASSIGN,
            TokenType::SLASHASSIGN,
            TokenType::STARASSIGN,
        ]) {
            self.advance();
            let expr = self.expression()?;
            self.consume(
                TokenType::SEMICOLON,
                "Expect \';\' after variable decleration.",
            )?;
            return Ok(WithPos::new(Statement::Var(name, expr,None), var_pos));
        }

        Err(ParserError::Expected(self.error(
            "Expected an assignment",
            var_pos,
        )))
    }
}


// Expression Parsing
impl<'a> Parser<'a> {
    fn expression(&mut self) -> Result<Expression, ParserError<'a>> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expression, ParserError<'a>> {
        let expr = self.ternary()?;

        if self.matched(vec![
            TokenType::ASSIGN,
            TokenType::PLUSASSIGN,
            TokenType::MINUSASSIGN,
            TokenType::STARASSIGN,
            TokenType::SLASHASSIGN,
        ]) {
            let kind = get_assign_operator(self.token_type());

            let value = self.assignment()?;

            match expr {
                Expression::Var(name, _) => {
                    return Ok(Expression::Assign {
                        handle: self.variable_use_maker.next(),
                        name,
                        value: Box::new(value),
                        kind,
                    })
                }
                _ => {
                    return Err(ParserError::IllegalExpression(
                        "Error at '=': Invalid assignment target.".to_owned(),
                    ))
                }
            }
        }

        Ok(expr)
    }


    fn ternary(&mut self) -> Result<Expression, ParserError<'a>> {
        let mut condition = self.or()?;

        while self.matched(vec![TokenType::QUESTION]) {
            self.consume(TokenType::QUESTION, "Expected a '?'")?;

            let then_branch = Box::new(self.expression()?);

            self.consume(
                TokenType::COLON,
                "Expected ':' after lhs ternary condition.",
            )?;

            let else_branch = Box::new(self.ternary()?);

            condition = Expression::Ternary {
                condition: Box::new(condition),
                then_branch,
                else_branch,
            }
        }

        Ok(condition)
    }

    fn or(&mut self) -> Result<Expression, ParserError<'a>> {
        let mut expr = self.and()?;

        while self.recognise(TokenType::OR) {
            let operator = get_logic_operator(self.token_type());

            let right = Box::new(self.and()?);

            expr = Expression::Logical {
                left: Box::new(expr),
                operator,
                right,
            }
        }

        Ok(expr)
    }

    fn and(&mut self) -> Result<Expression, ParserError<'a>> {
        let mut expr = self.equality()?;

        while self.recognise(TokenType::AND) {
            let operator = get_logic_operator(self.token_type());

            let right = Box::new(self.equality()?);

            expr = Expression::Logical {
                left: Box::new(expr),
                operator,
                right,
            }
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expression, ParserError<'a>> {
        let mut expr = self.comparison()?;

        while self.matched(vec![TokenType::BANGEQUAL, TokenType::EQUALEQUAL]) {
            let operator = get_operator(self.token_type());

            let right_expr = Box::new(self.comparison()?);

            expr = Expression::Binary {
                left_expr: Box::new(expr),
                operator,
                right_expr,
            };

            println!("token {:#?}", self.tokens);
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expression, ParserError<'a>> {
        let mut expr = self.addition()?;

        while self.matched(vec![
            TokenType::LESSTHAN,
            TokenType::LESSTHANEQUAL,
            TokenType::GREATERTHAN,
            TokenType::GREATERTHANEQUAL,
        ]) {
            let operator = get_operator(self.token_type());

            let right_expr = Box::new(self.addition()?);

            expr = Expression::Binary {
                left_expr: Box::new(expr),
                operator,
                right_expr,
            }
        }

        Ok(expr)
    }

    fn addition(&mut self) -> Result<Expression, ParserError<'a>> {
        let mut expr = self.multiplication()?;

        while self.matched(vec![TokenType::MINUS, TokenType::PLUS]) {
            let operator = get_operator(self.token_type());

            let right_expr = Box::new(self.multiplication()?);

            expr = Expression::Binary {
                left_expr: Box::new(expr),
                operator,
                right_expr,
            }
        }

        Ok(expr)
    }

    fn multiplication(&mut self) -> Result<Expression, ParserError<'a>> {
        let mut expr = self.unary()?;

        while self.matched(vec![TokenType::SLASH, TokenType::STAR]) {
            let operator = get_operator(self.token_type());

            let right_expr = Box::new(self.unary()?);

            expr = Expression::Binary {
                left_expr: Box::new(expr),
                operator,
                right_expr,
            }
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expression, ParserError<'a>> {
        if self.matched(vec![TokenType::BANG, TokenType::MINUS, TokenType::PLUS]) {
            let operator = get_unary_operator(self.token_type());

            let right = Box::new(self.unary()?);

            return Ok(Expression::Unary {
                expr: right,
                operator,
            });
        }

        self.call()
    }

    fn call(&mut self) -> Result<Expression, ParserError<'a>> {
        let mut expr = self.primary()?;

        loop {
            if self.recognise(TokenType::LBRACKET) {
                self.advance();
                let index = Box::new(self.expression()?);
                self.consume(
                    TokenType::RBRACKET,
                    "Expected ']' to close an index expression",
                )?;
                return Ok(Expression::IndexExpr {
                    target: Box::new(expr),
                    index,
                });
            } else if self.recognise(TokenType::LPAREN) {
                self.advance();
                expr = self.finish_call(expr)?;
            } else if self.recognise(TokenType::DOT) {
                let name = self.consume_name("Expected a \'class\' name")?;
                expr = Expression::Get {
                    object: Box::new(expr),
                    name,
                    handle: self.variable_use_maker.next(),
                }
            } else {
                break;
            }
        }

        Ok(expr)
    }


    fn primary(&mut self) -> Result<Expression, ParserError<'a>> {
        match self.advance() {
            Some(Token { ref token, ref pos }) => match *token {
                TokenType::FALSE(_) => Ok(Expression::Literal(Literal::False(false))),
                TokenType::TRUE(_) => Ok(Expression::Literal(Literal::True(true))),
                TokenType::NIL => Ok(Expression::Literal(Literal::Nil)),
                TokenType::INT(ref i) => Ok(Expression::Literal(Literal::Int(*i))),
                TokenType::FLOAT(ref f) => Ok(Expression::Literal(Literal::Float(*f))),
                TokenType::STRING(ref s) => Ok(Expression::Literal(Literal::Str(s.clone()))),
                TokenType::IDENTIFIER(ref ident) => Ok(Expression::Var(
                    self.symbols.symbol(ident),
                    self.variable_use_maker.next(),
                )),
                TokenType::THIS => Ok(Expression::This(self.variable_use_maker.next())),
                TokenType::FUNCTION => self.fun_body("function"),
                TokenType::LBRACKET => {
                    let mut items = vec![];

                    if self.recognise(TokenType::RBRACKET) {
                        self.advance();
                        return Ok(Expression::Array { items });
                    }

                    while {
                        items.push(self.expression()?);

                        self.recognise(TokenType::COMMA)
                            && self.advance().map(|t| t.token) == Some(TokenType::COMMA)
                    } {}

                    println!("{:#?}", self);

                    self.consume(
                        TokenType::RBRACKET,
                        "Expected a ']' to close the brackets .",
                    )?;

                    Ok(Expression::Array { items })
                }

                TokenType::LBRACE => {
                    let mut items: Vec<(Expression, Expression)> = vec![];


                    if self.recognise(TokenType::RBRACE) {
                        self.advance();
                        return Ok(Expression::Dict { items });
                    }

                    while {
                        let left = self.expression()?;
                        self.consume(TokenType::COLON, "Expected a ':' after dict key ")?;
                        let right = self.expression()?;

                        items.push((left, right));
                        self.recognise(TokenType::COMMA)
                            && self.advance().map(|t| t.token) == Some(TokenType::COMMA)
                    } {}


                    self.consume(TokenType::RBRACE, "Expected a '}' to close a dictionary.")?;

                    Ok(Expression::Dict { items })
                }

                TokenType::LPAREN => {
                    let expr = Box::new(self.expression()?);
                    self.consume(TokenType::RPAREN, "Expect \')\' after expression")?;

                    return Ok(Expression::Grouping { expr });
                }

                _ => {
                    println!("{:#?}", token);
                    Err(ParserError::IllegalExpression(self.error(
                        "Cannot parse the expression",
                        *pos,
                    )))
                }
            },
            None => Err(ParserError::EOF),
        }
    }
}


// Helper parsing functions
impl<'a> Parser<'a> {
    fn fun_body(&mut self, kind: &str) -> Result<Expression, ParserError<'a>> {
        self.consume(TokenType::LPAREN, "Expected '(' ")?;

        let mut parameters = vec![];

        if !self.recognise(TokenType::RPAREN) {
            while {
                if parameters.len() >= 32 {
                    println!("Cannot have more than 32 arguments")
                };

                let identifier = self.consume_name("Expected a parameter name")?;

                parameters.push(identifier);

                self.recognise(TokenType::COMMA)
                    && self.advance().map(|t| t.token) == Some(TokenType::COMMA)
            } {}
        }

        self.consume(TokenType::RPAREN, "Expected ')' after parameters.")?;

        self.consume(
            TokenType::LBRACE,
            &format!("Expected '{{' before {} body.", kind),
        )?;

        let body = Box::new(self.block()?);

        Ok(Expression::Func { parameters, body })
    }

    fn finish_call(&mut self, callee: Expression) -> Result<Expression, ParserError<'a>> {
        let mut arguments = vec![];

        if !self.recognise(TokenType::RPAREN) {
            while {
                if arguments.len() >= 32 {
                    println!("Cannot have more than 32 arguments.");
                }

                arguments.push(self.expression()?);
                self.recognise(TokenType::COMMA)
                    && self.advance().map(|t| t.token) == Some(TokenType::COMMA)
            } {}
        }

        self.consume(TokenType::RPAREN, "Expected ')' after arguments.")?;

        Ok(Expression::Call {
            callee: Box::new(callee),
            arguments,
        })
    }
}
