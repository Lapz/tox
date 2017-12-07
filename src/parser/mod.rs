use token::{Token, TokenType};
use std::iter::Peekable;
use std::vec::IntoIter;
use pos::Postition;
use ast::expr::*;
use ast::statement::*;
use pos::WithPos;
// use pprint::PrettyPrint;

pub struct Parser<'a> {
    tokens: Peekable<IntoIter<Token<'a>>>,
    loop_depth: i32,
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
            ParserError::NotExpected(ref e) |
            ParserError::IllegalToken(ref e) |
            ParserError::Expected(ref e) |
            ParserError::IllegalExpression(ref e) |
            ParserError::Break(ref e) => write!(f, "{}", e),
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
    pub fn new(tokens: Vec<Token<'a>>) -> Self {
        Parser {
            tokens: tokens.into_iter().peekable(),

            loop_depth: 0,
            variable_use_maker: VariableUseMaker::new(),
        }
    }

    pub fn parse_single(&mut self) -> Result<WithPos<Expression<'a>>, ParserError<'a>> {
        let pos = self.tokens.peek().map(|t| t.pos).unwrap();
        
        Ok(WithPos::new(self.expression()?,pos))
    }

    pub fn synchronize(&mut self) {
        self.advance();

        while self.peek(|token| token == &TokenType::EOF) {
            match self.advance().map(|t| t.token) {
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

    }


    fn error(&self, message: &str, pos: Postition) -> String {
        format!("{} on {}", message, pos)
    }


    fn peek<F>(&mut self, mut check: F) -> bool
    where
        F: FnMut(&TokenType<'a>) -> bool,
    {
        self.tokens.peek().map_or(
            false,
            |token| check(&token.token),
        )
    }

    fn matched(&mut self, tokens: Vec<TokenType<'a>>) -> bool {

        let mut found = false;

        for token in tokens {
            if self.peek(|peeked| peeked == &token) {
                found = true;
            }
        }

        if found { true } else { false }

    }

    fn advance(&mut self) -> Option<Token<'a>> {
        self.tokens.next()
    }


    fn token_type(&mut self) -> TokenType<'a> {
        self.advance().map(|t| t.token).unwrap()
    }

    fn consume(&mut self, token_to_check: TokenType<'a>, msg: &str) -> Result<(), ParserError<'a>> {
        match self.tokens.next() {
            Some(Token { ref token, ref pos }) => {
                if token == &token_to_check {
                    return Ok(());
                }

                Err(ParserError::Expected(self.error(msg, *pos)))
            }
            None => Err(ParserError::EOF),
        }
    }
}


// // Expression Parsing

impl<'a> Parser<'a> {
    fn expression(&mut self) -> Result<Expression<'a>, ParserError<'a>> {
        self.ternary()
    }


    fn ternary(&mut self) -> Result<Expression<'a>, ParserError<'a>> {
        let mut condition = self.comma()?;

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

    fn comma(&mut self) -> Result<Expression<'a>, ParserError<'a>> {

        let mut expr = self.equality()?;

        while self.matched(vec![TokenType::COMMA]) {
            let operator = get_operator(self.token_type());

            let right_expr = Box::new(self.equality()?);

            expr = Expression::Binary {
                left_expr: Box::new(expr),
                operator,
                right_expr,
            }
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expression<'a>, ParserError<'a>> {
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

    fn comparison(&mut self) -> Result<Expression<'a>, ParserError<'a>> {

        let mut expr = self.addition()?;

        while self.matched(vec![
            TokenType::LESSTHAN,
            TokenType::LESSTHANEQUAL,
            TokenType::GREATERTHAN,
            TokenType::GREATERTHANEQUAL,
        ])
        {
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

    fn addition(&mut self) -> Result<Expression<'a>, ParserError<'a>> {

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

    fn multiplication(&mut self) -> Result<Expression<'a>, ParserError<'a>> {
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

    fn unary(&mut self) -> Result<Expression<'a>, ParserError<'a>> {
        if self.matched(vec![TokenType::BANG, TokenType::MINUS, TokenType::PLUS]) {
            let operator = get_unary_operator(self.token_type());

            let right = Box::new(self.unary()?);

            return Ok(Expression::Unary {
                expr: right,
                operator,
            });
        }

        self.primary()

    }


    fn primary(&mut self) -> Result<Expression<'a>, ParserError<'a>> {
        match self.advance() {
            Some(Token { ref token, ref pos }) => {
                match *token {
                    TokenType::FALSE(_) => Ok(Expression::Literal(Literal::False(false))),
                    TokenType::TRUE(_) => Ok(Expression::Literal(Literal::True(true))),
                    TokenType::NIL => Ok(Expression::Literal(Literal::Nil)),
                    TokenType::INT(ref i) => Ok(Expression::Literal(Literal::Int(*i))),
                    TokenType::FLOAT(ref f) => Ok(Expression::Literal(Literal::Float(*f))),
                    TokenType::STRING(ref s) => Ok(Expression::Literal(Literal::Str(s.clone()))),
                    TokenType::LPAREN => {
                        let expr = Box::new(self.expression()?);
                        self.consume(
                            TokenType::RPAREN,
                            "Expect \')\' after expression",
                        )?;

                        return Ok(Expression::Grouping { expr });

                    }

                    _ => unimplemented!(),
                }
            }
            None => Err(ParserError::EOF),
            Some(other) => unimplemented!(),

        }
    }
}
