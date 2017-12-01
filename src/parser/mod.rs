use token::{Token, TokenType};
use std::iter::Peekable;
use std::vec::IntoIter;
use pos::Postition;
use ast::expr::*;
use ast::statement::*;
use pprint::PrettyPrint;

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
            tokens:tokens.into_iter().peekable(),
            loop_depth: 0,
            variable_use_maker: VariableUseMaker::new(),
        }
    }


    fn error(&self, message: &str, pos: Postition) -> String {
        format!("{} on {}", message, pos)
    }

    

    fn advance(&mut self) -> Option<Token<'a>> {
        self.tokens.next()
    }

    fn token_type(&mut self) -> Option<TokenType<'a>> {
        self.advance().map(|t| t.token)
    }

    

    fn peek<F>(&mut self, mut check:F) -> bool where F:FnMut(&TokenType<'a>) ->bool, {
        self.tokens.peek().map_or(false, |token| check(&token.token) )
    }

    fn matched(&mut self, tokens: Vec<TokenType<'a>>, msg: &str) -> Result<bool, ParserError<'a>> {

        let mut found = false;

        for token in tokens {
            if self.peek(|peeked| peeked == &token) {
                found = true;
            }
        }

        if found {
            Ok(true)
        } else {
            match self.tokens.next().map(|t| t.pos) {
                Some(pos) => Err(ParserError::Expected(self.error(msg, pos))),
                None => Err(ParserError::EOF),

            }
        }

    }




    fn check(&mut self, token_to_check: TokenType<'a>, msg: &str) -> Result<bool, ParserError<'a>> {
        match self.tokens.next() {
            Some(Token { ref token, ref pos }) => {
                if token == &token_to_check {
                    return Ok(true);
                }

                Err(ParserError::Expected(self.error(msg, *pos)))
            }
            None => Err(ParserError::EOF),
        }
    }

    pub fn parse_single(&mut self) -> Result<Expression<'a>, ParserError<'a>> {
        self.expression()
    }
}


// // Expression Parsing

impl<'a> Parser<'a> {
    fn declaration(&mut self) -> Result<Statement<'a>, ParserError<'a>> {
        unimplemented!()
    }

    fn expression(&mut self) -> Result<Expression<'a>, ParserError<'a>> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expression<'a>,ParserError<'a>> {
        let mut expr = self.comparison()?;

        while self.matched(vec![TokenType::BANGEQUAL,TokenType::EQUALEQUAL], "Expected a \'!\' or a \'==\' ")? {

            let operator = get_operator(self.token_type());

            let right_expr = self.comparison()?;

            expr =  Expression::Binary(Box::new(Binary{
                left_expr:expr,
                operator,
                right_expr
            }));

            println!("token {:#?}",self.tokens);

        }

       Ok(expr)

    }

    fn comparison(&mut self) -> Result<Expression<'a>,ParserError<'a>> {
        
        let mut expr = self.addition()?;
        
        while self.matched(vec![], "Expected either of a \'>\', \'>=\', \'<\' , \'>= \' ")? {
            let operator = get_operator(self.token_type());

            let right_expr = self.addition()?;
            
            expr = Expression::Binary(Box::new(Binary{
                left_expr:expr,
                operator,
                right_expr
            }))
        }

        Ok(expr)
    }

    fn addition(&mut self) -> Result<Expression<'a>,ParserError<'a>> {
        
        let mut expr = self.multiplication()?;
        
        while self.matched(vec![TokenType::MINUS,TokenType::PLUS],"Expected a \'-\' or a \'+\' ")? {
            let operator = get_operator(self.token_type());

            let right_expr = self.multiplication()?;

            expr = Expression::Binary(Box::new(Binary{
                left_expr:expr,
                operator,
                right_expr
            }))
        
        }

        Ok(expr)
    }

    fn multiplication(&mut self) -> Result<Expression<'a>,ParserError<'a>> {
        let mut expr = self.unary()?;

        while self.matched(vec![TokenType::SLASH,TokenType::STAR], "Expected a \' \\ \' or a \'*\'")? {
            let operator = get_operator(self.token_type());

            let right_expr = self.unary()?;

            expr = Expression::Binary(Box::new(Binary{
                left_expr:expr,
                operator,
                right_expr
            }))
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expression<'a>,ParserError<'a>> {
        if self.matched(vec![TokenType::BANG,TokenType::MINUS], "Expected a \' -\' or a \'! \' ")? {
            let operator = get_unary_operator(self.token_type());

            let right = self.unary()?;

            return Ok(Expression::Unary(Box::new(Unary{
                expr:right,
                operator
            })))
        }

        self.primary()

    }


    fn primary(&mut self) -> Result<Expression<'a>,ParserError<'a>> {
       match self.advance() {
           Some(Token{ref token, ref pos}) => {
               match *token {
            TokenType::FALSE(ref f) => Ok(Expression::Literal(Literal::False(f))),
          TokenType::TRUE(ref t)=> Ok(Expression::Literal(Literal::True(t))),
           TokenType::NIL=> Ok(Expression::Literal(Literal::False)),
          TokenType::INT(ref i)=> Ok(Expression::Literal(Literal::False)),
           TokenType::FLOAT(ref f)=> Ok(Expression::Literal(Literal::False)),
           TokenType::STRING(ref s) => Ok(Expression::Literal(Literal::False)),
           TokenType::LPAREN=> Ok(Expression::Literal(Literal::False)),

               }
           }
           
           _ => unimplemented!(),
           None => Err(ParserError::EOF)
       }
    }
    
}
