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
    pub fn new(tokens: Vec<Token<'a>>) -> Self {
        Parser {
            tokens: tokens.into_iter().peekable(),
            loop_depth: 0,
            variable_use_maker: VariableUseMaker::new(),
        }
    }


    fn error(&self, message: &str, pos: Postition) -> String {
        format!("{} on {}", message, pos)
    }

    fn block(&mut self) -> Result<Vec<Statement<'a>>, ParserError<'a>> {
        let mut statements = vec![];

        while self.tokens.peek().map(|t| &t.token) != Some(&TokenType::RBRACE) {
            statements.push(self.declaration()?);
        }

        self.check(TokenType::RBRACE, "Expected '}' after block")?;
        Ok(statements)
    }


fn function_body(&mut self, kind: &str) -> Result<Expression<'a>, ParserError<'a>> {
        self.check(TokenType::LPAREN, "Expected '(' ")?;

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



        self.check(TokenType::RPAREN, "Expected ')' after parameters.")?;


        self.check(TokenType::LBRACE, &format!("Expected '{{' before {} body.", kind))?;

        let body = self.block()?;



        Ok(Expression::Func(Box::new(Func { parameters, body })))
}

  

    fn peek_many(&mut self, tokens: Vec<TokenType<'a>>,msg:&str) -> Result<bool, ParserError<'a>> {
        for token_to_check in tokens {
            match self.tokens.peek() {
                Some(&Token { ref token, ref pos }) => if token == &token_to_check {
                    return Ok(true);
                },
                None => return Err(ParserError::EOFExpected(token_to_check)),
            }
        }

        Ok(false)
    }

   

    fn peek_check(&mut self, token_to_check: TokenType<'a>) -> bool {
        match self.tokens.peek() {
            Some(&Token { ref token, .. }) => {
                if token == &token_to_check {
                    return true;
                }

                false
            }
            None => false,
        }
    }

    fn check_many(&mut self, tokens:Vec<TokenType<'a>>,msg:&str) -> Result<bool,ParserError<'a>> {
        let mut found = false;
        
        for token in tokens {
            if self.peek_check(token){
                found = true;
            }
        }

        println!("{}",found );

        match found {
            true  => Ok(true),
            false => {
                let next_pos = self.tokens.next().map(|t| t.pos).unwrap();

                Err(ParserError::Expected(self.error(msg,next_pos)))
            }

        }
 
    }

    fn check(&mut self, token_to_check: TokenType<'a>,msg:&str) -> Result<bool,ParserError<'a>> {
        match self.tokens.next() {
            Some(Token { ref token, ref pos }) => {
                if token == &token_to_check {
                    return Ok(true);
                }

                Err(ParserError::Expected(self.error(msg,*pos)))
            }
            None => Err(ParserError::EOF)
        }
    }

    pub fn parse_single(&mut self) -> Result<Expression<'a>, ParserError<'a>> {
        self.expression()
    }
}


// // Expression Parsing

impl<'a> Parser<'a> {

    fn declaration(&mut self) -> Result<Statement<'a>,ParserError<'a>> {
        unimplemented!()
    }
    fn expression(&mut self) -> Result<Expression<'a>, ParserError<'a>> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expression<'a>, ParserError<'a>> {
        // assignment → identifier "=" assignment
        //            | logic_or ;
        let expr = self.ternary()?;

        if self.peek_many(
            vec![
                TokenType::ASSIGN,
                TokenType::PLUSASSIGN,
                TokenType::MINUSASSIGN,
                TokenType::STARASSIGN,
            ],
            "Expected one of '=', '-=', '*=' '/='",
        )? {

            let next = self.tokens.next().unwrap();

            let kind = get_assign_type(&next.token);

            let value = self.assignment()?;
            
            match expr {
                Expression::Var(variable, _) => return Ok(Expression::Assign(Box::new(Assign {
                    handle: self.variable_use_maker.next(),
                    name: variable,
                    value,
                    kind,
                }))),

                Expression::Get(ref get) => return Ok(Expression::Set(Box::new(Set {
                    object: get.object.clone(),
                    name: get.name.clone(),
                    value,
                    handle: self.variable_use_maker.next(),
                }))),

                _ => return Err(ParserError::IllegalExpression(self.error(&expr.pprint(),next.pos   )    )),
            }
            
        }


        Ok(expr)
    }

    fn ternary(&mut self) -> Result<Expression<'a>, ParserError<'a>> {
        let mut expr = self.or()?;

        if self.check(TokenType::QUESTION,"Expected '?' after assignment")? {
            let then_branch = self.expression()?;

            self.check(
                        TokenType::COLON,
                        "Expected ':' after lhs ternary condition.",
            )?;

             let else_branch = self.ternary()?;
            
             expr = Expression::Ternary(Box::new(Ternary{
                        condition: expr,
                        then_branch,
                        else_branch,
            }))
        }

        Ok(expr)
    }

    fn or(&mut self) -> Result<Expression<'a>, ParserError<'a>> {
        // logic_or    → logic_and ( "or" logic_and )*
        let mut expr = self.and()?;

        if self.check(TokenType::OR, "Expected the keyword 'or' ")? {
            let operator = LogicOperator::Or;

            let right = self.and()?;

            expr = Expression::Logical(Box::new(Logical {
                        left: expr,
                        operator,
                        right,
                    }))
        }

        Ok(expr)
    }

    fn and(&mut self) -> Result<Expression<'a>, ParserError<'a>> {
        // logic_and   → equality ( "and" equality )*

        let mut expr = self.equality()?;


        if self.check(TokenType::AND, "Expected the keyword 'and' ")? {
            let operator = LogicOperator::And;
            
            let right = self.equality()?;
            expr = Expression::Logical(Box::new(Logical {
                        left: expr,
                        operator,
                        right,
                    }))
        }

       
        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expression<'a>, ParserError<'a>> {
        // equality → comparison ( ( "!=" | "==" ) comparison )*

        let mut expr = self.comparison()?;

        if self.check_many(vec![TokenType::BANGEQUAL,TokenType::EQUALEQUAL], "Expected a '==' or '!=' ")? {
            let right_expr = self.comparison()?;

            let operator = get_operator(self.tokens.next().map(|t| t.token));

             expr = Expression::Binary(Box::new(Binary {
                        left_expr: expr,
                        operator,
                        right_expr: right_expr,
                    }))
        }

        Ok(expr)
}






    fn comparison(&mut self) -> Result<Expression<'a>, ParserError<'a>> {
        // comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )*

        let mut expr = self.term()?;

         if self.check_many(vec![TokenType::LESSTHANEQUAL,TokenType::LESSTHAN,TokenType::GREATERTHAN,TokenType::GREATERTHANEQUAL], "Expected a  '<' or '>' '<=' or '=>' ")? {
            let right_expr = self.term()?;

            let operator = get_operator(self.tokens.next().map(|t| t.token));

             expr = Expression::Binary(Box::new(Binary {
                        left_expr: expr,
                        operator,
                        right_expr,
                    }));
        }

        Ok(expr)
    }


    fn term(&mut self) -> Result<Expression<'a>, ParserError<'a>> {
        //  factor ( ( "-" | "+" ) factor )*

        let mut expr = self.factor()?;
        if self.check_many(vec![TokenType::PLUS,TokenType::MINUS], "Expected a  '-' or '+'")? {
         
            
            let right_expr = self.factor()?;

            let operator = get_operator(self.tokens.next().map(|t| t.token));

             expr = Expression::Binary(Box::new(Binary {
                        left_expr: expr,
                        operator,
                        right_expr,
                    }));
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expression<'a>, ParserError<'a>> {
        // unary  ( "/" | "*" ) unary )

        let mut expr = self.exponent()?;

         if self.check_many(vec![TokenType::SLASH,TokenType::STAR,TokenType::MODULO], "Expected a  '/' or '*' or '%' ")? {
            let right_expr = self.exponent()?;

            let operator = get_operator(self.tokens.next().map(|t| t.token));

             expr = Expression::Binary(Box::new(Binary {
                        left_expr: expr,
                        operator,
                        right_expr,
                    }));
        }

        Ok(expr)
    }

    fn exponent(&mut self) -> Result<Expression<'a>, ParserError<'a>> {
        let mut expr = self.unary()?;

        if self.check(TokenType::EXPONENTIAL,"Expected a '^'")? {
            let right_expr = self.unary()?;

                    expr = Expression::Binary(Box::new(Binary {
                        left_expr: expr,
                        operator: Operator::Exponential,
                        right_expr,
                    }));

        }

      

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expression<'a>, ParserError<'a>> {
        // ( "!" | "-" ) unary | primary

        if self.check(TokenType::EXPONENTIAL,"Expected a '!' or '-' ")? {
            let right_expr = self.unary()?;

                    expr = Expression::Binary(Box::new(Binary {
                        left_expr: expr,
                        operator: Operator::Exponential,
                        right_expr,
                    }));

        }

        match self.tokens.peek().map(|t| &t.token) {
            Some(&TokenType::BANG) => {
                self.tokens.next();


                // Actually consumes the token

                let expr = self.unary()?;

                Ok(Expression::Unary(Box::new(Unary {
                    operator: UnaryOperator::Bang,
                    expr,
                })))
            }

            Some(&TokenType::MINUS) => {
                self.tokens.next();

                let expr = self.unary()?;
                Ok(Expression::Unary(Box::new(Unary {
                    operator: UnaryOperator::Minus,
                    expr,
                })))
            }

            Some(_) => self.call(),

            None => Err(ParserError::EOFMany(
                vec![TokenType::BANG, TokenType::MINUS],
            )),
        }
    }


    fn call(&mut self) -> Result<Expression<'a>, ParserError<'a>> {
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

                        Some(Token { ref pos, .. }) => {
                            return Err(ParserError::Expected(self.error("Expected a class name",*pos)))
                        }

                        None => {
                            let error = "Expected a class name but instead found an EOF".to_owned();
                            return Err(ParserError::EOFExpected(TokenType::CLASS));
                        }
                    };

                    expr = Expression::Get(Box::new(Get {
                        object: expr,
                        name,
                        handle: self.variable_use_maker.next(),
                    }));
                } else {
                    break;
                }
            }
        }

        Ok(expr)
    }



    fn index_expr(&mut self, target: Expression<'a>) -> Result<Expression<'a>, ParserError<'a>> {
        let index = self.expression()?;
        self.check(
            TokenType::RBRACKET,
            "Expected ']' to close an index expression",
        )?;


        Ok(Expression::IndexExpr(Box::new(IndexExpr { target, index })))
    }


    fn finish_call(&mut self, callee: Expression<'a>) -> Result<Expression<'a>, ParserError<'a>> {
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

        self.check(TokenType::RPAREN, "Expected ')' after arguments.")?;

        Ok(Expression::Call(Box::new(Call { callee, arguments })))
    }


    fn primary(&mut self) -> Result<Expression<'a>, ParserError<'a>> {
        // NUMBER | STRING | "false" | "true" | "nil" | "(" expression ")"

        match self.tokens.next() {
            Some(Token { ref token, ref pos }) => match *token {
                TokenType::INT(i) => Ok(Expression::Literal(Literal::Int(i))),

                TokenType::STRING(ref string) => {
                    Ok(Expression::Literal(Literal::Str(string.to_owned())))
                }

                TokenType::TRUE(_) => Ok(Expression::Literal(Literal::True(true))),

                TokenType::FALSE(_) => Ok(Expression::Literal(Literal::False(false))),

                TokenType::NIL => Ok(Expression::Literal(Literal::Nil)),

                TokenType::FUNCTION => self.function_body("function"),

                TokenType::IDENTIFIER(ref s) => Ok(Expression::Var(
                    Variable(s.to_owned()),
                    self.variable_use_maker.next(),
                )),
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

                    self.check(
                        TokenType::RBRACKET,
                        "Expected a ']' to close the brackets .",
                    )?;


                    Ok(Expression::Array(Array { items }))
                }
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
                        self.check(TokenType::COLON, "Expected a ':' after dict key ")?;
                        let right = self.expression()?;

                        items.push((left, right));
                        self.tokens.peek().map(|t| &t.token) == Some(&TokenType::COMMA)
                            && self.tokens.next().map(|t| t.token) == Some(TokenType::COMMA)
                    } {}


                    self.check(TokenType::RBRACE, "Expected a '}' to close a dictionary.")?;

                    Ok(Expression::Dict(Dictionary { items }))
                }

                TokenType::LPAREN => {
                    let expr = match self.expression() {
                        Ok(expression) => expression,
                        Err(e) => return Err(e),
                    };

                    self.check(TokenType::RPAREN, "Expected a ')' to close the brackets.")?;


                    Ok(Expression::Grouping(Box::new(Grouping { expr })))
                }

                _ => Err(ParserError::IllegalExpression(self.error("Illegal Expression",*pos))),
            },

            None => Err(ParserError::EOF),
        }
    }
}
