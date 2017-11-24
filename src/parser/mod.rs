use token::{Token,TokenType};
use std::iter::Peekable;
use std::vec::IntoIter;
use pos::Postition;
use ast::expr::*;
use ast::statement::*;

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
        IllegalExpression(TokenType<'a>,Postition),
        EOF,
        EOFExpected(TokenType<'a>),
        EOFMany(Vec<TokenType<'a>>),
        MissingCloseBracket,
        Expected(TokenType<'a>,TokenType<'a>,Postition),
        Break(String),
    }

    impl <'a> Display for ParserError<'a> {
        fn fmt(&self, f: &mut Formatter) -> fmt::Result {
            match *self {
                ParserError::NotExpected(ref e) |
                ParserError::IllegalToken(ref e) |
                ParserError::Break(ref e) => write!(f, "{}", e),
                ParserError::MissingCloseBracket => write!(f, " ')' was expected but not found "),
                ParserError::EOF => write!(f,"Unexpected end of file"),
                ParserError::EOFExpected(ref e) => write!(f,"Expected {} but instead found the EOF",e),
                ParserError::EOFMany(ref many) => write!(f,"Expected {:?} but instead found EOF",many),
                ParserError::IllegalExpression(ref e,ref pos) => write!(f,"Illegal Expression {} on {}",e,pos),
                ParserError::Expected(ref expected,ref got,ref pos) => write!(f,"Expected {} but instead got {} on {}",expected,got,pos)
            }
        }
    }


impl<'a> Parser<'a> {
    fn new(tokens: Vec<Token<'a>>) -> Self {
        Parser {
            tokens: tokens.into_iter().peekable(),
             loop_depth: 0,
            variable_use_maker: VariableUseMaker::new(),
        }
    }


    fn error(self,message: &str,pos:Postition) -> String {
        format!("{} on {}", message,pos)
    }


   fn advance_check(
        &mut self,
        token_to_check: TokenType<'a>,
        message: &str,
    ) -> Result<(), ParserError<'a>> {
        match self.tokens.next().map(|t| t) {
            Some(Token {
                token,
                pos,
            }) => {
                if token == token_to_check {
                    return Ok(());
                }

                Err(ParserError::Expected(token,token_to_check,pos))
            },

            None => Err(ParserError::EOFExpected(token_to_check)),
        }
    }

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

    fn parse_single(&mut self) -> Result<Expression<'a>,ParserError<'a>>{
        self.expression()
    }

    

}






// // Expression Parsing

impl <'a> Parser<'a> {
    fn expression(&mut self) -> Result<Expression<'a>,ParserError<'a>> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expression<'a>,ParserError<'a>> {
        let expr = self.ternary()?;

        match self.tokens.peek().map(|t| &t.token) {
            token @ Some(&TokenType::ASSIGN) |
            token @ Some(&TokenType::PLUSASSIGN) |
            token @ Some(&TokenType::MINUSASSIGN) |
            token @Some(&TokenType::STARASSIGN) => {
                let kind = get_assign_type(&self.tokens.next().map(|t| t.token));

                let value = self.equality()?;

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

            None => Err(ParserError::EOFMany(vec![
                TokenType::ASSIGN,
                TokenType::PLUSASSIGN,
                TokenType::MINUSASSIGN,
                TokenType::STARASSIGN
            ])),
            Some(_) => Ok(expr),
        }
    }

    fn ternary(&mut self) -> Result<Expression<'a>,ParserError<'a>> {
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

    fn or(&mut self) -> Result<Expression<'a>, ParserError<'a>> {
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
                None => return Err(ParserError::EOF),
                Some(token) => return Err(ParserError::IllegalExpression(token,pos)),
            }
        }

        Ok(expr)
    }

    fn and(&mut self) -> Result<Expression<'a>, ParserError<'a>> {
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



    fn comparison(&mut self) -> Result<Expression<'a>,ParserError<'a>> {
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


    fn term(&mut self) -> Result<Expression<'a>,ParserError<'a>> {
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

    fn factor(&mut self) -> Result<Expression<'a>,ParserError<'a>> {
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

    fn exponent(&mut self) -> Result<Expression<'a>,ParserError<'a>> {
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

    fn unary(&mut self) -> Result<Expression<'a>,ParserError<'a>> {
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

            None => Err(ParserError::EOFMany(vec![
                TokenType::BANG,
                TokenType::MINUS
            ])),
        }
    }


    fn call(&mut self) -> Result<Expression<'a>,ParserError<'a>> {
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
                            ref pos,
                            ..
                        }) => {
                            return Err(ParserError::Expected(
                                format!("Expected a class name on line {} column {}", line, column),
                            ))
                        },

                        None => {
                            let error = "Expected a class name but instead found an EOF".to_owned();
                            return Err(ParserError::EOFExpected(TokenType::CLASS));
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



    fn index_expr(&mut self, target: Expression<'a>) -> Result<Expression<'a>,ParserError<'a>> {
        let index = self.expression()?;
        self.advance_check(TokenType::RBRACKET, "Expected ']' to close an index expression")?;


        Ok(Expression::IndexExpr(Box::new(IndexExpr { target, index })))
    }


    fn finish_call(&mut self, callee: Expression<'a>) -> Result<Expression<'a>,ParserError<'a>> {
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


    fn primary(&mut self) -> Result<Expression<'a>,ParserError<'a>> {
        // NUMBER | STRING | "false" | "true" | "nil" | "(" expression ")"



        match self.tokens.next() {
            Some(Token {
                ref token,
                ref pos,
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
                    

                    Err(ParserError::IllegalExpression(*token,*pos))
                },
            },

            None => Err(ParserError::EOF),
        }
    }


}

