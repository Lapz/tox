mod test;
use token::{Token, TokenType};
use std::iter::Peekable;
use std::vec::IntoIter;
use super::ast::expr::*;
use super::ast::statement::*;
use util::pos::{Position, Span, Spanned};
use util::emmiter::Reporter;
use symbol::{Symbol, Table};

#[derive(Debug)]
pub struct Parser<'a> {
    tokens: Peekable<IntoIter<Spanned<Token<'a>>>>,
    reporter: Reporter,
    loop_depth: i32,
    pub symbols: &'a mut Table<()>,
    parsing_cond: bool,
    variable_use_maker: VariableUseMaker,
}

pub type ParserResult<T> = Result<T, ()>;

/// Macro that is used to generate the code that parse a binary op
macro_rules! binary {
    ($_self:ident,$e:ident,$lhs:expr,$func:ident) => {
        while $_self.recognise($e) {
            let op = $_self.get_binary_op()?;

            let rhs = Box::new($_self.$func()?);

           $lhs = Spanned {
                span: $lhs.get_span().to(rhs.get_span()),
                value: Expression::Binary {
                    lhs: Box::new($lhs),
                    op,
                    rhs,
                },
            }
        }
    };

    ($_self:ident,$expr:expr, $lhs:expr,$func:ident) => {
        while $_self.matched($expr) {
            let op = $_self.get_binary_op()?;

            let rhs = Box::new($_self.$func()?);

           $lhs = Spanned {
                span: $lhs.get_span().to(rhs.get_span()),
                value: Expression::Binary {
                    lhs: Box::new($lhs),
                    op,
                    rhs,
                },
            }
        }
    };
}
/// Macro that expands to a match that takes a `TokenType` and turns it into a Operator
macro_rules! get_op {
    ($_self:ident,{ $($p:ident => $t:ident),*}) => {
        {
            match $_self.advance() {
                 $(Some(Spanned{
                    value: Token {
                        token:TokenType::$p,
                    },
                    ref span,
                }) => {
                    Ok(Spanned {
                    span: *span,
                    value: Op::$t,
                    })
                },)+

                Some(Spanned {
                value: Token { ref token },
                ref span,
            }) => {
                let msg = format!(
                    "Expected one of '!' '+' '-' '/''*' '=' '<' '>' '<=' '=>' but instead found {}",
                    token
                );

                $_self.error(msg, *span);

                Err(())
            }


            None => {
                $_self.reporter.global_error("Unexpected EOF");
                    Err(())
                }
            }
        }

    }
}

// The unary version of the get_binary_op macro
macro_rules! get_unary_op {
    ($_self:ident,{ $($p:ident => $t:ident),*}) => {
        {
            match $_self.advance() {
                 $(Some(Spanned{
                    value: Token {
                        token:TokenType::$p,
                    },
                    ref span,
                }) => {
                    Ok(Spanned {
                    span: *span,
                    value: UnaryOp::$t,
                    })
                },)+

                Some(Spanned {
                value: Token { ref token },
                ref span,
            }) => {
                let msg = format!(
                    "Expected one  '!' or '-' but instead found {}",
                    token
                );

                $_self.error(msg, *span);

                Err(())
            }


            None => {
                $_self.reporter.global_error("Unexpected EOF");
                    Err(())
                }
            }
        }

    }
}

macro_rules! get_assign_op {
    ($_self:ident,{ $($p:ident => $t:ident),*}) => {
        {
            match $_self.advance() {
                 $(Some(Spanned{
                    value: Token {
                        token:TokenType::$p,
                    },
                    ref span,
                }) => {
                    Ok(Spanned {
                    span: *span,
                    value: AssignOperator::$t,
                    })
                },)+

                Some(Spanned {
                value: Token { ref token },
                ref span,
            }) => {
                let msg = format!(
                    "Expected one  '+=','-=','*=','/='  but instead found {}",
                    token
                );

                $_self.error(msg, *span);

                Err(())
            }


            None => {
                $_self.reporter.global_error("Unexpected EOF");
                    Err(())
                }
            }
        }

    }
}

impl<'a> Parser<'a> {
    pub fn new(
        tokens: Vec<Spanned<Token<'a>>>,
        reporter: Reporter,
        symbols: &'a mut Table<()>,
    ) -> Self {
        Parser {
            tokens: tokens.into_iter().peekable(),
            symbols,
            loop_depth: 0,
            parsing_cond: false,
            reporter,
            variable_use_maker: VariableUseMaker::new(),
        }
    }

    // pub fn parse(&mut self) -> Result<Vec<Spanned<Statement>>, Vec<ParserError>> {
    //     let mut statements = vec![];

    //     let mut errors = vec![];

    //     while self.peek(|token| token != &TokenType::EOF) {
    //         match self.declaration() {
    //             Ok(statement) => statements.push(statement),
    //             Err(e) => {
    //                 errors.push(e);
    //                 self.synchronize();
    //             }
    //         }
    //     }

    //     if errors.is_empty() {
    //         Ok(statements)
    //     } else {
    //         Err(errors)
    //     }
    // }

    // pub fn synchronize(&mut self) {
    //     self.advance();

    //     while self.peek(|token| token == &TokenType::EOF) {
    //         match self.advance().map(|t| t.token) {
    //             Some(TokenType::CLASS)
    //             | Some(TokenType::FUNCTION)
    //             | Some(TokenType::IDENTIFIER(_))
    //             | Some(TokenType::FOR)
    //             | Some(TokenType::IF)
    //             | Some(TokenType::WHILE)
    //             | Some(TokenType::RETURN) => break,
    //             None => unreachable!(),
    //             _ => self.advance(),
    //         };
    //     }
    // }

    fn error<T: Into<String>>(&mut self, msg: T, span: Span) {
        self.reporter.error(msg, span)
    }

    fn peek<F>(&mut self, mut check: F) -> bool
    where
        F: FnMut(&TokenType<'a>) -> bool,
    {
        self.tokens
            .peek()
            .map_or(false, |span| check(&span.value.token))
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

        found
    }

    fn advance(&mut self) -> Option<Spanned<Token<'a>>> {
        self.tokens.next()
    }

    pub fn consume(&mut self, token_to_check: &TokenType<'a>, msg: &str) -> ParserResult<()> {
        match self.advance() {
            Some(Spanned {
                ref span,
                value: Token { ref token },
            }) => {
                if token == token_to_check {
                    return Ok(());
                }

                let msg = format!("{} but instead found {}", msg, token);

                self.error(msg, *span);

                Err(())
            }
            None => {
                self.reporter.global_error("Unexpected EOF");
                Err(())
            }
        }
    }

    fn consume_get_span(
        &mut self,
        token_to_check: &TokenType<'a>,
        msg: &str,
    ) -> ParserResult<Span> {
        match self.advance() {
            Some(Spanned {
                value: Token { ref token },
                ref span,
            }) => {
                if token == token_to_check {
                    return Ok(*span);
                }

                let msg = format!("{} but instead found {}", msg, token);

                self.error(msg, *span);

                Err(())
            }
            None => {
                self.reporter.global_error("Unexpected EOF");
                Err(())
            }
        }
    }

    pub fn consume_get_symbol(&mut self, msg: &str) -> ParserResult<Spanned<Symbol>> {
        match self.advance() {
            Some(Spanned {
                value:
                    Token {
                        token: TokenType::IDENTIFIER(ident),
                    },
                ref span,
            }) => Ok(Spanned {
                span: *span,
                value: self.symbols.symbol(ident),
            }),
            Some(Spanned {
                value: Token { ref token },
                ref span,
            }) => {
                let msg = format!("{} but instead found {}", msg, token);

                self.error(msg, *span);

                Err(())
            }
            None => {
                self.reporter.global_error("Unexpected EOF");
                Err(())
            }
        }
    }

    fn consume_get_symbol_and_span(&mut self, msg: &str) -> ParserResult<(Span, Spanned<Symbol>)> {
        match self.advance() {
            Some(Spanned {
                value:
                    Token {
                        token: TokenType::IDENTIFIER(ident),
                    },
                ref span,
            }) => Ok((
                *span,
                Spanned {
                    span: *span,
                    value: self.symbols.symbol(ident),
                },
            )),
            Some(Spanned {
                value: Token { ref token },
                ref span,
            }) => {
                let msg = format!("{} but instead found {}", msg, token);

                self.error(msg, *span);

                Err(())
            }
            None => {
                self.reporter.global_error("Unexpected EOF");
                Err(())
            }
        }
    }

    fn get_unary_op(&mut self) -> ParserResult<Spanned<UnaryOp>> {
        get_unary_op!(self,{
            BANG => Bang,
            MINUS => Minus
        })
    }

    fn get_binary_op(&mut self) -> ParserResult<Spanned<Op>> {
        get_op!(self, {
            AND => And,
            OR => Or,
            GREATERTHAN =>  GreaterThan,
            LESSTHAN => LessThan,
            GREATERTHANEQUAL => GreaterThanEqual,
            LESSTHANEQUAL => LessThanEqual,
            PLUS => Plus,
            MINUS => Minus,
            STAR => Star,
            SLASH => Slash,
            EQUALEQUAL => EqualEqual,
            MODULO => Modulo
        })
    }

    // fn get_type(&mut self) -> Result<Option<Ty>, ParserError> {
    //     if self.recognise(TokenType::COLON) {
    //         self.advance();

    //         let ty = self.parse_type()?;

    //         Ok(Some(ty))
    //     } else {
    //         Ok(None)
    //     }
    // }
}

impl<'a> Parser<'a> {
    fn declaration(&mut self) -> ParserResult<Spanned<Statement>> {
        if self.recognise(TokenType::VAR) {
            self.var_declaration()
        } else if self.recognise(TokenType::FUNCTION) {
            self.function("function")
        } else if self.recognise(TokenType::CLASS) {
            self.class_declaration()
        } else if self.recognise(TokenType::TYPE) {
            self.type_declaration()
        } else {
            self.statement()
        }
    }

    fn statement(&mut self) -> ParserResult<Spanned<Statement>> {
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
        } else if self.recognise(TokenType::PRINT) {
            self.print_statement()
        } else {
            self.expression_statement()
        }
    }

    fn block(&mut self) -> ParserResult<Spanned<Statement>> {
        let open_span = self.consume_get_span(&TokenType::LBRACE, "Expected a '{' ")?;

        let mut statements = vec![];

        while !self.recognise(TokenType::RBRACE) {
            statements.push(self.statement()?);
        }

        let close_span =
            self.consume_get_span(&TokenType::RBRACE, "Expected a \'}\' after block.")?;
        Ok(Spanned {
            span: open_span.to(close_span),
            value: Statement::Block(statements),
        })
    }

    fn break_statement(&mut self) -> ParserResult<Spanned<Statement>> {
        self.consume_get_span(&TokenType::BREAK, "Expected a 'break' ")?;
        Ok(Spanned {
            value: Statement::Break,
            span: self.consume_get_span(&TokenType::SEMICOLON, "Expected ';' ")?,
        })
    }

    fn continue_statement(&mut self) -> ParserResult<Spanned<Statement>> {
        self.consume_get_span(&TokenType::CONTINUE, "Expected 'continue' ")?;
        Ok(Spanned {
            value: Statement::Continue,
            span: self.consume_get_span(&TokenType::SEMICOLON, "Expected ';' ")?,
        })
    }

    fn expression_statement(&mut self) -> ParserResult<Spanned<Statement>> {
        let expr = self.expression()?;

        Ok(Spanned {
            span: self.consume_get_span(&TokenType::SEMICOLON, "Expected ';' ")?,
            value: Statement::Expr(expr),
        })
    }

    fn print_statement(&mut self) -> ParserResult<Spanned<Statement>> {
        let open_span = self.consume_get_span(&TokenType::PRINT, "Expected 'print' ")?;

        let expr = self.expression()?;

        Ok(Spanned {
            span: open_span.to(self.consume_get_span(&TokenType::SEMICOLON, "Expected ';' ")?),
            value: Statement::Expr(expr),
        })
    }

    fn return_statement(&mut self) -> ParserResult<Spanned<Statement>> {
        let open_span = self.consume_get_span(&TokenType::RETURN, "Expected 'return' ")?;

        let expr = self.expression()?;

        let close_span = self.consume_get_span(&TokenType::SEMICOLON, "Expected ';' ")?;

        Ok(Spanned {
            span: open_span.to(close_span),
            value: Statement::Expr(expr),
        })
    }

    fn while_statement(&mut self) -> ParserResult<Spanned<Statement>> {
        let open_span = self.consume_get_span(&TokenType::WHILE, "Expected 'while' ")?;

        self.parsing_cond = true;
        let cond = self.expression()?;
        self.parsing_cond = false;

        let body = self.statement()?;

        Ok(Spanned {
            span: open_span.to(body.get_span()),
            value: Statement::While {
                cond,
                body: Box::new(body),
            },
        })
    }

    fn do_statement(&mut self) -> ParserResult<Spanned<Statement>> {
        let open_span = self.consume_get_span(&TokenType::DO, "Expected 'do' ")?;

        let body = self.statement()?;

        self.consume(&TokenType::WHILE, "Expected while after 'do' condition")?;

        let cond = self.expression()?;

        Ok(Spanned {
            span: open_span.to(body.get_span()),
            value: Statement::While {
                cond,
                body: Box::new(body),
            },
        })
    }

    fn if_statement(&mut self) -> ParserResult<Spanned<Statement>> {
        let open_span = self.consume_get_span(&TokenType::IF, "Expected 'if' ")?;

        self.parsing_cond = true;
        let cond = self.expression()?;
        self.parsing_cond = false;

        let then = Box::new(self.statement()?);

        let (close_span, otherwise) = if self.recognise(TokenType::ELSE) {
            self.advance();

            let otherwise = self.statement()?;

            (Some(otherwise.get_span()), Some(Box::new(otherwise)))
        } else {
            (None, None)
        };

        Ok(Spanned {
            span: open_span.to(close_span.unwrap_or(open_span)),
            value: Statement::If {
                cond,
                then,
                otherwise,
            },
        })
    }

    fn for_statement(&mut self) -> ParserResult<Spanned<Statement>> {
        let open_span = self.consume_get_span(&TokenType::FOR, "Expected 'for' ")?;

        self.consume(&TokenType::LPAREN, "Expected '(' after 'for'")?;

        let mut init = None;

        if self.recognise(TokenType::SEMICOLON) {
            self.advance();
        } else if self.recognise(TokenType::VAR) {
            init = Some(Box::new(self.var_declaration()?));
        } else {
            init = Some(Box::new(self.expression_statement()?));
        }

        let cond = if !self.recognise(TokenType::SEMICOLON) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(&TokenType::SEMICOLON, "Expected ';' after loop condition .")?;

        let incr = if !self.recognise(TokenType::RPAREN) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(&TokenType::RPAREN, "Expected ')' after for clauses.")?;

        let body = self.statement()?;

        Ok(Spanned {
            span: open_span.to(body.get_span()),
            value: Statement::For {
                init,
                cond,
                incr,
                body: Box::new(body),
            },
        })
    }

    fn function(&mut self, kind: &str) -> ParserResult<Spanned<Statement>> {
        let open_span = self.consume_get_span(&TokenType::FUNCTION, "Expected 'function' ")?;

        let name = self.consume_get_symbol(&format!("Expected a {} name", kind))?;

        let body = self.fun_body(kind)?;

        Ok(Spanned {
            span: open_span.to(body.get_span()),
            value: Statement::Function { name, body },
        })
    }

    fn fun_body(&mut self, kind: &str) -> ParserResult<Spanned<Expression>> {
        let open_span = self.consume_get_span(&TokenType::LPAREN, "Expected '(' ")?;

        let mut params = Vec::with_capacity(32);
        let mut returns = None;

        if !self.recognise(TokenType::RPAREN) {
            loop {
                if params.len() >= 32 {
                    self.error("Too many params", open_span);
                    break;
                };

                let (open_span, name) = self.consume_get_symbol_and_span("Expected a param name")?;

                self.consume(&TokenType::COLON, "Expected a colon")?;

                let ty = self.parse_type()?;

                params.push(Spanned {
                    span: open_span.to(ty.get_span()),
                    value: FunctionParams { name, ty },
                });

                if self.recognise(TokenType::COMMA) {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        let rparen_span =
            self.consume_get_span(&TokenType::RPAREN, "Expected a ')' after function params")?;

        if self.recognise(TokenType::FRETURN) {
            self.advance();

            returns = Some(self.parse_type()?);
        }

        let body = Box::new(self.block()?);

        Ok(Spanned {
            span: open_span.to(body.get_span()),
            value: Expression::Func {
                params: Spanned {
                    span: open_span.to(rparen_span),
                    value: params,
                },
                body,
                returns,
            },
        })
    }

    fn type_declaration(&mut self) -> ParserResult<Spanned<Statement>> {
        let open_span = self.consume_get_span(&TokenType::TYPE, "Expected 'type' ")?;

        let alias = self.consume_get_symbol("Expected an identifier")?;

        self.consume(&TokenType::ASSIGN, "Expected '=' ")?;

        let ty = self.parse_type()?;

        let close_span = self.consume_get_span(&TokenType::SEMICOLON, "Expected ';' ")?;

        Ok(Spanned {
            span: open_span.to(close_span),
            value: Statement::TypeAlias { alias, ty },
        })
    }

    fn parse_type(&mut self) -> ParserResult<Spanned<Ty>> {
        unimplemented!();
        /* if self.recognise(TokenType::NIL) {
            Ok(Spanned {
                value: Ty::Nil,
                span: self.consume_get_span(&TokenType::NIL, "Expected 'nil' ")?,
            })
        } else if self.recognise(TokenType::LBRACKET) {
            self.advance();
            let ty = self.parse_type()?;
            self.consume(
                &TokenType::RBRACKET,
                "Expected a \']\' to close an array type",
            )?;

            Ok(Ty::Arr(Box::new(ty)))
        } else if self.recognise(TokenType::FUNCTION) {
            self.advance();
            self.consume(&TokenType::LPAREN, "Expected a \'(\'")?;
            let mut param_ty = Vec::with_capacity(32);

            while {
                let ty = self.parse_type()?;

                param_ty.push(ty);

                self.recognise(TokenType::COMMA)
                    && self.advance().map(|t| t.token) == Some(TokenType::COMMA)
            } {}

            self.consume(&TokenType::RPAREN, "Expected  \')\'")?;

            if self.recognise(TokenType::FRETURN) {
                self.advance();
                let ty = self.parse_type()?;
                Ok(Ty::Func(param_ty, Some(Box::new(ty))))
            } else {
                Ok(Ty::Func(param_ty, None))
            }
        } else {
            let ty = self.consume_get_symbol("Expected a type param")?;
            Ok(Ty::Simple(ty))
        } */
    }

    fn class_declaration(&mut self) -> ParserResult<Spanned<Statement>> {
        let struct_span = self.consume_get_span(&TokenType::CLASS, "Expected 'class' ")?;

        let name = self.consume_get_symbol("Expected a class name")?;

        let superclass = if self.recognise(TokenType::LESSTHAN) {
            self.advance();
            Some(self.consume_get_symbol("Expected a superclass name")?)
        } else {
            None
        };

        let mut properties = vec![];
        let mut methods = vec![];
        let open_span =
            self.consume_get_span(&TokenType::LBRACE, "Expected a '{' after class name")?;

        if !self.recognise(TokenType::RBRACE) {
            loop {
                if !self.recognise(TokenType::FUNCTION) {
                    loop {
                        let (open_span, name) =
                            self.consume_get_symbol_and_span("Expected a property name")?;

                        self.consume(&TokenType::COLON, "Expected ':'")?;

                        let ty = self.parse_type()?;

                        properties.push(Spanned {
                            span: open_span.to(ty.get_span()),
                            value: Field { name, ty },
                        });

                        if self.recognise(TokenType::COMMA) {
                            self.advance();
                        } else {
                            break;
                        }
                    }

                    self.consume(
                        &TokenType::SEMICOLON,
                        "Expected a semicolon after declaring properties",
                    )?;

                    if self.recognise(TokenType::RBRACE) {
                        break;
                    }
                }

                methods.push(self.function("method")?);
            }
        }

        let close_span = self.consume_get_span(&TokenType::RBRACE, "Expected '}' ")?;

        Ok(Spanned {
            span: struct_span.to(close_span),
            value: Statement::Class {
                name,
                superclass,
                body: Spanned {
                    span: open_span.to(close_span),
                    value: (methods, properties),
                },
            },
        })
    }

    fn var_declaration(&mut self) -> ParserResult<Spanned<Statement>> {
        let open_span = self.consume_get_span(&TokenType::VAR, "Expected 'let' ")?;

        let ident = self.consume_get_symbol("Expected an IDENTIFIER after a 'var' ")?;

        let ty = if self.recognise(TokenType::COLON) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };

        self.consume(&TokenType::ASSIGN, "Expected '='")?;

        let expr = if self.recognise(TokenType::SEMICOLON) {
            None
        } else {
            Some(self.expression()?)
        };

        let close_span = self.consume_get_span(&TokenType::SEMICOLON, "Expected ';'")?;

        Ok(Spanned {
            span: open_span.to(close_span),
            value: Statement::Var { ident, ty, expr },
        })
    }
}

impl<'a> Parser<'a> {
    fn expression(&mut self) -> ParserResult<Spanned<Expression>> {
        unimplemented!()
    }
}
/*
// Expression Parsing
impl<'a> Parser<'a> {
    fn expression(&mut self) -> Result<Spanned<Expression>, ParserError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Spanned<Expression>, ParserError> {
        let expr = self.ternary()?;

        if self.matched(vec![
            TokenType::ASSIGN,
            TokenType::PLUSASSIGN,
            TokenType::MINUSASSIGN,
            TokenType::STARASSIGN,
            TokenType::SLASHASSIGN,
        ]) {
            let next = self.advance().unwrap();
            let kind = get_assign_operator(&next.token);

            let value = self.assignment()?;

            match expr.node {
                Expression::Var(name, _) => {
                    return Ok(Spanned::new(
                        Expression::Assign {
                            handle: self.variable_use_maker.next(),
                            name,
                            value: Box::new(value),
                            kind,
                        },
                        next.pos,
                    ))
                }

                Expression::Get {
                    object, property, ..
                } => {
                    return Ok(Spanned::new(
                        Expression::Set {
                            object,
                            name: property,
                            value: Box::new(value),
                            handle: self.variable_use_maker.next(),
                        },
                        next.pos,
                    ));
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

    fn ternary(&mut self) -> Result<Spanned<Expression>, ParserError> {
        let mut condition = self.or()?;

        while self.matched(vec![TokenType::QUESTION]) {
            let ternary_pos = self.consume_get_pos(&TokenType::QUESTION, "Expected a '?'")?;

            let then_branch = Box::new(self.expression()?);

            self.consume(
                &TokenType::COLON,
                "Expected ':' after lhs ternary condition.",
            )?;

            let else_branch = Box::new(self.ternary()?);

            condition = Spanned::new(
                Expression::Ternary {
                    condition: Box::new(condition),
                    then_branch,
                    else_branch,
                },
                ternary_pos,
            )
        }

        Ok(condition)
    }

    fn or(&mut self) -> Result<Spanned<Expression>, ParserError> {
        let mut expr = self.and()?;

        while self.recognise(TokenType::OR) {
            let next = self.advance().unwrap();

            let operator = get_logic_operator(&next.token);

            let right = Box::new(self.and()?);

            expr = Spanned::new(
                Expression::Logical {
                    left: Box::new(expr),
                    operator,
                    right,
                },
                next.pos,
            )
        }

        Ok(expr)
    }

    fn and(&mut self) -> Result<Spanned<Expression>, ParserError> {
        let mut expr = self.equality()?;

        while self.recognise(TokenType::AND) {
            let next = self.advance().unwrap();

            let operator = get_logic_operator(&next.token);

            let right = Box::new(self.equality()?);

            expr = Spanned::new(
                Expression::Logical {
                    left: Box::new(expr),
                    operator,
                    right,
                },
                next.pos,
            )
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Spanned<Expression>, ParserError> {
        let mut expr = self.comparison()?;

        while self.matched(vec![TokenType::BANGEQUAL, TokenType::EQUALEQUAL]) {
            let next = self.advance().unwrap();

            let operator = get_operator(&next.token);

            let right_expr = Box::new(self.comparison()?);

            expr = Spanned::new(
                Expression::Binary {
                    left_expr: Box::new(expr),
                    operator,
                    right_expr,
                },
                next.pos,
            );
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Spanned<Expression>, ParserError> {
        let mut expr = self.addition()?;

        while self.matched(vec![
            TokenType::LESSTHAN,
            TokenType::LESSTHANEQUAL,
            TokenType::GREATERTHAN,
            TokenType::GREATERTHANEQUAL,
        ]) {
            let next = self.advance().unwrap();

            let operator = get_operator(&next.token);

            let right_expr = Box::new(self.addition()?);

            expr = Spanned::new(
                Expression::Binary {
                    left_expr: Box::new(expr),
                    operator,
                    right_expr,
                },
                next.pos,
            )
        }

        Ok(expr)
    }

    fn addition(&mut self) -> Result<Spanned<Expression>, ParserError> {
        let mut expr = self.multiplication()?;

        while self.matched(vec![TokenType::MINUS, TokenType::MODULO, TokenType::PLUS]) {
            let next = self.advance().unwrap();

            let operator = get_operator(&next.token);

            let right_expr = Box::new(self.multiplication()?);

            expr = Spanned::new(
                Expression::Binary {
                    left_expr: Box::new(expr),
                    operator,
                    right_expr,
                },
                next.pos,
            )
        }

        Ok(expr)
    }

    fn multiplication(&mut self) -> Result<Spanned<Expression>, ParserError> {
        let mut expr = self.unary()?;

        while self.matched(vec![TokenType::SLASH, TokenType::STAR]) {
            let next = self.advance().unwrap();

            let operator = get_operator(&next.token);

            let right_expr = Box::new(self.unary()?);

            expr = Spanned::new(
                Expression::Binary {
                    left_expr: Box::new(expr),
                    operator,
                    right_expr,
                },
                next.pos,
            )
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Spanned<Expression>, ParserError> {
        if self.matched(vec![TokenType::BANG, TokenType::MINUS]) {
            let next = self.advance().unwrap();

            let operator = get_unary_operator(&next.token);

            let right = Box::new(self.unary()?);

            return Ok(Spanned::new(
                Expression::Unary {
                    expr: right,
                    operator,
                },
                next.pos,
            ));
        }

        self.call()
    }

    fn call(&mut self) -> Result<Spanned<Expression>, ParserError> {
        let mut expr = self.primary()?;

        loop {
            if self.recognise(TokenType::LBRACKET) {
                self.advance();
                let index = Box::new(self.expression()?);
                let index_pos = self.consume_get_pos(
                    &TokenType::RBRACKET,
                    "Expected ']' to close an index expression",
                )?;
                return Ok(Spanned::new(
                    Expression::IndexExpr {
                        target: Box::new(expr),
                        index,
                    },
                    index_pos,
                ));
            } else if self.recognise(TokenType::LPAREN) {
                let call_pos = self.advance().unwrap().pos;
                expr.pos = call_pos;
                expr = self.finish_call(expr)?;
            } else if self.recognise(TokenType::DOT) {
                self.advance();

                let (property, pos) = self.consume_get_name_symbol("Expected a \'class\' name")?;
                expr = Spanned::new(
                    Expression::Get {
                        object: Box::new(expr),
                        property,
                        handle: self.variable_use_maker.next(),
                    },
                    pos,
                )
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn primary(&mut self) -> Result<Spanned<Expression>, ParserError> {
        match self.advance() {
            Some(Token { ref token, ref pos }) => match *token {
                TokenType::FALSE(_) => Ok(Spanned::new(
                    Expression::Literal(Literal::False(false)),
                    *pos,
                )),
                TokenType::TRUE(_) => {
                    Ok(Spanned::new(Expression::Literal(Literal::True(true)), *pos))
                }
                TokenType::NIL => Ok(Spanned::new(Expression::Literal(Literal::Nil), *pos)),
                TokenType::INT(ref i) => {
                    Ok(Spanned::new(Expression::Literal(Literal::Int(*i)), *pos))
                }
                TokenType::FLOAT(ref f) => {
                    Ok(Spanned::new(Expression::Literal(Literal::Float(*f)), *pos))
                }
                TokenType::STRING(ref s) => Ok(Spanned::new(
                    Expression::Literal(Literal::Str(s.clone())),
                    *pos,
                )),
                TokenType::IDENTIFIER(ident) => {
                    if self.recognise(TokenType::LBRACE) {
                        self.advance();
                        let mut properties = vec![];

                        if self.recognise(TokenType::RBRACE) {
                            self.advance();
                            return Ok(Spanned::new(
                                Expression::ClassInstance {
                                    properties,
                                    name: self.symbols.symbol(ident),
                                },
                                *pos,
                            ));
                        }

                        while {
                        let property_name = self.consume_get_symbol("Expected an Property name ")?;

                            self.consume(
                                &TokenType::COLON,
                                "Expected a colon after a class property name",
                            )?;

                            let property_value = self.expression()?;

                            properties.push((property_name, property_value));

                            self.recognise(TokenType::COMMA)
                                && self.advance().map(|t| t.token) == Some(TokenType::COMMA)
                        } {}

                        self.consume(
                            &TokenType::RBRACE,
                            "Expected a \'}\' to close a Class Instance",
                        )?;

                        return Ok(Spanned::new(
                            Expression::ClassInstance {
                                properties,
                                name: self.symbols.symbol(ident),
                            },
                            *pos,
                        ));
                    }

                    Ok(Spanned::new(
                        Expression::Var(self.symbols.symbol(ident), self.variable_use_maker.next()),
                        *pos,
                    ))
                }
                TokenType::THIS => Ok(Spanned::new(
                    Expression::This(self.variable_use_maker.next()),
                    *pos,
                )),

                TokenType::FUNCTION => self.fun_body("function"),
                TokenType::LBRACKET => {
                    let mut items = vec![];

                    if self.recognise(TokenType::RBRACKET) {
                        self.advance();
                        return Ok(Spanned::new(Expression::Array { items }, *pos));
                    }

                    while {
                        items.push(self.expression()?);

                        self.recognise(TokenType::COMMA)
                            && self.advance().map(|t| t.token) == Some(TokenType::COMMA)
                    } {}

                    self.consume(
                        &TokenType::RBRACKET,
                        "Expected a ']' to close the brackets .",
                    )?;

                    Ok(Spanned::new(Expression::Array { items }, *pos))
                }

                TokenType::LBRACE => {
                    let mut items: Vec<
                        (Spanned<Expression>, Spanned<Expression>),
                    > = vec![];

                    if self.recognise(TokenType::RBRACE) {
                        self.advance();
                        return Ok(Spanned::new(Expression::Dict { items }, *pos));
                    }

                    while {
                        let left = self.expression()?;
                        self.consume(&TokenType::COLON, "Expected a ':' after dict key ")?;
                        let right = self.expression()?;

                        items.push((left, right));
                        self.recognise(TokenType::COMMA)
                            && self.advance().map(|t| t.token) == Some(TokenType::COMMA)
                    } {}

                    let pos = self.consume_get_pos(
                        &TokenType::RBRACE,
                        "Expected a '}' to close a dictionary.",
                    )?;

                    Ok(Spanned::new(Expression::Dict { items }, pos))
                }

                TokenType::LPAREN => {
                    let expr = Box::new(self.expression()?);
                    let pos =
                        self.consume_get_pos(&TokenType::RPAREN, "Expect \')\' after expression")?;

                    Ok(Spanned::new(Expression::Grouping { expr }, pos))
                }

                ref e => {
                    println!("{:?} on {}", e, *pos);
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
    fn fun_body(&mut self, kind: &str) -> Result<Spanned<Expression>, ParserError> {
        let func_pos = self.consume_get_pos(&TokenType::LPAREN, "Expected '(' ")?;

        let mut parameters = Vec::with_capacity(32);
        let mut returns = None;

        if !self.recognise(TokenType::RPAREN) {
            while {
                if parameters.len() >= 32 {
                    return Err(ParserError::TooManyParams(func_pos));
                };

                let identifier = self.consume_get_symbol(&format!("Expected a {} name", kind))?;

                self.consume(&TokenType::COLON, "Expected a colon")?;

                let ty = self.parse_type()?;

                parameters.push((identifier, ty));

                self.recognise(TokenType::COMMA)
                    && self.advance().map(|t| t.token) == Some(TokenType::COMMA)
            } {}
        }

        self.consume(&TokenType::RPAREN, "Expected ')' after parameters.")?;

        if self.recognise(TokenType::FRETURN) {
            self.advance();

            returns = Some(self.parse_type()?);
        }

        let body = Box::new(self.block()?);

        Ok(Spanned::new(
            Expression::Func {
                parameters,
                body,
                returns,
            },
            func_pos,
        ))
    }
    fn finish_call(
        &mut self,
        callee: Spanned<Expression>,
    ) -> Result<Spanned<Expression>, ParserError> {
        let mut arguments = Vec::with_capacity(32);

        if !self.recognise(TokenType::RPAREN) {
            while {
                if arguments.len() >= 32 {
                    return Err(ParserError::TooManyParams(callee.pos));
                }

                arguments.push(self.expression()?);
                self.recognise(TokenType::COMMA)
                    && self.advance().map(|t| t.token) == Some(TokenType::COMMA)
            } {}
        }

        let pos = self.consume_get_pos(&TokenType::RPAREN, "Expected ')' after arguments.")?;

        Ok(Spanned::new(
            Expression::Call {
                callee: Box::new(callee),
                arguments,
            },
            pos,
        ))
    }
}
*/
