#[macro_use]
mod macros;

use ast::*;
use std::iter::Peekable;
use std::vec::IntoIter;
use symbol::{Symbol, Symbols};
use token::{Token, TokenType};
use util::emmiter::Reporter;
use util::pos::{Span, Spanned};

#[derive(Debug)]
pub struct Parser<'a> {
    tokens: Peekable<IntoIter<Spanned<Token<'a>>>>,
    reporter: Reporter,
    symbols: &'a mut Symbols<()>,
    /// Used to prevent parsing if (cond) { as a
    /// wrong statement
    parsing_cond: bool,
}

pub type ParserResult<T> = Result<T, ()>;

impl<'a> Parser<'a> {
    pub fn new(
        tokens: Vec<Spanned<Token<'a>>>,
        reporter: Reporter,
        symbols: &'a mut Symbols<()>,
    ) -> Self {
        Parser {
            tokens: tokens.into_iter().peekable(),
            symbols,
            parsing_cond: false,
            reporter,
        }
    }

    pub fn parse(&mut self) -> ParserResult<Program> {
        let mut program = Program {
            classes: Vec::new(),
            functions: Vec::new(),
            aliases: Vec::new(),
        };

        let mut had_error = false;

        while self.peek(|token| token != &TokenType::EOF) {
            if self.recognise(TokenType::FUNCTION) {
                match self.parse_function("function") {
                    Ok(function) => program.functions.push(function),
                    Err(_) => {
                        had_error = true;
                        self.synchronize();
                    }
                }
            } else if self.recognise(TokenType::CLASS) {
                match self.parse_class_declaration() {
                    Ok(class) => program.classes.push(class),
                    Err(_) => {
                        had_error = true;
                        self.synchronize();
                    }
                }
            } else if self.recognise(TokenType::TYPE) {
                match self.parse_type_alias() {
                    Ok(alias) => program.aliases.push(alias),
                    Err(_) => {
                        had_error = true;
                        self.synchronize();
                    }
                }
            } else {
                self.synchronize();
                had_error = true;
            }
        }

        if had_error {
            Err(())
        } else {
            Ok(program)
        }
    }

    pub fn synchronize(&mut self) {
        self.advance();

        while self.peek(|token| token == &TokenType::EOF) {
            match self.advance().map(|span| span.value.token) {
                Some(TokenType::CLASS)
                | Some(TokenType::FUNCTION)
                | Some(TokenType::IDENTIFIER(_))
                | Some(TokenType::FOR)
                | Some(TokenType::IF)
                | Some(TokenType::WHILE)
                | Some(TokenType::LBRACE)
                | Some(TokenType::RETURN) => break,
                None => unreachable!(),
                _ => self.advance(),
            };
        }
    }

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
    /// Checks if the next token is the expected token without
    /// advancing the token stream
    fn recognise(&mut self, token: TokenType<'a>) -> bool {
        if self.peek(|peeked| peeked == &token) {
            return true;
        }
        false
    }

    /// Checks if any one of the given tokens is the next token
    fn matched(&mut self, tokens: Vec<TokenType<'a>>) -> bool {
        let mut found = false;

        for token in tokens {
            if self.peek(|peeked| peeked == &token) {
                found = true;
            }
        }

        found
    }

    /// Advances the token stream
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
            MODULO => Modulo,
            EXPONENTIAL => Exponential
        })
    }

    fn get_assign_op(&mut self) -> ParserResult<Spanned<AssignOperator>> {
        get_assign_op!(self,{
            ASSIGN => Equal,
            PLUSASSIGN => PlusEqual,
            MINUSASSIGN => MinusEqual,
            STARASSIGN => StarEqual,
            SLASHASSIGN => SlashEqual
        })
    }
}

/* ***********************
*  Type Aliases
*  ***********************
*/
impl<'a> Parser<'a> {
    /// Parses a type alias of the form `type NAME = TYPE;`
    fn parse_type_alias(&mut self) -> ParserResult<Spanned<TypeAlias>> {
        let open_span = self.consume_get_span(&TokenType::TYPE, "Expected 'type' ")?;

        let alias = self.consume_get_symbol("Expected an identifier")?;

        self.consume(&TokenType::ASSIGN, "Expected '=' ")?;

        let ty = self.parse_type()?;

        let close_span = self.consume_get_span(&TokenType::SEMICOLON, "Expected ';' ")?;

        Ok(Spanned {
            span: open_span.to(close_span),
            value: TypeAlias { alias, ty },
        })
    }

    fn parse_type(&mut self) -> ParserResult<Spanned<Type>> {
        if self.recognise(TokenType::NIL) {
            Ok(Spanned {
                value: Type::Nil,
                span: self.consume_get_span(&TokenType::NIL, "Expected 'nil' ")?,
            })
        } else if self.recognise(TokenType::LBRACKET) {
            self.advance();
            let ty = self.parse_type()?;
            Ok(Spanned {
                value: Type::Arr(Box::new(ty)),
                span: self.consume_get_span(&TokenType::RBRACKET, "Expected ']' ")?,
            })
        } else if self.recognise(TokenType::FUNCTION) {
            let open_span = self.consume_get_span(&TokenType::FUNCTION, "Expected 'fun' ")?;

            self.consume(&TokenType::LPAREN, "Expected a \'(\'")?;
            let mut param_ty = Vec::with_capacity(32);

            if !self.recognise(TokenType::RPAREN) {
                loop {
                    param_ty.push(self.parse_type()?);

                    if self.recognise(TokenType::COMMA) {
                        self.advance();
                    } else {
                        break;
                    }
                }
            }

            let close_span = self.consume_get_span(&TokenType::RPAREN, "Expected  \')\'")?;

            if self.recognise(TokenType::FRETURN) {
                self.advance();
                let ty = self.parse_type()?;
                Ok(Spanned {
                    span: open_span.to(ty.get_span()),
                    value: Type::Func(param_ty, Some(Box::new(ty))),
                })
            } else {
                Ok(Spanned {
                    span: open_span.to(close_span),
                    value: Type::Func(param_ty, None),
                })
            }
        } else {
            let ty = self.consume_get_symbol("Expected a type param")?;

            Ok(Spanned {
                span: ty.get_span().to(ty.get_span()),
                value: Type::Simple(ty),
            })
        }
    }
}

/* ***********************
*  Functions
*  ***********************
*/

impl<'a> Parser<'a> {
    fn parse_function(&mut self, kind: &str) -> ParserResult<Spanned<Function>> {
        let open_span = self.consume_get_span(&TokenType::FUNCTION, "Expected 'function' ")?;

        let name = self.consume_get_symbol(&format!("Expected a {} name", kind))?;

        let param_span = self.consume_get_span(&TokenType::LPAREN, "Expected '(' ")?;

        let mut params = Vec::with_capacity(32);
        let mut returns = None;

        if !self.recognise(TokenType::RPAREN) {
            loop {
                if params.len() >= 32 {
                    self.error("Too many params", open_span);
                    break;
                };

                let msg = format!("Expected a {} name", kind);

                let (open_span, name) = self.consume_get_symbol_and_span(&msg)?;

                self.consume(&TokenType::COLON, "Expected a colon")?;

                let ty = self.parse_type()?;

                params.push(Spanned {
                    span: open_span.to(ty.get_span()),
                    value: FunctionParam { name, ty },
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

        let body = self.block()?;

        Ok(Spanned {
            span: open_span.to(body.get_span()),
            value: Function {
                name,
                body,
                params: Spanned {
                    span: param_span.to(rparen_span),
                    value: params,
                },
                returns,
            },
        })
    }

    /* ******************
     *
     * STATEMENT PARSERS
     *
     * ***************** */
    fn parse_statement(&mut self) -> ParserResult<Spanned<Statement>> {
        if self.recognise(TokenType::LBRACE) {
            self.block()
        } else if self.recognise(TokenType::VAR) {
            self.parse_var_declaration()
        } else if self.recognise(TokenType::BREAK) {
            self.parse_break_statement()
        } else if self.recognise(TokenType::CONTINUE) {
            self.parse_continue_statement()
        } else if self.recognise(TokenType::RETURN) {
            self.parse_return_statement()
        } else if self.recognise(TokenType::IF) {
            self.parse_if_statement()
        } else if self.recognise(TokenType::DO) {
            self.parse_do_statement()
        } else if self.recognise(TokenType::WHILE) {
            self.parse_while_statement()
        } else if self.recognise(TokenType::FOR) {
            self.parse_for_statement()
        } else if self.recognise(TokenType::PRINT) {
            self.parse_print_statement()
        } else {
            self.parse_expression_statement()
        }
    }

    fn parse_var_declaration(&mut self) -> ParserResult<Spanned<Statement>> {
        let open_span = self.consume_get_span(&TokenType::VAR, "Expected 'var' ")?;

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
            Some(self.parse_expression()?)
        };

        let close_span = self.consume_get_span(&TokenType::SEMICOLON, "Expected ';'")?;

        Ok(Spanned {
            span: open_span.to(close_span),
            value: Statement::VarDeclaration { ident, ty, expr },
        })
    }

    fn block(&mut self) -> ParserResult<Spanned<Statement>> {
        let open_span = self.consume_get_span(&TokenType::LBRACE, "Expected a '{' ")?;

        let mut statements = vec![];

        while !self.recognise(TokenType::RBRACE) {
            statements.push(self.parse_statement()?);
        }

        let close_span =
            self.consume_get_span(&TokenType::RBRACE, "Expected a \'}\' after block.")?;
        Ok(Spanned {
            span: open_span.to(close_span),
            value: Statement::Block(statements),
        })
    }

    fn parse_break_statement(&mut self) -> ParserResult<Spanned<Statement>> {
        self.consume_get_span(&TokenType::BREAK, "Expected a 'break' ")?;
        Ok(Spanned {
            value: Statement::Break,
            span: self.consume_get_span(&TokenType::SEMICOLON, "Expected ';' ")?,
        })
    }

    fn parse_continue_statement(&mut self) -> ParserResult<Spanned<Statement>> {
        self.consume_get_span(&TokenType::CONTINUE, "Expected 'continue' ")?;
        Ok(Spanned {
            value: Statement::Continue,
            span: self.consume_get_span(&TokenType::SEMICOLON, "Expected ';' ")?,
        })
    }

    fn parse_expression_statement(&mut self) -> ParserResult<Spanned<Statement>> {
        let expr = self.parse_expression()?;

        Ok(Spanned {
            span: self.consume_get_span(&TokenType::SEMICOLON, "Expected ';' ")?,
            value: Statement::Expr(expr),
        })
    }

    fn parse_print_statement(&mut self) -> ParserResult<Spanned<Statement>> {
        let open_span = self.consume_get_span(&TokenType::PRINT, "Expected 'print' ")?;

        let expr = self.parse_expression()?;

        Ok(Spanned {
            span: open_span.to(self.consume_get_span(&TokenType::SEMICOLON, "Expected ';' ")?),
            value: Statement::Print(expr),
        })
    }

    fn parse_return_statement(&mut self) -> ParserResult<Spanned<Statement>> {
        let open_span = self.consume_get_span(&TokenType::RETURN, "Expected 'return' ")?;

        let expr = if self.recognise(TokenType::SEMICOLON) {
            let span = self.consume_get_span(&TokenType::SEMICOLON, "Expected ';' ")?;

            return Ok(Spanned {
                span: open_span.to(span),
                value: Statement::Return(Spanned {
                    span,
                    value: Expression::Literal(Literal::Nil),
                }),
            });
        } else {
            self.parse_expression()?
        };

        let close_span = self.consume_get_span(&TokenType::SEMICOLON, "Expected ';' ")?;

        Ok(Spanned {
            span: open_span.to(close_span),
            value: Statement::Return(expr),
        })
    }

    fn parse_while_statement(&mut self) -> ParserResult<Spanned<Statement>> {
        let open_span = self.consume_get_span(&TokenType::WHILE, "Expected 'while' ")?;

        self.parsing_cond = true;
        let cond = self.parse_expression()?;
        self.parsing_cond = false;

        let body = self.parse_statement()?;

        Ok(Spanned {
            span: open_span.to(body.get_span()),
            value: Statement::While {
                cond,
                body: Box::new(body),
            },
        })
    }

    fn parse_do_statement(&mut self) -> ParserResult<Spanned<Statement>> {
        let open_span = self.consume_get_span(&TokenType::DO, "Expected 'do' ")?;

        let body = self.parse_statement()?;

        self.consume(&TokenType::WHILE, "Expected while after 'do' condition")?;

        let cond = self.parse_expression()?;

        Ok(Spanned {
            span: open_span.to(body.get_span()),
            value: Statement::While {
                cond,
                body: Box::new(body),
            },
        })
    }

    fn parse_if_statement(&mut self) -> ParserResult<Spanned<Statement>> {
        let open_span = self.consume_get_span(&TokenType::IF, "Expected 'if' ")?;

        self.parsing_cond = true;
        let cond = self.parse_expression()?;
        self.parsing_cond = false;

        let then = Box::new(self.parse_statement()?);

        let (close_span, otherwise) = if self.recognise(TokenType::ELSE) {
            self.advance();

            let otherwise = self.parse_statement()?;

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

    fn parse_for_statement(&mut self) -> ParserResult<Spanned<Statement>> {
        let open_span = self.consume_get_span(&TokenType::FOR, "Expected 'for' ")?;

        self.consume(&TokenType::LPAREN, "Expected '(' after 'for'")?;

        let mut init = None;

        if self.recognise(TokenType::SEMICOLON) {
            self.advance();
        } else if self.recognise(TokenType::VAR) {
            init = Some(Box::new(self.parse_var_declaration()?));
        } else {
            init = Some(Box::new(self.parse_expression_statement()?));
        }

        let cond = if !self.recognise(TokenType::SEMICOLON) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        self.consume(&TokenType::SEMICOLON, "Expected ';' after loop condition .")?;

        let incr = if !self.recognise(TokenType::RPAREN) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        self.consume(&TokenType::RPAREN, "Expected ')' after for clauses.")?;

        let body = self.parse_statement()?;

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

    /* ******************
     *
     * EXPRESSION PARSERS
     *
     * ***************** */

    fn parse_expression(&mut self) -> ParserResult<Spanned<Expression>> {
        self.assignment()
    }

    fn assignment(&mut self) -> ParserResult<Spanned<Expression>> {
        let expr = self.parse_or()?;

        if self.matched(vec![
            TokenType::ASSIGN,
            TokenType::PLUSASSIGN,
            TokenType::MINUSASSIGN,
            TokenType::STARASSIGN,
            TokenType::SLASHASSIGN,
        ]) {
            let kind = self.get_assign_op()?;

            let value = self.assignment()?;

            match expr {
                Spanned {
                    span,
                    value: Expression::Var(var),
                } => {
                    return Ok(Spanned {
                        span: span.to(value.get_span()),
                        value: Expression::Assign {
                            name: var,
                            value: Box::new(value),
                            kind,
                        },
                    })
                }

                Spanned {
                    span,
                    value:
                        Expression::Get {
                            object, property, ..
                        },
                } => {
                    return Ok(Spanned {
                        span: span.to(value.get_span()),
                        value: Expression::Set {
                            object,
                            name: property,
                            value: Box::new(value),
                        },
                    })
                }

                Spanned { ref span, .. } => {
                    self.error("Not a valid assingment target", *span);
                    return Err(());
                }
            }
        }

        Ok(expr)
    }

    fn parse_or(&mut self) -> ParserResult<Spanned<Expression>> {
        let mut lhs = self.parse_and()?;

        use self::TokenType::*;

        binary!(self, OR, lhs, parse_and);

        Ok(lhs)
    }

    fn parse_and(&mut self) -> ParserResult<Spanned<Expression>> {
        let mut lhs = self.parse_equality()?;

        use self::TokenType::*;

        binary!(self, AND, lhs, parse_equality);

        Ok(lhs)
    }

    fn parse_equality(&mut self) -> ParserResult<Spanned<Expression>> {
        let mut lhs = self.parse_comparison()?;

        binary!(
            self,
            vec![TokenType::BANGEQUAL, TokenType::EQUALEQUAL],
            lhs,
            parse_comparison
        );

        Ok(lhs)
    }

    fn parse_comparison(&mut self) -> ParserResult<Spanned<Expression>> {
        let mut lhs = self.parse_addition()?;

        binary!(
            self,
            vec![
                TokenType::LESSTHAN,
                TokenType::LESSTHANEQUAL,
                TokenType::GREATERTHAN,
                TokenType::GREATERTHANEQUAL,
            ],
            lhs,
            parse_addition
        );

        Ok(lhs)
    }

    fn parse_addition(&mut self) -> ParserResult<Spanned<Expression>> {
        let mut lhs = self.parse_multiplication()?;

        binary!(
            self,
            vec![
                TokenType::MINUS,
                TokenType::PLUS,
                TokenType::EXPONENTIAL,
                TokenType::MODULO
            ],
            lhs,
            parse_multiplication
        );

        Ok(lhs)
    }

    fn parse_multiplication(&mut self) -> ParserResult<Spanned<Expression>> {
        let mut lhs = self.parse_unary()?;

        binary!(
            self,
            vec![TokenType::SLASH, TokenType::STAR],
            lhs,
            parse_unary
        );

        Ok(lhs)
    }

    fn parse_unary(&mut self) -> ParserResult<Spanned<Expression>> {
        if self.matched(vec![TokenType::BANG, TokenType::MINUS]) {
            let op = self.get_unary_op()?;

            let right = self.parse_unary()?;

            return Ok(Spanned {
                span: op.get_span().to(right.get_span()),
                value: Expression::Unary {
                    op,
                    expr: Box::new(right),
                },
            });
        }

        self.call()
    }

    fn primary(&mut self) -> ParserResult<Spanned<Expression>> {
        match self.advance() {
            Some(Spanned {
                ref span,
                ref value,
            }) => match value.token {
                TokenType::TRUE(_) => Ok(Spanned {
                    span: *span,
                    value: Expression::Literal(Literal::True(true)),
                }),
                TokenType::NIL => Ok(Spanned {
                    span: *span,
                    value: Expression::Literal(Literal::Nil),
                }),
                TokenType::FALSE(_) => Ok(Spanned {
                    span: *span,
                    value: Expression::Literal(Literal::False(false)),
                }),
                TokenType::STRING(ref s) => Ok(Spanned {
                    span: *span,
                    value: Expression::Literal(Literal::Str(s.clone())),
                }),
                TokenType::INT(n) => Ok(Spanned {
                    span: *span,
                    value: Expression::Literal(Literal::Int(n)),
                }),
                TokenType::FLOAT(n) => Ok(Spanned {
                    span: *span,
                    value: Expression::Literal(Literal::Float(n)),
                }),

                TokenType::THIS => Ok(Spanned {
                    span: *span,
                    value: Expression::This,
                }),

                TokenType::LPAREN => {
                    let expr = Box::new(self.parse_expression()?);

                    let close_span = self.consume_get_span(&TokenType::RPAREN, "Expected ')'")?;

                    Ok(Spanned {
                        span: span.to(close_span),
                        value: Expression::Grouping { expr },
                    })
                }

                TokenType::IDENTIFIER(ident) => {
                    let ident = Spanned {
                        value: self.symbols.symbol(ident),
                        span: *span,
                    };
                    self.parse_ident(ident)
                }

                TokenType::LBRACKET => {
                    let mut items = Vec::with_capacity(32);

                    if !self.recognise(TokenType::RBRACKET) {
                        loop {
                            if items.len() >= 32 {
                                break;
                            };

                            items.push(self.parse_expression()?);

                            if self.recognise(TokenType::COMMA) {
                                self.advance();
                            } else {
                                break;
                            }
                        }
                    }

                    let close_span =
                        self.consume_get_span(&TokenType::RBRACKET, "Expected a closing `]`")?;

                    Ok(Spanned {
                        value: Expression::Array { items },
                        span: span.to(close_span),
                    })
                }

                ref other => {
                    let msg = format!("No rules expected '{}' ", other);

                    self.error(msg, *span);

                    Err(())
                }
            },
            None => Err(()), // TODO: ADD an error?
        }
    }

    fn parse_ident(&mut self, symbol: Spanned<Symbol>) -> ParserResult<Spanned<Expression>> {
        if self.recognise(TokenType::LBRACE) {
            self.advance();

            let mut props = vec![];

            if !self.recognise(TokenType::RBRACE) {
                loop {
                    let (open_span, symbol) =
                        self.consume_get_symbol_and_span("Expected a field name")?;

                    self.consume(&TokenType::COLON, "Expected a colon")?;

                    let expr = self.parse_expression()?;

                    props.push(Spanned {
                        span: open_span.to(symbol.get_span()),
                        value: InstanceField { symbol, expr },
                    });

                    if self.recognise(TokenType::COMMA) {
                        self.advance();
                    } else {
                        break;
                    }
                }
            }

            let close_span = self.consume_get_span(&TokenType::RBRACE, "Expected '}' ")?;

            Ok(Spanned {
                span: symbol.get_span().to(close_span),
                value: Expression::ClassInstance {
                    symbol,
                    props: props,
                },
            })
        } else {
            Ok(Spanned {
                span: symbol.get_span(),
                value: Expression::Var(symbol),
            })
        }
    }

    fn call(&mut self) -> ParserResult<Spanned<Expression>> {
        let mut expr = self.primary()?;

        loop {
            if self.recognise(TokenType::LPAREN) {
                expr = self.finish_call(expr)?;
            } else if self.recognise(TokenType::LBRACKET) {
                let open_span = self.consume_get_span(&TokenType::LBRACKET, "Expected '[' ")?;

                let index = Box::new(self.parse_expression()?);

                let close_span = self.consume_get_span(&TokenType::RBRACKET, "Expected ']' ")?;
                return Ok(Spanned {
                    span: open_span.to(close_span),
                    value: Expression::SubScript {
                        target: Box::new(expr),
                        index,
                    },
                });
            } else if self.recognise(TokenType::DOT) {
                self.advance();

                let (close_span, property) =
                    self.consume_get_symbol_and_span("Expected an identifier")?;

                expr = Spanned {
                    span: expr.get_span().to(close_span),
                    value: Expression::Get {
                        object: Box::new(expr),
                        property,
                    },
                }
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Spanned<Expression>) -> ParserResult<Spanned<Expression>> {
        self.consume(&TokenType::LPAREN, "Expected '(' ")?;

        let mut args = vec![];

        if !self.recognise(TokenType::RPAREN) {
            loop {
                args.push(self.parse_expression()?);

                if self.recognise(TokenType::COMMA) {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        let close_span = self.consume_get_span(&TokenType::RPAREN, "Expected '(' ")?;

        Ok(Spanned {
            span: callee.get_span().to(close_span),
            value: Expression::Call {
                callee: Box::new(callee),
                args,
            },
        })
    }
}

/* ***********************
*  Classes
*  ***********************
*/
impl<'a> Parser<'a> {
    fn parse_class_declaration(&mut self) -> ParserResult<Spanned<Class>> {
        let open_span = self.consume_get_span(&TokenType::CLASS, "Expected 'class' ")?;

        let name = self.consume_get_symbol("Expected a class name")?;

        let superclass = if self.recognise(TokenType::LESSTHAN) {
            self.advance();
            Some(self.consume_get_symbol("Expected a superclass name")?)
        } else {
            None
        };

        let mut properties = vec![];
        let mut methods = vec![];

        self.consume(&TokenType::LBRACE, "Expected a '{' after class name")?;

        while !self.recognise(TokenType::RBRACE) {
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

            methods.push(self.parse_function("method")?);
        }

        let close_span = self.consume_get_span(&TokenType::RBRACE, "Expected '}' ")?;

        Ok(Spanned {
            span: open_span.to(close_span),
            value: Class {
                name,
                superclass,
                methods,
                fields: properties,
            },
        })
    }
}

// impl<'a> Parser<'a> {

// }
