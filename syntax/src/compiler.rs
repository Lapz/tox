use ast::*;
use rand::{self, Rng};
use token::{Token, TokenType};
use util::emmiter::Reporter;
use util::pos::{CharPosition, Position, Span, Spanned};
use util::symbol::{Symbol, Symbols};

type ParserResult<T> = Result<T, ()>;

pub struct Parser<'a> {
    /// The input string
    input: &'a str,
    chars: CharPosition<'a>,
    /// The reporter that collects all the erros
    reporter: Reporter,
    /// The symbols table that will be built up during parsing
    symbols: &'a mut Symbols<()>,
    /// The character ahead
    lookahead: Option<(Position, char)>,
    /// the last token
    last_token: Option<Spanned<Token<'a>>>,
    /// The very first character
    start: Position,
    /// The last charact
    end: Position,
    /// Flag that manages whetere we are in a cond or class instance
    parsing_cond: bool,
}
// The lexing and get tokens is handled here
impl<'a> Parser<'a> {
    pub fn new(input: &'a str, reporter: Reporter, symbols: &'a mut Symbols<()>) -> Self {
        let mut chars = CharPosition::new(input);
        let end = chars.pos;
        let mut parser = Self {
            input,
            end,
            start: end,
            reporter,
            lookahead: chars.next(),
            last_token: None,
            chars,
            symbols,
            parsing_cond: false,
        };

        let token = parser.next();

        parser.last_token = Some(token.unwrap());

        parser


    }

    pub fn parse(&mut self) -> ParserResult<Program> {
        let mut program = Program {
            classes: Vec::new(),
            functions: Vec::new(),
            aliases: Vec::new(),
        };

        let mut had_error = false;

        

        while self.peek(|token| token != '\0') {
            if self.recognise(TokenType::FUNCTION)? {
                match self.parse_function("function") {
                    Ok(function) => program.functions.push(function),
                    Err(_) => {
                        had_error = true;
                       
                        self.synchronize();
                    }
                }
            } else if self.recognise(TokenType::CLASS)? {
                match self.parse_class_declaration() {
                    Ok(class) => program.classes.push(class),
                    Err(_) => {
                        had_error = true;
                        self.synchronize();
                    }
                }
            } else if self.recognise(TokenType::TYPE)? {
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

        while self.peek(|token| token == '\0') {
            match self.next().map(|span| span.value.token) {
                Ok(TokenType::CLASS)
                | Ok(TokenType::FUNCTION)
                | Ok(TokenType::IDENTIFIER(_))
                | Ok(TokenType::FOR)
                | Ok(TokenType::IF)
                | Ok(TokenType::WHILE)
                | Ok(TokenType::LBRACE)
                | Ok(TokenType::RETURN) => break,
                Err(_) => unreachable!(),
                Ok(_) => self.advance(),
            };
        }
    }

    fn next(&mut self) -> ParserResult<Spanned<Token<'a>>> {
        while let Some((start, ch)) = self.advance() {
            return match ch {
                '.' => Ok(span(TokenType::DOT, start)),
                '?' => Ok(span(TokenType::QUESTION, start)),
                ';' => Ok(span(TokenType::SEMICOLON, start)),
                '{' => Ok(span(TokenType::LBRACE, start)),
                '}' => Ok(span(TokenType::RBRACE, start)),
                '[' => Ok(span(TokenType::LBRACKET, start)),
                ']' => Ok(span(TokenType::RBRACKET, start)),
                '(' => Ok(span(TokenType::LPAREN, start)),
                ')' => Ok(span(TokenType::RPAREN, start)),
                ',' => Ok(span(TokenType::COMMA, start)),
                ':' => Ok(span(TokenType::COLON, start)),
                '|' => Ok(span(TokenType::BAR, start)),
                '^' => Ok(span(TokenType::EXPONENTIAL, start)),
                '%' => Ok(span(TokenType::MODULO, start)),
                '!' => {
                    if self.peek(|ch| ch == '=') {
                        self.advance();
                        Ok(spans(TokenType::BANGEQUAL, start, start.shift('=')))
                    } else {
                        Ok(span(TokenType::BANG, start))
                    }
                }
                '>' => {
                    if self.peek(|ch| ch == '=') {
                        self.advance();
                        Ok(spans(TokenType::GREATERTHANEQUAL, start, start.shift('=')))
                    } else {
                        Ok(span(TokenType::GREATERTHAN, start))
                    }
                }
                '<' => {
                    if self.peek(|ch| ch == '=') {
                        self.advance();
                        Ok(spans(TokenType::LESSTHANEQUAL, start, start.shift('=')))
                    } else {
                        Ok(span(TokenType::LESSTHAN, start))
                    }
                }

                '=' => {
                    if self.peek(|ch| ch == '=') {
                        self.advance();
                        Ok(spans(TokenType::EQUALEQUAL, start, start.shift('=')))
                    } else {
                        Ok(span(TokenType::ASSIGN, start))
                    }
                }

                '+' => {
                    if self.peek(|ch| ch == '=') {
                        self.advance();
                        Ok(spans(TokenType::PLUSASSIGN, start, start.shift('=')))
                    } else {
                        Ok(span(TokenType::PLUS, start))
                    }
                }

                '-' => {
                    if self.peek(|ch| ch == '=') {
                        self.advance();
                        Ok(spans(TokenType::MINUSASSIGN, start, start.shift('=')))
                    } else if self.peek(|ch| ch == '>') {
                        self.advance();
                        Ok(spans(TokenType::FRETURN, start, start.shift('>')))
                    } else {
                        Ok(span(TokenType::MINUS, start))
                    }
                }

                '*' => {
                    if self.peek(|ch| ch == '=') {
                        self.advance();
                        Ok(spans(TokenType::STARASSIGN, start, start.shift('=')))
                    } else {
                        Ok(span(TokenType::STAR, start))
                    }
                }

                '"' => match self.string_literal(start) {
                    Ok(token) => Ok(token),
                    Err(e) => {
                        continue; // error is reported in the function
                    }
                },

                '/' => {
                    if self.peek(|ch| ch == '=') {
                        self.advance();
                        Ok(spans(TokenType::SLASHASSIGN, start, start.shift('=')))
                    } else if self.peek(|ch| ch == '/') {
                        self.advance();
                        self.line_comment(start);
                        continue;
                    } else if self.peek(|ch| ch == '*') {
                        self.block_comment()?;
                        continue;
                    } else {
                        Ok(span(TokenType::SLASH, start))
                    }
                }

                ch if ch.is_numeric() => self.number(start),
                ch if is_letter_ch(ch) => Ok(self.identifier(start)),
                ch if ch.is_whitespace() => continue,
                ch => {
                    let msg = format!("Unexpected char {} on {}", ch, start);
                    self.error(msg, start);
                    continue;
                }
            };
        }

        Ok(spans(TokenType::EOF, self.end, self.end))
    }

    /// Advances the input return the current postion and the char we are at
    fn advance(&mut self) -> Option<(Position, char)> {
        match self.lookahead {
            Some((pos, ch)) => {
                self.end = self.end.shift(ch);
                self.lookahead = self.chars.next();
                Some((pos, ch))
            }

            None => None,
        }
    }

    /// Slice the input return the string contained
    /// between the supplied postion
    fn slice(&self, start: Position, end: Position) -> &'a str {
        &self.input[start.absolute..end.absolute]
    }

    /// Advance the input whilst the given function evaluates
    /// to true
    fn take_whilst<F>(&mut self, start: Position, mut terminate: F) -> (Position, &'a str)
    where
        F: FnMut(char) -> bool,
    {
        while let Some((end, ch)) = self.lookahead {
            if !terminate(ch) {
                return (end, self.slice(start, end));
            }
            self.advance();
        }

        (self.end, self.slice(start, self.end))
    }

    /// Lookahead at the input
    fn peek<F>(&mut self, mut check: F) -> bool
    where
        F: FnMut(char) -> bool,
    {
        self.lookahead.map_or(false, |(_, ch)| check(ch))
    }

    /// Reporter an error at the given character
    fn error<T: Into<String>>(&mut self, msg: T, pos: Position) {
        self.reporter.error(
            msg.into(),
            Span {
                start: pos,
                end: pos,
            },
        )
    }

    /// Reporter an error that spans the given positions
    fn spanned_error<T: Into<String>>(&mut self, msg: T, start: Position, end: Position) {
        self.reporter.error(msg.into(), Span { start, end })
    }

    /// Reporter an error with the given span
    fn span_error<T: Into<String>>(&mut self, msg: T, span: Span) {
        self.reporter.error(msg.into(), span)
    }

    /// Method that handles a line comment
    fn line_comment(&mut self, start: Position) {
        let (_, _) = self.take_whilst(start, |ch| ch != '\n');
    }

    /// Method that handles a block comment
    fn block_comment(&mut self) -> ParserResult<()> {
        self.advance(); // Eats the '*'

        let mut last = None;
        loop {
            self.advance(); // Eats the '*'

            match self.lookahead {
                Some((_, '/')) => {
                    self.advance();
                    return Ok(());
                }
                Some((pos, _)) => {
                    last = Some(pos);
                    continue;
                }

                None => {
                    self.error("Unclosed block comment", last.unwrap());
                    return Err(());
                }
            }
        }
    }

    /// Handles a string.
    fn string_literal(&mut self, start: Position) -> ParserResult<Spanned<Token<'a>>> {
        let mut string = String::new();
        let mut last = None; // placement value

        while let Some((next, ch)) = self.advance() {
            match ch {
                '"' => {
                    let end = next.shift(ch);

                    string.push('\0');

                    return Ok(spans(TokenType::STRING(string), start, end));
                }

                ch => {
                    last = Some(next); // the last thing in the string
                    string.push(ch)
                }
            }
        }

        let msg = format!("Unclosed string");

        self.error(msg, last.unwrap()); // has to be the end as we keep on adding to our string till we reach the end

        Err(())
    }

    /// Handles number,both ints and floats
    fn number(&mut self, start: Position) -> ParserResult<Spanned<Token<'a>>> {
        let (end, int) = self.take_whilst(start, |c| c.is_numeric());

        let (token, start, end) = match self.lookahead {
            Some((_, '.')) => {
                self.advance();

                let (end, float) = self.take_whilst(start, |c| c.is_numeric());

                match self.lookahead {
                    Some((pos, ch)) if ch.is_alphabetic() => {
                        let msg = format!("Unexpected char {}", ch);
                        self.error(msg, pos);
                        return Err(()); // Rejects floats like 10.k
                    }

                    _ => (TokenType::FLOAT(float.parse().unwrap()), start, end),
                }
            }

            Some((pos, ch)) if ch.is_alphabetic() => {
                let msg = format!("Unexpected char {}", ch);
                self.error(msg, pos);
                return Err(()); // Rejects number like 1k
            }
            None | Some(_) => {
                if let Ok(val) = int.parse() {
                    (TokenType::INT(val), start, end)
                } else {
                    let msg = format!("`{}` cannot fit into a int.", int);
                    self.spanned_error(msg, start, end);
                    return Err(());
                }
            }
        };

        Ok(spans(token, start, end))
    }

    /// Handles any identifier.
    // Newkeywords should be added to the look_up_identifier function
    fn identifier(&mut self, start: Position) -> Spanned<Token<'a>> {
        let (end, ident) = self.take_whilst(start, is_letter_ch);
        spans(look_up_identifier(ident), start, end)
    }
}

impl<'a> Parser<'a> {
    /// Checks if any one of the given tokens is the next token
    fn matched(&mut self, tokens: Vec<TokenType>) -> ParserResult<bool> {
        let mut found = false;

        for token in tokens {
            if self.recognise(token)? {
                found = true;
            }
        }

        Ok(found)
    }

    // check if the input matches everything
    fn matches(&mut self, chars: Vec<char>) -> bool {
        let mut matches = false;

        for ch in chars {
            self.chars.chars.peek().map(|c| if *c == ch { matches = true});
        }

        

        matches
    }

    pub fn consume(&mut self, token_to_check: &TokenType<'a>, msg: &str) -> ParserResult<()> {
        match self.next() {
            Ok(Spanned {
                ref span,
                value: Token { ref token },
            }) => {
                if token == token_to_check {
                    return Ok(());
                }

                let msg = format!("{} but instead found {}", msg, token);

                self.span_error(msg, *span);

                Err(())
            }

            Err(_) => Err(()),
        }
    }

    fn consume_get_span(
        &mut self,
        token_to_check: &TokenType<'a>,
        msg: &str,
    ) -> ParserResult<Span> {
        match self.next() {
            Ok(Spanned {
                value: Token { ref token },
                ref span,
            }) => {
                if token == token_to_check {
                    return Ok(*span);
                }

                let msg = format!("{} but instead found {}", msg, token);

                self.span_error(msg, *span);

                Err(())
            }
            Err(_) => Err(()),
        }
    }

    pub fn consume_get_symbol(&mut self, msg: &str) -> ParserResult<Spanned<Symbol>> {
        match self.next() {
            Ok(Spanned {
                value:
                    Token {
                        token: TokenType::IDENTIFIER(ident),
                    },
                ref span,
            }) => Ok(Spanned {
                span: *span,
                value: self.symbols.symbol(ident),
            }),
            Ok(Spanned {
                value: Token { ref token },
                ref span,
            }) => {
                let msg = format!("{} but instead found {}", msg, token);

                self.span_error(msg, *span);

                Err(())
            }
            Err(_) => Err(()),
        }
    }

    fn consume_get_symbol_and_span(&mut self, msg: &str) -> ParserResult<(Span, Spanned<Symbol>)> {
        match self.next() {
            Ok(Spanned {
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
            Ok(Spanned {
                value: Token { ref token },
                ref span,
            }) => {
                let msg = format!("{} but instead found {}", msg, token);

                self.span_error(msg, *span);

                Err(())
            }
            Err(()) => Err(()),
        }
    }

    fn random_ident(&mut self) -> Symbol {
        let mut rng = rand::thread_rng();
        let letter: char = rng.gen_range(b'A', b'Z') as char;
        let number: u32 = rng.gen_range(0, 999999);
        let s = format!("{}{:06}", letter, number);

        self.symbols.symbol(&s)
    }

    

    fn recognise(&mut self, token: TokenType) -> ParserResult<bool> {
        if let Som
        Ok(self.last_token.as_ref().map_or(false, |t|  &t.value.token == &token))
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
            EXPONENTIAL => Exponential,
            BANGEQUAL => BangEqual
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
        if self.recognise(TokenType::NIL)? {
            Ok(Spanned {
                value: Type::Nil,
                span: self.consume_get_span(&TokenType::NIL, "Expected 'nil' ")?,
            })
        } else if self.peek(|ch| ch == '[') {
            self.advance();
            let ty = self.parse_type()?;
            Ok(Spanned {
                value: Type::Arr(Box::new(ty)),
                span: self.consume_get_span(&TokenType::RBRACKET, "Expected ']' ")?,
            })
        } else if self.recognise(TokenType::FUNCTION)? {
            let open_span = self.consume_get_span(&TokenType::FUNCTION, "Expected 'fun' ")?;

            self.consume(&TokenType::LPAREN, "Expected a \'(\'")?;
            let mut param_ty = Vec::with_capacity(32);

            if !self.peek(|ch| ch == ')') {
                loop {
                    param_ty.push(self.parse_type()?);

                    if self.peek(|ch| ch == ',') {
                        self.advance();
                    } else {
                        break;
                    }
                }
            }

            let close_span = self.consume_get_span(&TokenType::RPAREN, "Expected  \')\'")?;

            if self.recognise(TokenType::FRETURN)? {
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

        let params = self.parse_params(param_span, "function")?;
        let mut returns = None;

        let rparen_span =
            self.consume_get_span(&TokenType::RPAREN, "Expected a ')' after function params")?;

        if self.recognise(TokenType::FRETURN)? {
            self.advance();

            returns = Some(self.parse_type()?);
        }

        let body = self.parse_block()?;

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

    fn parse_params(
        &mut self,
        open_span: Span,
        kind: &str,
    ) -> ParserResult<Vec<Spanned<FunctionParam>>> {
        let mut params = Vec::with_capacity(32);

        if !self.peek(|ch| ch == ')') && !self.peek(|ch| ch == '|') {
            loop {
                if params.len() >= 32 {
                    self.span_error("Too many params", open_span);
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

                if self.peek(|ch| ch == ',') {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        Ok(params)
    }

    /* ******************
     *
     * STATEMENT PARSERS
     *
     * ***************** */
    pub fn parse_statement(&mut self) -> ParserResult<Spanned<Statement>> {
        if self.peek(|ch| ch == '{') {
            self.parse_block()
        } else if self.recognise(TokenType::LET)? {
            self.parse_var_declaration()
        } else if self.recognise(TokenType::BREAK)? {
            self.parse_break_statement()
        } else if self.recognise(TokenType::CONTINUE)? {
            self.parse_continue_statement()
        } else if self.recognise(TokenType::RETURN)? {
            self.parse_return_statement()
        } else if self.recognise(TokenType::IF)? {
            self.parse_if_statement()
        } else if self.recognise(TokenType::DO)? {
            self.parse_do_statement()
        } else if self.recognise(TokenType::WHILE)? {
            self.parse_while_statement()
        } else if self.recognise(TokenType::FOR)? {
            self.parse_for_statement()
        } else if self.recognise(TokenType::PRINT)? {
            self.parse_print_statement()
        } else {
            self.parse_expression_statement()
        }
    }

    fn parse_var_declaration(&mut self) -> ParserResult<Spanned<Statement>> {
        let open_span = self.consume_get_span(&TokenType::LET, "Expected 'var' ")?;

        let ident = self.consume_get_symbol("Expected an IDENTIFIER after a 'var' ")?;

        let ty = if self.peek(|ch| ch == ':') {
            self.advance();

            Some(self.parse_type()?)
        } else {
            None
        };

        let expr = if self.peek(|ch| ch == ';') {
            None
        } else {
            self.consume(&TokenType::ASSIGN, "Expected '='")?;
            Some(self.parse_expression()?)
        };

        let close_span = self.consume_get_span(&TokenType::SEMICOLON, "Expected ';'")?;

        Ok(Spanned {
            span: open_span.to(close_span),
            value: Statement::VarDeclaration { ident, ty, expr },
        })
    }

    fn parse_block(&mut self) -> ParserResult<Spanned<Statement>> {
        let open_span = self.consume_get_span(&TokenType::LBRACE, "Expected a '{' ")?;

        let mut statements = vec![];

        while !self.peek(|ch| ch == '}') {
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

    pub fn parse_expression_statement(&mut self) -> ParserResult<Spanned<Statement>> {
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

        let expr = if self.peek(|ch| ch == ';') {
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

        let (close_span, otherwise) = if self.recognise(TokenType::ELSE)? {
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

        if self.peek(|ch| ch == ';') {
            self.advance();
        } else if self.recognise(TokenType::LET)? {
            init = Some(Box::new(self.parse_var_declaration()?));
        } else {
            init = Some(Box::new(self.parse_expression_statement()?));
        }

        let cond = if !self.peek(|ch| ch == ';') {
            Some(self.parse_expression()?)
        } else {
            None
        };

        self.consume(&TokenType::SEMICOLON, "Expected ';' after loop condition .")?;

        let incr = if !self.peek(|ch| ch == ')') {
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
        let expr = self.parse_ternary()?;

        if self.peek(|ch| ch == '=')
            || self.matches(vec!['+', '='])
            || self.matches(vec!['-', '='])
            || self.matches(vec!['*', '='])
            || self.matches(vec!['/', '='])
        {
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
                    self.span_error("Not a valid assingment target", *span);
                    return Err(());
                }
            }
        }

        Ok(expr)
    }

    fn parse_ternary(&mut self) -> ParserResult<Spanned<Expression>> {
        let mut expr = self.parse_or()?;

        if self.peek(|ch| ch == '?') {
            self.advance();

            let if_true = self.parse_expression()?;

            self.consume(&TokenType::COLON, "Expected ':'")?;

            let if_false = self.parse_expression()?;

            expr = Spanned {
                span: expr.get_span().to(if_false.get_span()),
                value: Expression::Ternary {
                    condition: Box::new(expr),
                    then_branch: Box::new(if_true),
                    else_branch: Box::new(if_false),
                },
            }
        };

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
                TokenType::PLUS,
                TokenType::MINUS,
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
        if self.peek(|ch| ch == '!') || self.peek(|ch| ch == '-') {
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
        match self.next() {
            Ok(Spanned {
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

                TokenType::BAR => {
                    let closure = self.parse_closure(*span)?;

                    Ok(Spanned {
                        span: closure.get_span(),
                        value: Expression::Closure(Box::new(closure)),
                    })
                }

                TokenType::LBRACKET => {
                    let mut items = Vec::with_capacity(32);

                    if !self.peek(|ch| ch == ']') {
                        loop {
                            if items.len() >= 32 {
                                break;
                            };

                            items.push(self.parse_expression()?);

                            if self.peek(|ch| ch == ',') {
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

                    self.span_error(msg, *span);

                    Err(())
                }
            },
            Err(_) => Err(()), // TODO: ADD an error?
        }
    }

    fn parse_closure(&mut self, open_span: Span) -> ParserResult<Spanned<Function>> {
        let params = self.parse_params(open_span, "closure")?;

        let params_span = self.consume_get_span(&TokenType::BAR, "Expected `|` ")?;

        let returns = if self.recognise(TokenType::FRETURN)? {
            self.advance(); // skip the ->
            Some(self.parse_type()?)
        } else {
            None
        };

        let body = self.parse_block()?;

        Ok(Spanned {
            span: open_span.to(body.get_span()),
            value: Function {
                name: Spanned {
                    span: open_span.to(body.get_span()),
                    value: self.random_ident(),
                },
                params: Spanned {
                    span: open_span.to(params_span),
                    value: params,
                },
                body,
                returns,
            },
        })
    }

    fn parse_ident(&mut self, symbol: Spanned<Symbol>) -> ParserResult<Spanned<Expression>> {
        if self.peek(|ch| ch == '{') {
            self.advance();

            let mut props = vec![];

            if !self.peek(|ch| ch == '}') {
                loop {
                    let (open_span, symbol) =
                        self.consume_get_symbol_and_span("Expected a field name")?;

                    self.consume(&TokenType::COLON, "Expected a colon")?;

                    let expr = self.parse_expression()?;

                    props.push(Spanned {
                        span: open_span.to(symbol.get_span()),
                        value: InstanceField { symbol, expr },
                    });

                    if self.peek(|ch| ch == ',') {
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
            if self.peek(|ch| ch == '(') {
                expr = self.finish_call(expr)?;
            } else if self.peek(|ch| ch == '[') {
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
            } else if self.peek(|ch| ch == '.') {
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

        if self.peek(|ch| ch == ')') {
            loop {
                args.push(self.parse_expression()?);

                if self.peek(|ch| ch == ',') {
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

        let superclass = if self.peek(|ch| ch == '<') {
            self.advance();
            Some(self.consume_get_symbol("Expected a superclass name")?)
        } else {
            None
        };

        let mut properties = vec![];
        let mut methods = vec![];

        self.consume(&TokenType::LBRACE, "Expected a '{' after class name")?;

        while !self.peek(|ch| ch == '}') {
            if !self.recognise(TokenType::FUNCTION)? {
                loop {
                    let (open_span, name) =
                        self.consume_get_symbol_and_span("Expected a property name")?;

                    self.consume(&TokenType::COLON, "Expected ':'")?;

                    let ty = self.parse_type()?;

                    properties.push(Spanned {
                        span: open_span.to(ty.get_span()),
                        value: Field { name, ty },
                    });

                    if self.peek(|ch| ch == ',') {
                        self.advance();
                    } else {
                        break;
                    }
                }

                self.consume(
                    &TokenType::SEMICOLON,
                    "Expected a semicolon after declaring properties",
                )?;

                if self.peek(|ch| ch == '}') {
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

#[inline]
fn token_with_info(token: TokenType) -> Token {
    Token { token }
}

#[inline]
fn span(token: TokenType, start: Position) -> Spanned<Token> {
    Spanned {
        value: token_with_info(token),
        span: Span { start, end: start },
    }
}

#[inline]
fn spans(token: TokenType, start: Position, end: Position) -> Spanned<Token> {
    Spanned {
        value: token_with_info(token),
        span: Span { start, end },
    }
}

#[inline]
fn is_letter_ch(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_'
}

#[inline]
fn look_up_identifier(id: &str) -> TokenType {
    match id {
        // Class
        "class" => TokenType::CLASS,
        "print" => TokenType::PRINT,
        "this" => TokenType::THIS,
        "type" => TokenType::TYPE,
        // Functions and vars
        "fn" => TokenType::FUNCTION,
        "let" => TokenType::LET,
        // Control Flow
        "if" => TokenType::IF,
        "else" => TokenType::ELSE,
        "for" => TokenType::FOR,
        "while" => TokenType::WHILE,
        "return" => TokenType::RETURN,
        "break" => TokenType::BREAK,
        "continue" => TokenType::CONTINUE,
        "do" => TokenType::DO,
        // Booleans
        "true" => TokenType::TRUE(true),
        "false" => TokenType::FALSE(false),
        "or" => TokenType::OR,
        "and" => TokenType::AND,
        "nil" => TokenType::NIL,
        _ => TokenType::IDENTIFIER(id),
    }
}
