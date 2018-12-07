use parser::{Parser, ParserResult};
use token::{Token, TokenType};
use util::pos::{Position, Span, Spanned};

impl<'a> Parser<'a> {
    /// Function that gets the next token and puts it within the VecDeque whilst returning the
    /// last token
    pub(crate) fn next(&mut self) -> ParserResult<Spanned<Token<'a>>> {
        let token = self.next_token()?;
        self.past_tokens.push_back(token);

        match self.past_tokens.pop_front() {
            Some(token) => Ok(token),
            None => Err(()),
        }
    }

    pub(crate) fn next_token(&mut self) -> ParserResult<Spanned<Token<'a>>> {
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
                    Err(_) => {
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
    pub(crate) fn advance(&mut self) -> Option<(Position, char)> {
        match self.lookahead {
            Some((pos, ch)) => {
                self.start = pos;
                self.end = self.end.shift(ch);
                self.lookahead = self.chars.next();
                Some((pos, ch))
            }

            None => None,
        }
    }

    /// Slice the input return the string contained
    /// between the supplied postion
    pub(crate) fn slice(&self, start: Position, end: Position) -> &'a str {
        &self.input[start.absolute..end.absolute]
    }

    /// Advance the input whilst the given function evaluates
    /// to true
    pub(crate) fn take_whilst<F>(
        &mut self,
        start: Position,
        mut terminate: F,
    ) -> (Position, &'a str)
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
    pub(crate) fn peek<F>(&mut self, mut check: F) -> bool
    where
        F: FnMut(char) -> bool,
    {
        self.lookahead.map_or(false, |(_, ch)| check(ch))
    }

    /// Reporter an error at the given character
    pub(crate) fn error<T: Into<String>>(&mut self, msg: T, pos: Position) {
        self.reporter.error(
            msg.into(),
            Span {
                start: pos,
                end: pos,
            },
        )
    }

    /// Reporter an error that spans the given positions
    pub(crate) fn spanned_error<T: Into<String>>(
        &mut self,
        msg: T,
        start: Position,
        end: Position,
    ) {
        self.reporter.error(msg.into(), Span { start, end })
    }

    /// Reporter an error with the given span
    pub(crate) fn span_error<T: Into<String>>(&mut self, msg: T, span: Span) {
        self.reporter.error(msg.into(), span)
    }

    /// Method that handles a line comment
    pub(crate) fn line_comment(&mut self, start: Position) {
        let (_, _) = self.take_whilst(start, |ch| ch != '\n');
    }

    /// Method that handles a block comment
    pub(crate) fn block_comment(&mut self) -> ParserResult<()> {
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
    pub(crate) fn string_literal(&mut self, start: Position) -> ParserResult<Spanned<Token<'a>>> {
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

        self.error("Unclosed string", last.unwrap()); // has to be the end as we keep on adding to our string till we reach the end

        Err(())
    }

    /// Handles number,both ints and floats
    pub(crate) fn number(&mut self, start: Position) -> ParserResult<Spanned<Token<'a>>> {
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
    pub(crate) fn identifier(&mut self, start: Position) -> Spanned<Token<'a>> {
        let (end, ident) = self.take_whilst(start, is_letter_ch);
        spans(look_up_identifier(ident), start, end)
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
