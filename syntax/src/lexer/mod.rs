use token::{Token, TokenType};
use util::pos::{CharPosition, Position, Span, Spanned};
use util::emmiter::Reporter;
use std::fmt::{Display, Formatter};
use std::fmt;

#[derive(Debug)]
pub enum LexerError {
    UnclosedString,
    UnclosedBlockComment,
    EOF,
    Unexpected(char, Position),
}

impl Display for LexerError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            LexerError::UnclosedString => write!(f, "unclosed string"),
            LexerError::EOF => write!(f, "Unexpected EOf"),
            LexerError::UnclosedBlockComment => write!(f, "unclosed block comment"),
            LexerError::Unexpected(ref c, ref p) => write!(f, "Unexpected char {} on {}", c, p),
        }
    }
}

impl Into<String> for LexerError {
    fn into(self) -> String {
        match self {
            LexerError::UnclosedString => "Unclosed string".into(),
            LexerError::EOF => "Unexpected EOF".into(),
            LexerError::UnclosedBlockComment => "Unclosed block comment".into(),
            LexerError::Unexpected(ref c, _) => format!("Unexpected char '{}' ", c),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    // A lexer instance
    input: &'a str,
    chars: CharPosition<'a>,
    reporter: Reporter,
    lookahead: Option<(Position, char)>,
    end: Position,
}

impl<'a> Lexer<'a> {
    /// Returns a new Lexer
    pub fn new(input: &'a str, reporter: Reporter) -> Lexer {
        let mut chars = CharPosition::new(input);
        let end = chars.pos;
        Lexer {
            input,
            end,
            reporter,
            lookahead: chars.next(),
            chars,
        }
    }

    fn span_error<T: Into<String>>(&mut self, msg: T, start: Position, end: Position) {
        self.reporter.error(msg.into(), Span { start, end })
    }

    fn error<T: Into<String>>(&mut self, msg: T, pos: Position) {
        self.reporter.error(
            msg.into(),
            Span {
                start: pos,
                end: pos,
            },
        )
    }

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

    fn slice(&self, start: Position, end: Position) -> &'a str {
        &self.input[start.absolute..end.absolute]
    }

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

    fn peek<F>(&mut self, mut check: F) -> bool
    where
        F: FnMut(char) -> bool,
    {
        self.lookahead.map_or(false, |(_, ch)| check(ch))
    }

    fn line_comment(&mut self, start: Position) {
        let (_, _) = self.take_whilst(start, |ch| ch != '\n');
    }

    fn block_comment(&mut self) -> Result<(), LexerError> {
        self.advance(); // Eats the '*'
        loop {
            self.advance(); // Eats the '*'

            match self.lookahead {
                Some((_, '/')) => {
                    self.advance();
                    return Ok(());
                }
                Some((_, _)) => continue,

                None => return Err(LexerError::UnclosedBlockComment),
            }
        }
    }

    fn string_literal(&mut self, start: Position) -> Result<Spanned<Token<'a>>, LexerError> {
        let mut string = String::new();

        while let Some((next, ch)) = self.advance() {
            match ch {
                '"' => {
                    let end = next.shift(ch);

                    return Ok(spans(
                        TokenType::STRING(string.as_bytes().to_vec()),
                        start,
                        end,
                    ));
                }

                ch => string.push(ch),
            }
        }

        Err(LexerError::UnclosedString)
    }

    fn number(&mut self, start: Position) -> Result<Spanned<Token<'a>>, LexerError> {
        let (end, int) = self.take_whilst(start, |c| c.is_numeric());

        let (token, start, end) = match self.lookahead {
            Some((_, '.')) => {
                self.advance();

                let (end, float) = self.take_whilst(start, |c| c.is_numeric());

                match self.lookahead {
                    Some((_, ch)) if ch.is_alphabetic() => {
                        return Err(LexerError::Unexpected(ch, start)); // Rejects floats like 10.k
                    }

                    _ => (TokenType::FLOAT(float.parse().unwrap()), start, end),
                }
            }

            Some((_, ch)) if ch.is_alphabetic() => return Err(LexerError::Unexpected(ch, start)),
            None | Some(_) => {
                if let Ok(val) = int.parse() {
                    (TokenType::INT(val), start, end)
                } else {
                    return Err(LexerError::EOF); // change
                }
            }
        };

        Ok(spans(token, start, end))
    }

    fn identifier(&mut self, start: Position) -> Spanned<Token<'a>> {
        let (end, ident) = self.take_whilst(start, is_letter_ch);
        spans(look_up_identifier(ident), start, end)
    }

    fn next(&mut self) -> Result<Spanned<Token<'a>>, LexerError> {
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
                '^' => Ok(span(TokenType::EXPONENTIAL, start)),
                '%' => Ok(span(TokenType::MODULO, start)),
                '"' => match self.string_literal(start) {
                    Ok(token) => Ok(token),
                    Err(e) => {
                        let msg: String = e.into();
                        let end = self.end;
                        self.span_error(msg, start, end);
                        continue;
                    }
                },

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

                ch if ch.is_numeric() => self.number(start),
                ch if is_letter_ch(ch) => Ok(self.identifier(start)),
                ch if ch.is_whitespace() => continue,
                ch => {
                    let msg: String = LexerError::Unexpected(ch, start).into();
                    self.error(msg, start);
                    continue;
                }
            };
        }

        Ok(spans(TokenType::EOF, self.end, self.end))
    }

    pub fn lex(&mut self) -> Result<Vec<Spanned<Token<'a>>>, ()> {
        let mut tokens = vec![];

        let mut errors = vec![];

        while !self.lookahead.is_none() {
            match self.next() {
                Ok(token) => tokens.push(token),
                Err(e) => errors.push(e),
            }
        }

        if errors.is_empty() {
            return Ok(tokens);
        }

        Err(())
    }
}

#[inline]
fn is_letter_ch(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_'
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
fn token_with_info(token: TokenType) -> Token {
    Token { token }
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
        "fun" => TokenType::FUNCTION,
        "var" => TokenType::VAR,
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
