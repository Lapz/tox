use crate::ast::SyntaxKind;

use crate::token::Token;
use errors::{
    pos::{CharPosition, Position, Span},
    Reporter,
};

pub type LexerResult<T> = Result<T, ()>;

pub struct Lexer<'a> {
    input: &'a str,
    chars: CharPosition<'a>,
    /// The character ahead
    lookahead: Option<(Position, char)>,
    reporter: Reporter,
    past_tokens: Vec<Span<Token>>,
    start: Position,
    end: Position,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str, reporter: Reporter) -> Self {
        let mut chars = CharPosition::new(input);
        let end = chars.pos;

        Self {
            lookahead: chars.next(),
            input,
            reporter,
            chars,
            start: end,
            end,
            past_tokens: Vec::new(),
        }
    }

    pub fn reporter(self) -> Reporter {
        self.reporter
    }

    pub fn lex(&mut self) -> Vec<Span<Token>> {
        let mut tokens = Vec::new();

        if self.input.is_empty() {
            return Vec::new();
        }

        while let Ok(token) = self.next() {
            if token.value.kind == SyntaxKind::EOF {
                tokens.push(token);
                break;
            }
            tokens.push(token);
        }

        tokens
    }
    /// Function that gets the next token and puts it within the Vec whilst returning the
    /// last token
    fn next(&mut self) -> LexerResult<Span<Token>> {
        let token = self.next_token();
        self.past_tokens.push(token);

        match self.past_tokens.pop() {
            Some(token) => Ok(token),
            None => Err(()),
        }
    }

    fn error(
        &mut self,
        message: impl Into<String>,
        additional_info: impl Into<String>,
        span: (Position, Position),
    ) {
        self.reporter.error(message, additional_info, span)
    }

    /// Advances the input return the current position and the char we are at
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
    /// between the supplied position
    pub(crate) fn slice(&self, start: Position, end: Position) -> &'a str {
        &self.input[start.absolute as usize..end.absolute as usize]
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
    pub(crate) fn peek<F>(&self, mut check: F) -> bool
    where
        F: FnMut(char) -> bool,
    {
        self.lookahead.map_or(false, |(_, ch)| check(ch))
    }

    pub(crate) fn next_token(&mut self) -> Span<Token> {
        while let Some((start, ch)) = self.advance() {
            return match ch {
                '.' => span(SyntaxKind::DOT, start),
                '?' => span(SyntaxKind::QUESTION, start),
                ';' => span(SyntaxKind::SEMI, start),
                '{' => span(SyntaxKind::L_CURLY, start),
                '}' => span(SyntaxKind::R_CURLY, start),
                '[' => span(SyntaxKind::L_BRACK, start),
                ']' => span(SyntaxKind::R_BRACK, start),
                '(' => span(SyntaxKind::L_PAREN, start),
                ')' => span(SyntaxKind::R_PAREN, start),
                ',' => span(SyntaxKind::COMMA, start),
                '_' => span(SyntaxKind::UNDERSCORE, start),
                '|' => span(SyntaxKind::PIPE, start),
                '^' => span(SyntaxKind::EXPONENTIAL, start),
                '%' => span(SyntaxKind::PERCENT, start),
                '&' => {
                    if self.peek(|ch| ch == '&') {
                        self.advance();
                        spans(SyntaxKind::AMPAMP, start, start.span('&'))
                    } else {
                        span(SyntaxKind::AMP, start)
                    }
                }
                ':' => {
                    if self.peek(|ch| ch == ':') {
                        self.advance();
                        spans(SyntaxKind::COLON_COLON, start, start.span(':'))
                    } else {
                        span(SyntaxKind::COLON, start)
                    }
                }
                '!' => {
                    if self.peek(|ch| ch == '=') {
                        self.advance();
                        spans(SyntaxKind::NEQ, start, start.span(':'))
                    } else {
                        span(SyntaxKind::EXCL, start)
                    }
                }

                '=' => {
                    if self.peek(|ch| ch == '=') {
                        self.advance();
                        spans(SyntaxKind::EQEQ, start, start.span('='))
                    } else if self.peek(|ch| ch == '>') {
                        self.advance();
                        spans(SyntaxKind::FAT_ARROW, start, start.span('>'))
                    } else {
                        span(SyntaxKind::EQ, start)
                    }
                }

                '>' => {
                    if self.peek(|ch| ch == '=') {
                        self.advance();
                        spans(SyntaxKind::GTEQ, start, start.span(':'))
                    } else {
                        span(SyntaxKind::R_ANGLE, start)
                    }
                }

                '<' => {
                    if self.peek(|ch| ch == '=') {
                        self.advance();
                        spans(SyntaxKind::LTEQ, start, start.span(':'))
                    } else {
                        span(SyntaxKind::L_ANGLE, start)
                    }
                }

                '+' => {
                    if self.peek(|ch| ch == '=') {
                        self.advance();
                        spans(SyntaxKind::PLUSEQ, start, start.span('='))
                    } else {
                        span(SyntaxKind::PLUS, start)
                    }
                }

                '-' => {
                    if self.peek(|ch| ch == '=') {
                        self.advance();
                        spans(SyntaxKind::MINUSEQ, start, start.span('='))
                    } else if self.peek(|ch| ch == '>') {
                        self.advance();
                        spans(SyntaxKind::FRETURN, start, start.span('>'))
                    } else {
                        span(SyntaxKind::MINUS, start)
                    }
                }

                '*' => {
                    if self.peek(|ch| ch == '=') {
                        self.advance();

                        spans(SyntaxKind::STAREQ, start, start.span('='))
                    } else {
                        span(SyntaxKind::STAR, start)
                    }
                }

                '"' => self.string_literal(start),
                '/' => {
                    if self.peek(|ch| ch == '=') {
                        self.advance();

                        spans(SyntaxKind::SLASHEQ, start, start.span('='))
                    } else if self.peek(|ch| ch == '/') {
                        self.advance();
                        self.line_comment(start)
                    } else if self.peek(|ch| ch == '*') {
                        self.block_comment(start)
                    } else {
                        span(SyntaxKind::SLASH, start)
                    }
                }

                ch if ch.is_numeric() => self.number(start),
                ch if is_letter_ch(ch) => self.identifier(start),
                ch if ch.is_whitespace() => {
                    let (end, _) = self.take_whilst(start, char::is_whitespace);
                    spans(SyntaxKind::WHITESPACE, start, end)
                }
                ch => {
                    self.error(
                        "Unknown character",
                        format!("Unknown character `{}`", ch),
                        (start, start),
                    );
                    spans(SyntaxKind::ERROR, start, start.shift(ch))
                }
            };
        }

        span(SyntaxKind::EOF, self.end)
    }

    /// Handle a line comment
    pub(crate) fn line_comment(&mut self, start: Position) -> Span<Token> {
        let (end, _) = self.take_whilst(start, |ch| ch != '\n');

        spans(SyntaxKind::COMMENT, start, end)
    }

    /// Handle  a block comment
    pub(crate) fn block_comment(&mut self, start: Position) -> Span<Token> {
        self.advance(); // Eats the '*'

        let mut depth = 1usize;

        while let Some((_, c)) = self.advance() {
            match c {
                '/' if self.peek(|ch| ch == '*') => {
                    self.advance();
                    depth += 1;
                }

                '*' if self.peek(|ch| ch == '/') => {
                    self.advance();
                    depth -= 1;

                    if depth == 0 {
                        break;
                    }
                }

                _ => (),
            }
        }

        if depth == 0 {
            spans(SyntaxKind::COMMENT, start, self.end)
        } else {
            spans(SyntaxKind::ERROR, start, self.end)
        }
    }

    fn string_literal(&mut self, start: Position) -> Span<Token> {
        while let Some((next, ch)) = self.advance() {
            if ch == '"' {
                let end = next.shift(ch);

                return spans(SyntaxKind::STRING, start, end);
            }
        }

        spans(SyntaxKind::ERROR, start, self.end)
    }

    /// Handles any identifier.
    // New key words should be added to the look_up_identifier function
    pub(crate) fn identifier(&mut self, start: Position) -> Span<Token> {
        let (end, ident) = self.take_whilst(start, is_letter_ch);
        spans(look_up_identifier(ident), start, end)
    }

    /// Handles number,both int's and floats
    pub(crate) fn number(&mut self, start: Position) -> Span<Token> {
        let (end, _) = self.take_whilst(start, char::is_numeric);

        match self.lookahead {
            Some((end, '.')) => {
                self.advance();

                if self.peek(char::is_numeric) {
                    let (end, _) = self.take_whilst(start, char::is_numeric);

                    spans(SyntaxKind::FLOAT_NUMBER, start, end)
                } else {
                    spans(SyntaxKind::ERROR, start, end)
                }
            }

            Some((end, ch)) if ch.is_alphabetic() => spans(SyntaxKind::ERROR, start, end),

            Some((end, _)) => spans(SyntaxKind::INT_NUMBER, start, end),
            None => spans(SyntaxKind::INT_NUMBER, start, end),
        }
    }
}

#[inline]
fn is_letter_ch(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_'
}

#[inline]
fn look_up_identifier(id: &str) -> SyntaxKind {
    match id {
        // Class
        "class" => SyntaxKind::CLASS_KW,
        "extends" => SyntaxKind::EXTENDS_KW,
        "type" => SyntaxKind::TYPE_KW,
        "as" => SyntaxKind::AS_KW,
        "match" => SyntaxKind::MATCH_KW,
        "enum" => SyntaxKind::ENUM_KW,
        "import" => SyntaxKind::IMPORT_KW,
        "mod" => SyntaxKind::MOD_KW,
        "export" => SyntaxKind::EXPORT_KW,
        "self" => SyntaxKind::SELF_KW,
        // Functions and vars
        "fn" => SyntaxKind::FN_KW,
        "let" => SyntaxKind::LET_KW,
        // Control Flow
        "if" => SyntaxKind::IF_KW,
        "else" => SyntaxKind::ELSE_KW,
        "for" => SyntaxKind::FOR_KW,
        "while" => SyntaxKind::WHILE_KW,
        "return" => SyntaxKind::RETURN_KW,
        "break" => SyntaxKind::BREAK_KW,
        "continue" => SyntaxKind::CONTINUE_KW,
        "do" => SyntaxKind::DO_KW,

        // Booleans
        "true" => SyntaxKind::TRUE_KW,
        "false" => SyntaxKind::FALSE_KW,
        "or" => SyntaxKind::OR_KW,
        "and" => SyntaxKind::AND_KW,
        "nil" => SyntaxKind::NIL_KW,
        _ => SyntaxKind::IDENT,
    }
}

#[inline]
fn span(token: SyntaxKind, start: Position) -> Span<Token> {
    Span::new(Token::new(token, 1), start, start)
}

#[inline]
fn spans(token: SyntaxKind, start: Position, end: Position) -> Span<Token> {
    Span::new(Token::new(token, end.absolute - start.absolute), start, end)
}
