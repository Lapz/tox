use crate::pos::{CharPosition, Position, Span};
use crate::token::{SyntaxKind, Token};
use rowan::SmolStr;

pub type LexerResult<T> = Result<T, ()>;

pub struct Lexer<'a> {
    input: &'a str,
    chars: CharPosition<'a>,
    /// The character ahead
    lookahead: Option<(Position, char)>,

    past_tokens: Vec<Span<Token>>,

    start: Position,
    end: Position,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut chars = CharPosition::new(input);
        let end = chars.pos;

        Self {
            lookahead: chars.next(),
            input,
            chars,
            start: end,
            end,
            past_tokens: Vec::new(),
        }
    }

    pub fn lex(&mut self) -> Vec<Span<Token>> {
        let mut tokens = Vec::new();
        if self.input.is_empty() {
            return Vec::new();
        }

        while let Ok(token) = self.next() {
            if token.value.kind == SyntaxKind::EOF {
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

    pub(crate) fn next_token(&mut self) -> Span<Token> {
        while let Some((start, ch)) = self.advance() {
            return match ch {
                '.' => span(SyntaxKind::DOT, start),
                '?' => span(SyntaxKind::QUESTION, start),
                ';' => span(SyntaxKind::SEMICOLON, start),
                '{' => span(SyntaxKind::L_BRACE, start),
                '}' => span(SyntaxKind::R_BRACE, start),
                '[' => span(SyntaxKind::L_BRACKET, start),
                ']' => span(SyntaxKind::R_BRACKET, start),
                '(' => span(SyntaxKind::L_PAREN, start),
                ')' => span(SyntaxKind::R_PAREN, start),
                ',' => span(SyntaxKind::COMMA, start),
                '_' => span(SyntaxKind::UNDERSCORE, start),
                '|' => span(SyntaxKind::BAR, start),
                '^' => span(SyntaxKind::EXPONENTIAL, start),
                '%' => span(SyntaxKind::MODULO, start),
                ':' => {
                    if self.peek(|ch| ch == ':') {
                        self.advance();
                        spans(SyntaxKind::COLON_COLON, start, start.shift(':'))
                    } else {
                        span(SyntaxKind::COLON, start)
                    }
                }
                '!' => {
                    if self.peek(|ch| ch == '=') {
                        self.advance();
                        spans(SyntaxKind::BANG_EQUAL, start, start.shift(':'))
                    } else {
                        span(SyntaxKind::BANG, start)
                    }
                }

                '>' => {
                    if self.peek(|ch| ch == '=') {
                        self.advance();
                        spans(SyntaxKind::GREATER_EQUAL, start, start.shift(':'))
                    } else {
                        span(SyntaxKind::GREATER, start)
                    }
                }

                '<' => {
                    if self.peek(|ch| ch == '=') {
                        self.advance();
                        spans(SyntaxKind::LESS_EQUAL, start, start.shift(':'))
                    } else {
                        span(SyntaxKind::LESS, start)
                    }
                }

                '+' => {
                    if self.peek(|ch| ch == '=') {
                        self.advance();
                        spans(SyntaxKind::PLUS_ASSIGN, start, start.shift('='))
                    } else {
                        span(SyntaxKind::PLUS, start)
                    }
                }

                '-' => {
                    if self.peek(|ch| ch == '=') {
                        self.advance();
                        spans(SyntaxKind::MINUS_ASSIGN, start, start.shift('='))
                    } else if self.peek(|ch| ch == '>') {
                        self.advance();
                        spans(SyntaxKind::FRETURN, start, start.shift('>'))
                    } else {
                        span(SyntaxKind::MINUS, start)
                    }
                }

                '*' => {
                    if self.peek(|ch| ch == '=') {
                        self.advance();
                        spans(SyntaxKind::STAR_ASSIGN, start, start.shift('='))
                    } else {
                        span(SyntaxKind::STAR, start)
                    }
                }

                '"' => self.string_literal(start),
                '/' => {
                    if self.peek(|ch| ch == '=') {
                        self.advance();
                        spans(SyntaxKind::SLASH_ASSIGN, start, start.shift('='))
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
                ch if ch.is_whitespace() => continue,
                _ => span(SyntaxKind::ERROR, start),
            };
        }

        span(SyntaxKind::EOF, self.end)
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
            match ch {
                '"' => {
                    let end = next.shift(ch);

                    return spans(SyntaxKind::STRING, start, end);
                }

                ch => (),
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
        "class" => SyntaxKind::CLASS,
        "extends" => SyntaxKind::EXTENDS,
        "print" => SyntaxKind::PRINT,
        "type" => SyntaxKind::TYPE,
        "as" => SyntaxKind::AS,
        "match" => SyntaxKind::MATCH,
        "enum" => SyntaxKind::ENUM,
        "export" => SyntaxKind::EXPORT,
        // Functions and vars
        "fn" => SyntaxKind::FUNCTION,
        "let" => SyntaxKind::LET,
        // Control Flow
        "if" => SyntaxKind::IF,
        "else" => SyntaxKind::ELSE,
        "for" => SyntaxKind::FOR,
        "while" => SyntaxKind::WHILE,
        "return" => SyntaxKind::RETURN,
        "break" => SyntaxKind::BREAK,
        "continue" => SyntaxKind::CONTINUE,
        "do" => SyntaxKind::DO,

        // Booleans
        "true" => SyntaxKind::TRUE,
        "false" => SyntaxKind::FALSE,
        "or" => SyntaxKind::OR,
        "and" => SyntaxKind::AND,
        "nil" => SyntaxKind::NIL,
        _ => SyntaxKind::IDENTIFIER,
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

#[cfg(test)]
mod test {

    use super::{Lexer, SyntaxKind, Token};
    use insta::assert_debug_snapshot_matches;
    use SyntaxKind::*;

    #[test]
    fn it_works() {
        let tokens = Lexer::new("fn main() {print('hello')}").lex();

        assert_debug_snapshot_matches!(tokens)
    }
    #[test]
    fn lex_int() {
        let tokens = Lexer::new("12,34,,45,67,89,0").lex();

        assert_debug_snapshot_matches!(tokens)
    }

    #[test]
    fn lex_floats() {
        let tokens = Lexer::new("12.34,45.67,89.10").lex();

        assert_debug_snapshot_matches!(tokens)
    }

    #[test]
    fn lex_block_comments() {
        let tokens = Lexer::new("12 /*this is a comment */ 34").lex();

        assert_debug_snapshot_matches!(tokens)
    }

    #[test]
    fn lex_nested_block_comments() {
        let tokens = Lexer::new("/*this /*is a nested */  comment */").lex();

        assert_debug_snapshot_matches!(tokens)
    }

    #[test]
    fn lex_line_comment() {
        let tokens = Lexer::new("12 // this is a line comment").lex();

        assert_debug_snapshot_matches!(tokens)
    }
}
