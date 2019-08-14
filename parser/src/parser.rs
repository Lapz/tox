mod classes;
mod enums;
mod expressions;
mod function;
mod params;
mod pattern;
mod source_file;
mod statements;
mod type_alias;
mod type_params;
mod types;
mod visibility;
use rowan::GreenNodeBuilder;
use std::collections::VecDeque;
use std::iter::Peekable;
use syntax::{
    Span,
    SyntaxKind::{self, *},
    Token,
};
pub struct Parser<'a, I>
where
    I: Iterator<Item = Span<Token>>,
{
    input: &'a str,
    pub builder: GreenNodeBuilder,
    past_tokens: VecDeque<Span<Token>>,
    lookahead: Option<Span<Token>>,
    iter: Peekable<I>,
}

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Span<Token>>,
{
    pub fn new(iter: I, input: &'a str) -> Self {
        let mut iter = iter.peekable();
        Parser {
            lookahead: iter.next(),
            builder: GreenNodeBuilder::new(),
            past_tokens: VecDeque::new(),
            iter,
            input,
        }
    }

    pub(crate) fn is_ahead<F>(&self, mut check: F) -> bool
    where
        F: FnMut(SyntaxKind) -> bool,
    {
        self.lookahead
            .as_ref()
            .map_or(false, |token| check(token.value.kind))
    }

    pub(crate) fn peek(&mut self) -> SyntaxKind {
        self.iter
            .peek()
            .as_ref()
            .map_or(SyntaxKind::EOF, |token| token.value.kind)
    }

    fn start_node(&mut self, kind: SyntaxKind) {
        self.builder.start_node(kind.into())
    }

    fn finish_node(&mut self) {
        self.builder.finish_node();
    }

    fn expect<T: Into<String>>(&mut self, expected: SyntaxKind, msg: T) {
        if self.is_ahead(|t| t == expected) {
        } else {
            // panic!(
            //     "Expected {:?} found {:?} ahead is {:?}",
            //     expected,
            //     self.current(),
            //     self.peek()
            // )
        }

        self.bump();
    }

    fn expected(&mut self, expected: SyntaxKind) -> bool {
        if self.is_ahead(|t| t == expected) {
            self.bump();
            true
        } else {
            self.error("");
            false
        }
    }

    fn current(&self) -> SyntaxKind {
        self.lookahead
            .as_ref()
            .map(|token| token.value.kind)
            .unwrap_or(EOF)
    }

    fn at(&self, check: SyntaxKind) -> bool {
        self.current() == check
    }

    fn matches(&self, kind: Vec<SyntaxKind>) -> bool {
        for kind in kind {
            if kind == self.current() {
                return true;
            }
        }
        false
    }

    fn error(&mut self, err: &str) {
        //TODO report error

        self.bump();
    }

    pub fn bump(&mut self) {
        let token = self.lookahead.take();
        match token {
            Some(token) => {
                let text = &self.input[token.start.absolute as usize
                    ..token.start.absolute as usize + token.value.len as usize];
                self.builder.token(token.value.kind.into(), text.into());
                self.past_tokens.push_front(token);
                self.lookahead = self.iter.next()
            }
            None => {}
        }
    }

    fn ident(&mut self) {
        self.start_node(NAME);
        self.expect(IDENT, "Expected an identifier");
        self.finish_node()
    }
}
