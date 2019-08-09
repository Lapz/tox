mod function;
mod params;
mod pattern;
mod source_file;
mod type_params;
mod types;

use rowan::{GreenNode, GreenNodeBuilder};
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

    pub(crate) fn peek<F>(&self, mut check: F) -> bool
    where
        F: FnMut(SyntaxKind) -> bool,
    {
        self.lookahead
            .as_ref()
            .map_or(false, |token| check(token.value.kind))
    }

    fn start_node(&mut self, kind: SyntaxKind) {
        self.builder.start_node(kind.into())
    }

    fn finish_node(&mut self) {
        self.builder.finish_node();
    }

    fn expect<T: Into<String>>(&mut self, expected: SyntaxKind, msg: T) {
        if self.peek(|t| t == expected) {
        } else {
            self.error("")
        }

        self.bump();
    }

    fn expected(&mut self, expected: SyntaxKind) -> bool {
        if self.peek(|t| t == expected) {
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
            .unwrap_or(ERROR)
    }

    fn at(&self, check: SyntaxKind) -> bool {
        self.current() == check
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
        self.start_node(IDENT);
        self.bump();
        self.finish_node()
    }
}
