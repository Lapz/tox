mod classes;
mod enums;
mod expressions;
mod function;
mod params;
mod pattern;
mod pratt;
mod source_file;

mod type_alias;
mod type_params;
mod types;
mod visibility;
use crate::Span;
use errors::{pos::Position, Reporter};
use pratt::{InfixParser, Precedence, PrefixParser, Rule as _, RuleToken};
use rowan::GreenNodeBuilder;
use std::collections::{HashMap, VecDeque};
use std::iter::Peekable;
use syntax::{
    SyntaxKind::{self, *},
    Token,
};
pub struct Parser<'a, I>
where
    I: Iterator<Item = Span<Token>>,
{
    input: &'a str,
    pub builder: GreenNodeBuilder,
    pub past_tokens: VecDeque<Span<Token>>,
    reporter: Reporter,
    lookahead: Option<Span<Token>>,
    iter: Peekable<I>,
    prefix: HashMap<RuleToken, &'a dyn PrefixParser<I>>,
    infix: HashMap<RuleToken, &'a dyn InfixParser<I>>,
}

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Span<Token>>,
{
    pub fn new(iter: I, reporter: Reporter, input: &'a str) -> Self {
        let mut iter = iter.peekable();
        let mut parser = Parser {
            lookahead: iter.next(),
            builder: GreenNodeBuilder::new(),
            past_tokens: VecDeque::new(),
            prefix: HashMap::new(),
            infix: HashMap::new(),
            reporter,
            iter,
            input,
        };

        parser.prefix(RuleToken::Literal, &expressions::LiteralParselet);
        parser.prefix(RuleToken::Ident, &expressions::IdentParselet);
        parser.prefix(RuleToken::Excl, &expressions::UnaryParselet);
        parser.prefix(RuleToken::Minus, &expressions::UnaryParselet);
        parser.prefix(RuleToken::LParen, &expressions::GroupingParselet);
        // parser.prefix(RuleToken::)
        //
        parser.infix(
            RuleToken::LParen,
            &expressions::CallParselet(Precedence::Call),
        );

        parser.infix(
            RuleToken::Plus,
            &expressions::BinaryParselet(Precedence::Term),
        );
        parser.infix(
            RuleToken::Minus,
            &expressions::BinaryParselet(Precedence::Term),
        );
        parser.infix(
            RuleToken::Slash,
            &expressions::BinaryParselet(Precedence::Term),
        );
        parser.infix(
            RuleToken::Star,
            &expressions::BinaryParselet(Precedence::Term),
        );
        parser.infix(
            RuleToken::Comparison,
            &expressions::BinaryParselet(Precedence::Comparison),
        );

        parser.infix(
            RuleToken::AmpAmp,
            &expressions::BinaryParselet(Precedence::And),
        );

        parser.infix(
            RuleToken::PipePipe,
            &expressions::BinaryParselet(Precedence::Or),
        );

        parser.infix(
            RuleToken::EqEq,
            &expressions::BinaryParselet(Precedence::Equality),
        );

        parser
    }

    fn prefix(&mut self, rule: RuleToken, parser: &'a dyn pratt::PrefixParser<I>) {
        self.prefix.insert(rule, parser);
    }

    fn infix(&mut self, rule: RuleToken, parser: &'a dyn pratt::InfixParser<I>) {
        self.infix.insert(rule, parser);
    }

    fn checkpoint(&self) -> rowan::Checkpoint {
        self.builder.checkpoint()
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
        while self.at(SyntaxKind::WHITESPACE) {
            self.bump()
        }
        self.builder.start_node(kind.into())
    }

    fn start_node_at(&mut self, checkpoint: rowan::Checkpoint, kind: SyntaxKind) {
        while self.at(SyntaxKind::WHITESPACE) {
            self.bump()
        }
        self.builder.start_node_at(checkpoint, kind.into())
    }

    fn finish_node(&mut self) {
        self.builder.finish_node();
    }

    fn recover(&mut self) {
        if self.at(T!["{"]) || self.at(T!["}"]) {
            return;
        }

        self.bump();
    }

    fn recover_until(&mut self, token: SyntaxKind) {
        while !self.lookahead.is_none() && !self.at(token) {
            self.bump();
        }

        self.bump(); // eat the token as well
    }

    fn error(&mut self, message: impl Into<String>, additional_info: impl Into<String>) {
        self.start_node(SyntaxKind::ERROR);

        self.recover();

        self.reporter
            .error(message, additional_info, self.current_span());
        self.finish_node()
    }

    fn error_until(
        &mut self,
        message: impl Into<String>,
        additional_info: impl Into<String>,
        token: SyntaxKind,
    ) {
        self.start_node(SyntaxKind::ERROR);

        self.reporter
            .error(message, additional_info, self.current_span());

        self.recover_until(token);

        self.finish_node()
    }

    fn current_span(&self) -> (Position, Position) {
        self.lookahead
            .as_ref()
            .map(|token| (token.start, token.end))
            .unwrap_or_else(|| {
                let token = self.past_tokens.front().unwrap();
                (token.start, token.end)
            })
    }

    fn current_string(&self) -> &str {
        self.lookahead
            .as_ref()
            .map(|token| {
                &self.input[token.start.absolute as usize
                    ..token.start.absolute as usize + token.value.len as usize]
            })
            .unwrap_or("")
    }

    fn precedence(&self) -> Precedence {
        let token = self.current();

        let rule = token.rule();

        self.infix
            .get(&rule)
            .map_or(Precedence::None, |parser| parser.pred())
    }

    fn expect<T: Into<String>>(&mut self, expected: SyntaxKind, _msg: T) {
        while self.is_ahead(|t| t == SyntaxKind::WHITESPACE) || self.at(SyntaxKind::WHITESPACE) {
            self.bump()
        }

        if self.is_ahead(|t| t == expected) {
            self.bump();
        } else {
            self.error(
                format!("Expected `{}`", expected.text()),
                format!(
                    "Expected `{}` but instead found `{}`",
                    expected.text(),
                    self.current().text()
                ),
            );
        }
    }

    fn expected(&mut self, expected: SyntaxKind) -> bool {
        while self.is_ahead(|t| t == SyntaxKind::WHITESPACE) || self.at(SyntaxKind::WHITESPACE) {
            self.bump()
        }

        if self.is_ahead(|t| t == expected) {
            self.bump();
            true
        } else {
            self.error_until(
                format!("Expected `{}`", expected.text()),
                format!(
                    "Expected `{}` but instead found `{}` ",
                    expected.text(),
                    self.current_string()
                ),
                expected,
            );
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

    pub fn bump(&mut self) {
        if self.at(SyntaxKind::EOF) {
            return;
        }
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
