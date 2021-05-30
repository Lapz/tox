mod classes;
mod enums;
mod expressions;
mod function;
mod imports;
mod module;
mod params;
mod pattern;
mod pratt;
mod restrictions;
mod source_file;
mod type_alias;
mod type_args;
mod type_params;
mod types;
mod visibility;
use crate::Span;
use errors::{pos::Position, Reporter};
use pratt::{InfixParser, Precedence, PrefixParser, Rule as _, RuleToken};
use restrictions::Restrictions;
use rowan::{GreenNodeBuilder, TextRange, TextUnit};
use std::collections::HashMap;
use std::mem::replace;
use syntax::{
    SyntaxKind::{self, *},
    Token, T,
};

#[derive(Debug)]
enum State {
    PendingStart,
    Normal,
    PendingFinish,
}

pub struct Parser<'a> {
    input: &'a str,
    current: &'a Span<Token>,
    state: State,
    pub builder: GreenNodeBuilder<'static>,
    reporter: Reporter,
    text_pos: TextUnit,
    tokens: &'a [Span<Token>],
    token_pos: usize,
    prefix: HashMap<RuleToken, &'a dyn PrefixParser>,
    infix: HashMap<RuleToken, &'a dyn InfixParser>,
    restriction: Option<Restrictions>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Span<Token>], reporter: Reporter, input: &'a str) -> Self {
        let mut parser = Parser {
            current: &tokens[0],
            tokens,
            state: State::PendingStart,
            token_pos: 0,
            text_pos: 0.into(),
            builder: GreenNodeBuilder::new(),
            prefix: HashMap::new(),
            infix: HashMap::new(),
            reporter,
            input,
            restriction: None,
        };
        parser.prefix(RuleToken::LBracket, &expressions::LiteralParselet);
        parser.prefix(RuleToken::Literal, &expressions::LiteralParselet);
        parser.prefix(RuleToken::Ident, &expressions::IdentParselet);
        parser.prefix(RuleToken::Excl, &expressions::UnaryParselet);
        parser.prefix(RuleToken::Minus, &expressions::UnaryParselet);
        parser.prefix(RuleToken::LParen, &expressions::GroupingParselet);
        parser.prefix(RuleToken::Pipe, &expressions::ClosureParselet);

        parser.infix(
            RuleToken::LBrace,
            &expressions::RecordParselet(Precedence::Call),
        );

        parser.infix(
            RuleToken::LParen,
            &expressions::CallParselet(Precedence::Call),
        );

        parser.infix(
            RuleToken::LBracket,
            &expressions::IndexParselet(Precedence::Call),
        );

        parser.infix(
            RuleToken::Dot,
            &expressions::FieldParselet(Precedence::Call),
        );
        parser.infix(
            RuleToken::Eq,
            &expressions::BinaryParselet(Precedence::Assignment),
        );

        parser.infix(
            RuleToken::PlusEq,
            &expressions::BinaryParselet(Precedence::Assignment),
        );

        parser.infix(
            RuleToken::StarEq,
            &expressions::BinaryParselet(Precedence::Assignment),
        );

        parser.infix(
            RuleToken::MinusEq,
            &expressions::BinaryParselet(Precedence::Assignment),
        );

        parser.infix(
            RuleToken::SlashEq,
            &expressions::BinaryParselet(Precedence::Assignment),
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

    pub fn reporter(self) -> Reporter {
        self.reporter
    }

    fn prefix(&mut self, rule: RuleToken, parser: &'a dyn pratt::PrefixParser) {
        self.prefix.insert(rule, parser);
    }

    fn infix(&mut self, rule: RuleToken, parser: &'a dyn pratt::InfixParser) {
        self.infix.insert(rule, parser);
    }

    fn checkpoint(&self) -> rowan::Checkpoint {
        self.builder.checkpoint()
    }

    fn lookahead(&self) -> Option<&Span<Token>> {
        self.tokens.get(self.token_pos + 1)
    }

    fn start_node(&mut self, kind: SyntaxKind) {
        match replace(&mut self.state, State::Normal) {
            State::PendingStart => {
                self.builder.start_node(kind.into());
                // No need to attach trivia's to previous node: there is no
                // previous node.
                return;
            }
            State::PendingFinish => (),
            State::Normal => (),
        }

        let n_trivias = self.tokens[self.token_pos..]
            .iter()
            .take_while(|token| token.value.kind.is_trivia())
            .count();

        let leading_trivias = &self.tokens[self.token_pos..self.token_pos + n_trivias];
        let mut trivia_end: TextUnit = self.text_pos
            + leading_trivias
                .iter()
                .map(|it| TextUnit::from(it.value.len))
                .sum::<TextUnit>();

        let n_attached_trivias = {
            let leading_trivias = leading_trivias.iter().rev().map(|it| {
                let next_end = trivia_end - TextUnit::from(it.value.len);
                let range = TextRange::from_to(next_end, trivia_end);
                trivia_end = next_end;
                (it.value.kind, &self.input[range])
            });
            n_attached_trivias(kind, leading_trivias)
        };

        self.eat_n_trivias(n_trivias - n_attached_trivias);

        self.builder.start_node(kind.into());

        self.eat_n_trivias(n_attached_trivias)
    }

    fn eat_n_trivias(&mut self, n: usize) {
        for _ in 0..n {
            let token = &self.tokens[self.token_pos];
            assert!(token.value.kind.is_trivia());
            self.add_token(token);
        }
    }

    fn start_node_at(&mut self, checkpoint: rowan::Checkpoint, kind: SyntaxKind) {
        self.builder.start_node_at(checkpoint, kind.into());
    }

    fn finish_node(&mut self) {
        match replace(&mut self.state, State::PendingFinish) {
            State::PendingFinish | State::Normal => {
                self.builder.finish_node();
            }
            State::PendingStart => unreachable!(),
        }
    }

    fn recover(&mut self) {
        match self.current() {
            T!["{"] => self.recover_until(T!["}"]),
            T!["}"] => self.recover_until(T![;]),
            IDENT => self.recover_until_many(&[T!["}"], T![;], T![fn]]),
            _ => {
                self.bump();
            }
        }
    }

    fn recover_until_many(&mut self, tokens: &[SyntaxKind]) {
        while self.lookahead().is_some() && !self.matches(tokens.to_vec()) {
            self.bump();
        }

        self.bump(); // eat the token as well
    }

    fn recover_until(&mut self, token: SyntaxKind) {
        while self.lookahead().is_some() && !self.at(token) {
            self.bump();
        }

        self.bump(); // eat the token as well
    }

    fn error(&mut self, message: impl Into<String>, additional_info: impl Into<String>) {
        let message = message.into();
        let additional_info = additional_info.into();
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
        (self.current.start, self.current.end)
    }

    fn current_string(&self) -> &str {
        let token = self.current;

        &self.input[token.start.absolute as usize..token.end.absolute as usize]
    }

    fn precedence(&mut self) -> Precedence {
        let token = self.current();

        let rule = token.rule();

        self.infix
            .get(&rule)
            .map_or(Precedence::None, |parser| parser.pred())
    }

    fn expect(&mut self, expected: SyntaxKind) {
        if self.at(expected) {
            self.bump();
        } else {
            let current = self.current();
            self.error(
                format!("Expected `{}`", expected.text()),
                format!(
                    "Expected `{}` but instead found `{}`",
                    expected.text(),
                    current.text()
                ),
            );
        }
    }

    fn expects(&mut self, expected: Vec<SyntaxKind>) -> bool {
        for kind in expected {
            let current = self.current();
            if kind == current {
                self.bump();
                return true;
            }
        }

        false
    }

    fn expected(&mut self, expected: SyntaxKind) -> bool {
        if self.at(expected) {
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

    fn current(&mut self) -> SyntaxKind {
        while self.current.value.kind.is_trivia() {
            self.add_token(self.current);
        }
        self.current.value.kind
    }

    fn at(&mut self, check: SyntaxKind) -> bool {
        self.current() == check
    }

    fn matches(&mut self, kind: Vec<SyntaxKind>) -> bool {
        for kind in kind {
            if kind == self.current() {
                return true;
            }
        }
        false
    }

    fn add_token(&mut self, token: &Span<Token>) {
        let len = TextUnit::from(token.value.len);
        let range = TextRange::offset_len(token.start.absolute.into(), len);

        let text = &self.input[range];

        self.text_pos += len;
        self.token_pos += 1;

        self.current = &self.tokens[self.token_pos];
        self.builder.token(token.value.kind.into(), text.into());
    }

    pub fn bump(&mut self) {
        if self.at(SyntaxKind::EOF) {
            return;
        }

        self.add_token(self.current);
    }

    fn ident(&mut self) {
        self.start_node(NAME);
        self.expect(IDENT);
        self.finish_node()
    }
}

/// Taken from https://github.com/rust-analyzer/rust-analyzer/blob/918547dbe9a2907401102eba491ac25cebe1404d/crates/ra_syntax/src/parsing/text_tree_sink.rs
/// All copyright goes to the rust-analyzer devs
fn n_attached_trivias<'a>(
    kind: SyntaxKind,
    trivias: impl Iterator<Item = (SyntaxKind, &'a str)>,
) -> usize {
    use SyntaxKind::*;
    match kind {
        TYPE_ALIAS_DEF | CLASS_DEF | ENUM_DEF | ENUM_VARIANT | FN_DEF | MOD_DEF | IMPORT_DEF => {
            let mut res = 0;
            for (i, (kind, text)) in trivias.enumerate() {
                match kind {
                    WHITESPACE => {
                        if text.contains("\n\n") {
                            break;
                        }
                    }
                    COMMENT => {
                        res = i + 1;
                    }
                    _ => (),
                }
            }
            res
        }
        _ => 0,
    }
}
