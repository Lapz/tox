use ast::expr::{Expression, Ty};
use util::pos::Spanned;
use util::symbol::Symbol;

#[derive(Debug, PartialOrd, Clone, PartialEq)]
pub enum Statement {
    Var(Symbol, Option<Spanned<Expression>>, Option<Spanned<Ty>>),
    Block(Vec<Spanned<Statement>>),
    Break,
    Class {
        name: Spanned<Symbol>,
        superclass: Option<Spanned<Symbol>>,
        methods: Vec<Spanned<Statement>>,
        properties: Vec<(Spanned<Symbol>, Ty)>,
    },
    Continue,

    DoStmt {
        condition: Spanned<Expression>,
        body: Box<Spanned<Statement>>,
    },

    ExpressionStmt(Spanned<Expression>),

    ExternFunction {
        name: Spanned<Symbol>,
        params: Vec<(Symbol, Ty)>,
        returns: Option<Ty>,
    },

    Function {
        name: Symbol,
        body: Spanned<Expression>,
    },

    ForStmt {
        initializer: Option<Box<Spanned<Statement>>>,
        condition: Option<Spanned<Expression>>,
        increment: Option<Spanned<Expression>>,
        body: Box<Spanned<Statement>>,
    },

    IfStmt {
        condition: Spanned<Expression>,
        then_branch: Box<Spanned<Statement>>,
        else_branch: Option<Box<Spanned<Statement>>>,
    },

    Print(Spanned<Expression>),

    WhileStmt {
        condition: Spanned<Expression>,
        body: Box<Spanned<Statement>>,
    },

    Return(Option<Spanned<Expression>>),

    TypeAlias {
        alias: Spanned<Symbol>,
        ty: Spanned<Ty>,
    },
}
