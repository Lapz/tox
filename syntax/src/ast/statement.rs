use ast::expr::{Expression, Ty};
use util::pos::Spanned;
use util::symbol::Symbol;

#[derive(Debug, PartialOrd, Clone, PartialEq)]
pub enum Statement {
    Block(Vec<Spanned<Statement>>),
    Break,
    Class {
        name: Spanned<Symbol>,
        superclass: Option<Spanned<Symbol>>,
        body: Spanned<(Vec<Spanned<Statement>>, Vec<Spanned<Field>>)>,
    },

    Continue,

    Expr(Spanned<Expression>),

    ExternFunction {
        name: Spanned<Symbol>,
        params: Spanned<Vec<Spanned<FunctionParams>>>,
        returns: Option<Spanned<Ty>>,
    },

    Function {
        name: Spanned<Symbol>,
        params: Spanned<Vec<Spanned<FunctionParams>>>,
        body: Box<Spanned<Statement>>,
        returns: Option<Spanned<Ty>>,
    },

    For {
        init: Option<Box<Spanned<Statement>>>,
        cond: Option<Spanned<Expression>>,
        incr: Option<Spanned<Expression>>,
        body: Box<Spanned<Statement>>,
    },

    If {
        cond: Spanned<Expression>,
        then: Box<Spanned<Statement>>,
        otherwise: Option<Box<Spanned<Statement>>>,
    },

    Print(Spanned<Expression>),

    While {
        cond: Spanned<Expression>,
        body: Box<Spanned<Statement>>,
    },

    Var {
        ident: Spanned<Symbol>,
        ty: Option<Spanned<Ty>>,
        expr: Option<Spanned<Expression>>,
    },

    Return(Spanned<Expression>),

    TypeAlias {
        alias: Spanned<Symbol>,
        ty: Spanned<Ty>,
    },
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct Field {
    pub name: Spanned<Symbol>,
    pub ty: Spanned<Ty>,
}

#[derive(Debug, PartialOrd, PartialEq, Clone)]
pub struct FunctionParams {
    pub name: Spanned<Symbol>,
    pub ty: Spanned<Ty>,
}
