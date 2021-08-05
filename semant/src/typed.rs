use syntax::TextUnit;

use crate::{
    hir::{BinOp, LiteralId, NameId, UnaryOp},
    util, Span, Type,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Typed<T> {
    pub ty: Type,
    pub item: T,
    pub span: (TextUnit, TextUnit),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    pub functions: Vec<Typed<Function>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub exported: bool,
    pub name: Span<NameId>,
    pub params: Vec<Typed<Param>>,
    pub body: Vec<Typed<Stmt>>,
}
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Param {
    pub pat: Typed<Pattern>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Pattern {
    Bind { name: util::Span<NameId> },
    Placeholder,
    Tuple(Vec<Typed<Pattern>>),
    Literal(LiteralId),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Stmt {
    Let {
        pat: Typed<Pattern>,
        ascribed_type: Option<Type>,
        initializer: Option<Typed<Expr>>,
    },
    Expr(Typed<Expr>),
    Error,
}
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Expr {
    Array(Vec<Typed<Expr>>),
    Binary {
        lhs: Box<Typed<Expr>>,
        op: BinOp,
        rhs: Box<Typed<Expr>>,
    },
    Block(Vec<Typed<Stmt>>, bool),
    Break,
    Call {
        callee: Box<Typed<Expr>>,
        args: Vec<Typed<Expr>>,
        type_args: Vec<Type>,
    },
    Cast {
        expr: Box<Typed<Expr>>,
        ty: Type,
    },
    Closure {
        // params: Vec<Param>,
    // body: util::Span<BlockId>,
    // returns: Option<util::Span<Type>>,
    },
    Continue,
    If {
        cond: Box<Typed<Expr>>,
        then_branch: Box<Typed<Expr>>,
        else_branch: Option<Box<Typed<Expr>>>,
    },
    Ident(util::Span<NameId>),
    Index {
        base: Box<Typed<Expr>>,
        index: Box<Typed<Expr>>,
    },
    While {
        cond: Box<Typed<Expr>>,
        body: Box<Typed<Expr>>,
    },
    Literal(LiteralId),
    Paren(Box<Typed<Expr>>),
    Tuple(Vec<Typed<Expr>>),
    Unary {
        op: UnaryOp,
        expr: Box<Typed<Expr>>,
    },
    Return(Option<Box<Typed<Expr>>>),
    Match {
        expr: Box<Typed<Expr>>,
        arms: Vec<MatchArm>,
    },
    Enum {
        def: Span<NameId>,
        variant: Span<NameId>,
        expr: Option<Box<Typed<Expr>>>,
    },
    RecordLiteral {
        def: util::Span<NameId>,
        fields: Vec<(util::Span<NameId>, Typed<Expr>)>,
    },
    Field(Vec<Typed<Expr>>),
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct MatchArm {
    pub(crate) pats: Vec<util::Span<Pattern>>,
    pub(crate) expr: Expr,
}

impl<T> Typed<T> {
    pub fn new(item: T, ty: Type, span: (TextUnit, TextUnit)) -> Typed<T> {
        Typed { item, ty, span }
    }
}
