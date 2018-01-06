use ast::expr::{Expression, ExpressionTy};
use pos::WithPos;
use symbol::Symbol;

#[derive(Debug, PartialOrd, Clone, PartialEq)]
pub enum Statement {
    ExpressionStmt(WithPos<Expression>),
    Var(Symbol, Option<WithPos<Expression>>, Option<ExpressionTy>),
    Block(Vec<WithPos<Statement>>),
    Class {
        name: Symbol,
        superclass: Option<Symbol>,
        methods: Vec<WithPos<Statement>>,
        properties: Vec<(Symbol, ExpressionTy)>,
    },

    IfStmt {
        condition: WithPos<Expression>,
        then_branch: Box<WithPos<Statement>>,
        else_branch: Option<Box<WithPos<Statement>>>,
    },

    Print(WithPos<Expression>),
    WhileStmt {
        condition: WithPos<Expression>,
        body: Box<WithPos<Statement>>,
    },
    ForStmt {
        initializer: Option<Box<WithPos<Statement>>>,
        condition: Option<WithPos<Expression>>,
        increment: Option<WithPos<Expression>>,
        body: Box<WithPos<Statement>>,
    },
    Function {
        name: Symbol,
        body: WithPos<Expression>,
    },

    DoStmt {
        condition: WithPos<Expression>,
        body: Box<WithPos<Statement>>,
    },

    Break,
    Continue,
    TypeAlias {
        alias: Symbol,
        ty: ExpressionTy,
    },
    Return(Option<WithPos<Expression>>),
}
