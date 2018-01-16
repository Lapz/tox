use ast::expr::{Expression, ExpressionTy};
use util::pos::WithPos;
use util::symbol::Symbol;

#[derive(Debug, PartialOrd, Clone, PartialEq)]
pub enum Statement {
    
    Var(Symbol, Option<WithPos<Expression>>, Option<ExpressionTy>),
    Block(Vec<WithPos<Statement>>),
    Break,
    Class {
        name: Symbol,
        superclass: Option<Symbol>,
        methods: Vec<WithPos<Statement>>,
        properties: Vec<(Symbol, ExpressionTy)>,
    },
    Continue,
    
    DoStmt {
        condition: WithPos<Expression>,
        body: Box<WithPos<Statement>>,
    },

    ExpressionStmt(WithPos<Expression>),

    Function {
        name: Symbol,
        body: WithPos<Expression>,
    },

    ForStmt {
        initializer: Option<Box<WithPos<Statement>>>,
        condition: Option<WithPos<Expression>>,
        increment: Option<WithPos<Expression>>,
        body: Box<WithPos<Statement>>,
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

    Return(Option<WithPos<Expression>>),

    TypeAlias {
        alias: Symbol,
        ty: ExpressionTy,
    },
}
