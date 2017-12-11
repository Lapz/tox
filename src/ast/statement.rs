use ast::expr::Expression;
use types::Type;
use pos::WithPos;
use symbol::Symbol;

#[derive(Debug, PartialOrd, Clone, PartialEq)]
pub enum Statement {
    ExpressionStmt(Expression),
    Var(Symbol, Expression,Option<Type>),
    Block(Vec<WithPos<Statement>>),
    Class {
        name: Symbol,
        methods: Vec<WithPos<Statement>>,
    },
    IfStmt {
        condition: Expression,
        then_branch: Box<WithPos<Statement>>,
        else_branch: Option<Box<WithPos<Statement>>>,
    },
    WhileStmt {
        condition: Expression,
        body: Box<WithPos<Statement>>,
    },

    ForStmt {
        initializer: Box<WithPos<Statement>>,
        condition: Expression,
        increment: Expression,
        body: Box<WithPos<Statement>>,
    },

    Function {
        name: Symbol,
        body: Expression,
    },

    DoStmt {
        condition: Expression,
        body: Box<WithPos<Statement>>,
    },

    Break,
    Continue,
    Return(Option<Expression>),
}
