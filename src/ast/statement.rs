use ast::expr::{Expression, Variable};
use pos::WithPos;
#[derive(Debug, PartialOrd, Clone, PartialEq)]
pub enum Statement<'a> {
    ExpressionStmt(Expression<'a>),
    PPrint(Expression<'a>),
    Var(Variable<'a>, Expression<'a>),
    Block(Vec<WithPos<Statement<'a>>>),
    Class {
        name: Variable<'a>,
        methods: Vec<WithPos<Statement<'a>>>,
    },
    IfStmt {
        condition: Expression<'a>,
        then_branch: Box<WithPos<Statement<'a>>>,
        else_branch: Option<Box<WithPos<Statement<'a>>>>,
    },
    WhileStmt {
        condition: Expression<'a>,
        body: Box<WithPos<Statement<'a>>>,
    },

    ForStmt {
        initializer: Box<WithPos<Statement<'a>>>,
        condition: Expression<'a>,
        increment: Expression<'a>,
        body: Box<WithPos<Statement<'a>>>,
    },

    Function {
        name: Variable<'a>,
        body: Expression<'a>,
    },

    DoStmt {
        condition: Expression<'a>,
        body: Box<WithPos<Statement<'a>>>,
    },
    Break,
    Continue,
    Return(Option<Expression<'a>>),
}

#[derive(Debug, PartialOrd, Clone, PartialEq)]
pub struct Class<'a> {
    pub name: Variable<'a>,
    pub methods: Vec<Statement<'a>>,
}
