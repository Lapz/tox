use ast::expr::{Expression, Variable};

#[derive(Debug, PartialOrd, Clone, PartialEq)]
pub enum Statement<'a> {
    ExpressionStmt(Expression<'a>),
    PPrint(Expression<'a>),
    Var(Variable<'a>, Expression<'a>),
    Block(Vec<Statement<'a>>),
    IfStmt(Box<If<'a>>),
    WhileStmt(Box<While<'a>>),
    Function(Box<Function<'a>>),
    DoStmt(Box<Do<'a>>),
    Break,
    Continue,
    Class(Class<'a>),
    Return(Option<Expression<'a>>),
}

#[derive(Debug, PartialOrd, Clone, PartialEq)]
pub struct Class<'a> {
    pub name: Variable<'a>,
    pub methods: Vec<Statement<'a>>,
}
#[derive(Debug, PartialOrd, Clone, PartialEq)]
pub struct If<'a> {
    pub condition: Expression<'a>,
    pub then_branch: Statement<'a>,
    pub else_branch: Option<Statement<'a>>,
}

#[derive(Debug, PartialOrd, Clone, PartialEq)]
pub struct Do<'a> {
    pub condition: Expression<'a>,
    pub do_thing: Statement<'a>,
}

#[derive(Debug, PartialOrd, Clone, PartialEq)]
pub struct Function<'a> {
    pub name: Variable<'a>,
    pub function: Expression<'a>,
}


#[derive(Debug, PartialOrd, Clone, PartialEq)]
pub struct While<'a> {
    pub condition: Expression<'a>,
    pub body: Statement<'a>,
}

#[derive(Debug, PartialOrd, Clone, PartialEq)]
pub struct For<'a> {
    pub initializer: Statement<'a>,
    pub condition: Expression<'a>,
    pub increment: Expression<'a>,
    pub body: Statement<'a>,
}
