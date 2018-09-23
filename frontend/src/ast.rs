use infer::types::Type;
use syntax::ast::expr::{AssignOperator, Literal, Op, UnaryOp};
use util::symbol::Symbol;

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: Symbol,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Method {
    pub name: Symbol,
    pub params: Vec<Type>,
    pub returns: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedExpression {
    pub expr: Box<Expression>,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionParam {
    pub name: Symbol,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Block(Vec<Statement>),
    Break,
    Class {
        name: Symbol,
        methods: Vec<Statement>,
        fields: Vec<Field>,
    },

    Continue,

    Expr(TypedExpression),

    Function {
        name: Symbol,
        params: Vec<FunctionParam>,
        body: Box<Statement>,
        returns: Type,
    },

    If {
        cond: TypedExpression,
        then: Box<Statement>,
        otherwise: Option<Box<Statement>>,
    },

    Print(TypedExpression),

    While(TypedExpression, Box<Statement>),

    Var {
        ident: Symbol,
        ty: Type,
        expr: Option<TypedExpression>,
    },

    Return(TypedExpression),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    // The different type of expressions availabe
    Array(Vec<TypedExpression>),
    Assign(Symbol, AssignOperator, TypedExpression),
    Binary(TypedExpression, Op, TypedExpression),
    Call(TypedExpression, Vec<TypedExpression>),

    ClassInstance(Symbol, Vec<TypedExpression>),
    Get(Symbol, TypedExpression),
    Grouping(TypedExpression),

    Index(Symbol, TypedExpression),

    Literal(Literal),
    /// Name, Object, Value
    Set(Symbol, TypedExpression, TypedExpression),

    Ternary(TypedExpression, TypedExpression, TypedExpression),
    Unary(UnaryOp, TypedExpression),

    This,
    Var(Symbol, Type),
}
