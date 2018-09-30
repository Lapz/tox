use infer::types;
use infer::types::Type;
use std::collections::HashMap;
pub(crate) use syntax::ast::{AssignOperator, Literal, Op, UnaryOp};
use util::symbol::Symbol;

#[derive(Debug)]
pub struct Program {
    pub functions: Vec<Function>,
    pub classes: Vec<Class>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Symbol,
    pub params: Vec<FunctionParam>,
    pub body: Box<Statement>,
    pub returns: Type,
}
#[derive(Debug, Clone)]
pub struct Class {
    pub name: Symbol,
    pub fields: Vec<Field>,
    pub methods: Vec<Function>,
}
#[derive(Debug, Clone)]
pub struct Field {
    pub name: Symbol,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct Method {
    pub name: Symbol,
    pub params: Vec<Type>,
    pub returns: Type,
}

#[derive(Debug, Clone)]
pub struct TypedExpression {
    pub expr: Box<Expression>,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct FunctionParam {
    pub name: Symbol,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Block(Vec<Statement>),
    Break,
    Continue,
    Expr(TypedExpression),

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

#[derive(Debug, Clone)]
pub enum Expression {
    // The different type of expressions availabe
    Array(Vec<TypedExpression>),
    Assign(Symbol, AssignOperator, TypedExpression),
    Binary(TypedExpression, Op, TypedExpression),
    Call(TypedExpression, Vec<TypedExpression>),
    Closure(Box<Function>),
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
