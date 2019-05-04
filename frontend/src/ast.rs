use crate::infer::types::Type;

use crate::infer::types::Property;
pub(crate) use syntax::ast::{AssignOperator, Literal, Op, UnaryOp};
use util::pos::Spanned;
use util::symbol::Symbol;

#[derive(Debug, Clone)]
pub struct Program {
    pub functions: Vec<Function>,
    pub classes: Vec<Class>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Symbol,
    pub params: Vec<FunctionParam>,
    pub body: Box<Spanned<TypedStatement>>,
    pub returns: Type,
}
#[derive(Debug, Clone)]
pub struct Class {
    pub name: Symbol,
    pub superclass: Option<Spanned<Symbol>>,
    pub properties: Vec<Property>,
    pub methods: Vec<Function>,
}

#[derive(Debug, Clone)]
pub struct Method {
    pub name: Symbol,
    pub params: Vec<Type>,
    pub returns: Type,
}

#[derive(Debug, Clone)]
pub struct TypedExpression {
    pub expr: Box<Spanned<Expression>>,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct TypedStatement {
    pub statement: Box<Spanned<Statement>>,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct FunctionParam {
    pub name: Symbol,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Option<Spanned<TypedExpression>>,
    pub body: Spanned<TypedStatement>,
    pub is_all: bool,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Block(Vec<Spanned<TypedStatement>>),
    Break,
    Continue,
    Expr(Spanned<TypedExpression>),

    If {
        cond: Spanned<TypedExpression>,
        then: Spanned<TypedStatement>,
        otherwise: Option<Spanned<TypedStatement>>,
    },

    Print(Spanned<TypedExpression>),

    While(Spanned<TypedExpression>, Spanned<TypedStatement>),

    Let {
        ident: Symbol,
        ty: Type,
        expr: Option<Spanned<TypedExpression>>,
    },

    Return(Spanned<TypedExpression>),
}
#[derive(Debug, Clone)]
pub struct ClassLiteralProperty {
    pub name: Symbol,
    pub expr: Spanned<TypedExpression>,
}

#[derive(Debug, Clone)]
pub enum Expression {
    // The different type of expressions availabe
    Array(Vec<Spanned<TypedExpression>>),
    Assign(Symbol, AssignOperator, Spanned<TypedExpression>),
    Binary(Spanned<TypedExpression>, Op, Spanned<TypedExpression>),
    Call(Symbol, Vec<Spanned<TypedExpression>>),

    Cast(Spanned<TypedExpression>, Type),

    Closure(Box<Function>),
    ClassLiteral {
        symbol: Symbol,
        properties: Vec<Spanned<ClassLiteralProperty>>,
    },
    GetProperty {
        property_name: Symbol,
        property: Spanned<TypedExpression>,
    },
    GetMethod {
        method_name: Symbol,
        method: Spanned<TypedExpression>,
    },

    Grouping(Spanned<TypedExpression>),

    Index(Spanned<TypedExpression>, Spanned<TypedExpression>),
    InstanceMethodCall {
        method_name: Symbol,
        instance: Spanned<TypedExpression>,
        params: Vec<Spanned<TypedExpression>>,
    },
    Literal(Literal),
    /// Name, Object, Value
    Set(Symbol, Spanned<TypedExpression>, Spanned<TypedExpression>),
    StaticMethodCall {
        class_name: Symbol,
        method_name: Symbol,
        params: Vec<Spanned<TypedExpression>>,
    },
    Ternary(
        Spanned<TypedExpression>,
        Spanned<TypedExpression>,
        Spanned<TypedExpression>,
    ),
    Unary(UnaryOp, Spanned<TypedExpression>),
    Var(Symbol, Type),
}
