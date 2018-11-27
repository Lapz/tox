use infer::types::Type;

pub(crate) use syntax::ast::{AssignOperator, Literal, Op, UnaryOp};
use util::pos::Spanned;
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
    pub body: Box<Spanned<Statement>>,
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
    pub expr: Box<Spanned<Expression>>,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct FunctionParam {
    pub name: Symbol,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Block(Vec<Spanned<Statement>>),
    Break,
    Continue,
    Expr(Spanned<TypedExpression>),

    If {
        cond: Spanned<TypedExpression>,
        then: Box<Spanned<Statement>>,
        otherwise: Option<Box<Spanned<Statement>>>,
    },

    Print(Spanned<TypedExpression>),

    While(Spanned<TypedExpression>, Box<Spanned<Statement>>),

    Var {
        ident: Symbol,
        ty: Type,
        expr: Option<Spanned<TypedExpression>>,
    },

    Return(Spanned<TypedExpression>),
}

#[derive(Debug, Clone)]
pub enum Expression {
    // The different type of expressions availabe
    Array(Vec<Spanned<TypedExpression>>),
    Assign(Symbol, AssignOperator, Spanned<TypedExpression>),
    Binary(Spanned<TypedExpression>, Op, Spanned<TypedExpression>),
    Call(Symbol, Vec<Spanned<TypedExpression>>),
   
    Closure(Box<Function>),
    ClassInstance(Symbol, Vec<(Symbol, Spanned<TypedExpression>)>),
    Get(Symbol, Spanned<TypedExpression>),
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
        class_name:Symbol,
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
