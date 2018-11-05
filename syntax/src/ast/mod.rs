use util::pos::Spanned;
use util::symbol::Symbol;

#[derive(Debug, Clone)]
pub struct Program {
    pub functions: Vec<Spanned<Function>>,
    pub classes: Vec<Spanned<Class>>,
    pub aliases: Vec<Spanned<TypeAlias>>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Spanned<Symbol>,
    pub params: Spanned<Vec<Spanned<FunctionParam>>>,
    pub body: Spanned<Statement>,
    pub returns: Option<Spanned<Type>>,
}

#[derive(Debug, Clone)]
pub struct Class {
    pub name: Spanned<Symbol>,
    pub superclass: Option<Spanned<Symbol>>,
    pub methods: Vec<Spanned<Function>>,
    pub fields: Vec<Spanned<Field>>,
}

#[derive(Debug, Clone)]
pub struct TypeAlias {
    pub alias: Spanned<Symbol>,
    pub ty: Spanned<Type>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Block(Vec<Spanned<Statement>>),
    Break,
    Continue,
    Expr(Spanned<Expression>),
    For {
        init: Option<Box<Spanned<Statement>>>,
        cond: Option<Spanned<Expression>>,
        incr: Option<Spanned<Expression>>,
        body: Box<Spanned<Statement>>,
    },

    If {
        cond: Spanned<Expression>,
        then: Box<Spanned<Statement>>,
        otherwise: Option<Box<Spanned<Statement>>>,
    },

    Print(Spanned<Expression>),

    While {
        cond: Spanned<Expression>,
        body: Box<Spanned<Statement>>,
    },

    VarDeclaration {
        ident: Spanned<Symbol>,
        ty: Option<Spanned<Type>>,
        expr: Option<Spanned<Expression>>,
    },

    Return(Spanned<Expression>),
}

#[derive(Debug, Clone)]
pub struct Field {
    pub name: Spanned<Symbol>,
    pub ty: Spanned<Type>,
}

#[derive(Debug, Clone)]
pub struct FunctionParam {
    pub name: Spanned<Symbol>,
    pub ty: Spanned<Type>,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Array {
        items: Vec<Spanned<Expression>>,
    },
    Assign {
        name: Spanned<Symbol>,
        kind: Spanned<AssignOperator>,
        value: Box<Spanned<Expression>>,
    },
    Binary {
        lhs: Box<Spanned<Expression>>,
        op: Spanned<Op>,
        rhs: Box<Spanned<Expression>>,
    },

    Closure(Spanned<Box<Function>>),
    Call {
        callee: Box<Spanned<Expression>>,
        args: Vec<Spanned<Expression>>,
    },

    ClassInstance {
        symbol: Spanned<Symbol>,
        props: Vec<Spanned<InstanceField>>,
    },

    Get {
        object: Box<Spanned<Expression>>,
        property: Spanned<Symbol>,
    },

    Grouping {
        expr: Box<Spanned<Expression>>,
    },

    SubScript {
        target: Box<Spanned<Expression>>,
        index: Box<Spanned<Expression>>,
    },

    Literal(Literal),

    Set {
        object: Box<Spanned<Expression>>,
        name: Spanned<Symbol>,
        value: Box<Spanned<Expression>>,
    },

    Ternary {
        condition: Box<Spanned<Expression>>,
        then_branch: Box<Spanned<Expression>>,
        else_branch: Box<Spanned<Expression>>,
    },
    Unary {
        op: Spanned<UnaryOp>,
        expr: Box<Spanned<Expression>>,
    },

    This,
    Var(Spanned<Symbol>),
}

#[derive(Debug, Clone)]
pub struct InstanceField {
    pub symbol: Spanned<Symbol>,
    pub expr: Spanned<Expression>,
}

#[derive(Debug, Clone)]
pub enum Type {
    Simple(Spanned<Symbol>),
    Arr(Box<Spanned<Type>>),
    Func(Vec<Spanned<Type>>, Option<Box<Spanned<Type>>>),
    Nil,
}

#[derive(Debug, Clone)]
pub enum Literal {
    // The raw values available
    Float(f64),
    Int(i64),
    Str(Vec<u8>),
    True(bool),
    False(bool),
    Nil,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
    // The possible operators for the binary and unary expression
    BangEqual,
    EqualEqual,
    LessThan,
    LessThanEqual,
    GreaterThanEqual,
    GreaterThan,
    Plus,
    Minus,
    Star,
    Slash,
    Modulo,
    Exponential,
    And,
    Or,
}

#[derive(Debug, Clone, Hash)]
pub enum AssignOperator {
    // The possible operators for the binary and unary expression
    Equal,
    MinusEqual,
    PlusEqual,
    StarEqual,
    SlashEqual,
}

#[derive(Debug, Clone, Hash)]
pub enum UnaryOp {
    Bang,
    Minus,
}
