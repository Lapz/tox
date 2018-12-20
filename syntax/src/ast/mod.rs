use util::pos::Spanned;
use util::symbol::Symbol;

#[derive(Debug, Clone)]
pub struct Program {
    pub functions: Vec<Spanned<Function>>,
    pub classes: Vec<Spanned<Class>>,
    pub aliases: Vec<Spanned<TypeAlias>>,
}

#[derive(Debug, Clone)]
pub struct ItemName {
    pub name: Spanned<Symbol>,
    pub type_params: Vec<Spanned<Symbol>>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Spanned<ItemName>,
    pub params: Spanned<Vec<Spanned<FunctionParam>>>,
    pub body: Spanned<Statement>,
    pub returns: Option<Spanned<Type>>,
}

#[derive(Debug, Clone)]
pub struct Class {
    pub name: Spanned<ItemName>,
    pub superclass: Option<Spanned<Symbol>>,
    pub methods: Vec<Spanned<Function>>,
    pub fields: Vec<Spanned<Field>>,
}

#[derive(Debug, Clone)]
pub struct TypeAlias {
    pub alias: Spanned<ItemName>,
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

    Closure(Box<Spanned<Function>>),
    Call(Spanned<Call>),

    ClassLiteral(Spanned<ClassLiteral>),

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

    Var(Spanned<Symbol>),
}

#[derive(Debug, Clone)]
pub struct ClassLiteralField {
    pub symbol: Spanned<Symbol>,
    pub expr: Spanned<Expression>,
}

#[derive(Debug, Clone)]
pub enum ClassLiteral {
    /// A class literal with no provided types
    /// i.e Foo {
    ///     int:0
    /// }
    Simple {
        symbol: Spanned<Symbol>,
        props: Vec<Spanned<ClassLiteralField>>,
    },
    /// A class literal with  provided types
    /// i.e Foo::<i32> {
    ///     int:0
    /// }   
    Instantiation {
        symbol: Spanned<Symbol>,
        types: Spanned<Vec<Spanned<Type>>>,
        props: Vec<Spanned<ClassLiteralField>>,
    },
}

#[derive(Debug, Clone)]
pub enum Call {
    /// A function/method literal with no provided types
    /// i.e foo.bar(1,2,3); add(10,2);
    Simple {
        args: Vec<Spanned<Expression>>,
        callee: Box<Spanned<Expression>>,
    },

    /// A function/method literal with provided types
    /// i.e id::<i32>(foo); foo.add::<i32,i32>(10,2);
    Instantiation {
        args: Vec<Spanned<Expression>>,
        callee: Box<Spanned<Expression>>,
        types: Spanned<Vec<Spanned<Type>>>,
    },
}
#[derive(Debug, Clone)]
pub enum Type {
    /// Type that is an identifier i.e bool,int,float
    Simple(Spanned<Symbol>),
    /// Type that is an array i.e [int]
    Arr(Box<Spanned<Type>>),
    /// Type that is a function i.e fn(int,int) -> int;
    Func(Vec<Spanned<Type>>, Option<Box<Spanned<Type>>>),
    /// Type that is nill
    /// TODO remove and rely on Simple
    Nil,
    /// Type of a generic type i.e List<i32> Foo<List<List<i32>>>
    Generic(Spanned<Symbol>, Vec<Spanned<Type>>),
}

#[derive(Debug, Clone)]
pub enum Literal {
    // The raw values available
    Float(f64),
    Int(i64),
    Str(String),
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
