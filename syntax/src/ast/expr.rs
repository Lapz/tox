use util::pos::Spanned;
use util::symbol::Symbol;

#[derive(Debug, PartialOrd, Clone, PartialEq)]
pub enum Expression {
    // The different type of expressions availabe
    Array {
        items: Vec<Spanned<Expression>>,
    },
    Assign {
        handle: VariableUseHandle,
        name: Spanned<Symbol>,
        kind: Spanned<AssignOperator>,
        value: Box<Spanned<Expression>>,
    },
    Binary {
        lhs: Box<Spanned<Expression>>,
        op: Spanned<Op>,
        rhs: Box<Spanned<Expression>>,
    },
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
        handle: VariableUseHandle,
    },
    Grouping {
        expr: Box<Spanned<Expression>>,
    },

    Index {
        target: Box<Spanned<Expression>>,
        index: Box<Spanned<Expression>>,
    },

    Literal(Literal),

    Set {
        object: Box<Spanned<Expression>>,
        handle: VariableUseHandle,
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

    This(VariableUseHandle),
    Var(Spanned<Symbol>, VariableUseHandle),
}

#[derive(Debug, PartialOrd, PartialEq, Clone)]
pub struct InstanceField {
    pub symbol: Spanned<Symbol>,
    pub expr: Spanned<Expression>,
}

#[derive(Debug, PartialOrd, Clone, PartialEq)]
pub enum Ty {
    Simple(Spanned<Symbol>),
    Arr(Box<Spanned<Ty>>),
    Func(Vec<Spanned<Ty>>, Option<Box<Spanned<Ty>>>),
    Nil,
}

#[derive(Debug, PartialOrd, Clone, PartialEq)]
pub enum Literal {
    // The raw values available
    Float(f64),
    Int(i64),
    Str(Vec<u8>),
    True(bool),
    False(bool),
    Nil,
}

#[derive(Debug, PartialOrd, PartialEq, Clone, Copy, Hash, Eq)]
pub struct VariableUseHandle(pub u64);

#[derive(Debug, Clone, Copy,Default)]
pub struct VariableUseMaker {
    next_value: u64,
}

impl VariableUseMaker {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn next_handle(&mut self) -> VariableUseHandle {
        let value = self.next_value;

        self.next_value += 1;
        VariableUseHandle(value)
    }
}

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Var {
    Field {
        ident: Spanned<Symbol>,
        value: Spanned<Symbol>,
    },
    Simple(Spanned<Symbol>, VariableUseHandle),
    SubScript {
        expr: Box<Spanned<Expression>>,
        target: Spanned<Symbol>,
    },
}

#[derive(Debug, PartialOrd, Clone, PartialEq)]
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

#[derive(Debug, PartialOrd, Clone, PartialEq, Hash)]
pub enum AssignOperator {
    // The possible operators for the binary and unary expression
    Equal,
    MinusEqual,
    PlusEqual,
    StarEqual,
    SlashEqual,
}

#[derive(Debug, PartialOrd, Clone, PartialEq, Hash)]
pub enum UnaryOp {
    Bang,
    Minus,
}
