use ast::statement::Statement;
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
        left_expr: Box<Spanned<Expression>>,
        operator: Spanned<Op>,
        right_expr: Box<Spanned<Expression>>,
    },
    Call {
        callee: Box<Spanned<Expression>>,
        arguments: Vec<Spanned<Expression>>,
    },

    ClassInstance {
        name: Symbol,
        properties: Vec<(Spanned<Symbol>, Spanned<Expression>)>,
    },
    Dict {
        items: Vec<(Spanned<Expression>, Spanned<Expression>)>,
    },
    Func {
        params: Spanned<Vec<Spanned<FunctionParams>>>,
        body: Box<Spanned<Statement>>,
        returns: Option<Spanned<Ty>>,
    },
    Get {
        object: Box<Spanned<Expression>>,
        property: Spanned<Symbol>,
        handle: VariableUseHandle,
    },
    Grouping {
        expr: Box<Spanned<Expression>>,
    },

    IndexExpr {
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
        operator: Spanned<UnaryOp>,
        expr: Box<Spanned<Expression>>,
    },

    This(VariableUseHandle),
    Var(Spanned<Symbol>, VariableUseHandle),
}

#[derive(Debug, PartialOrd, PartialEq, Clone)]
pub struct FunctionParams {
    pub name: Spanned<Symbol>,
    pub ty: Spanned<Ty>,
}

#[derive(Debug, PartialOrd, Clone, PartialEq)]
pub enum Ty {
    Simple(Symbol),
    Arr(Box<Ty>),
    Func(Vec<Ty>, Option<Box<Ty>>),
    Nil,
}

#[derive(Debug, PartialOrd, Clone, PartialEq)]
pub enum Literal {
    // The raw values available
    Float(f64),
    Int(i64),
    Str(String),
    True(bool),
    False(bool),
    Nil,
}

#[derive(Debug, PartialOrd, PartialEq, Clone, Copy, Hash, Eq)]
pub struct VariableUseHandle(pub u64);

#[derive(Debug, Clone, Copy)]
pub struct VariableUseMaker {
    next_value: u64,
}

impl VariableUseMaker {
    pub fn new() -> Self {
        VariableUseMaker { next_value: 0 }
    }

    pub fn next(&mut self) -> VariableUseHandle {
        let value = self.next_value;

        self.next_value += 1;
        VariableUseHandle(value)
    }
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

use token::TokenType;
#[inline]
pub(crate) fn get_assign_operator(token: &TokenType) -> AssignOperator {
    match *token {
        TokenType::BANGEQUAL => AssignOperator::StarEqual,
        TokenType::MINUSASSIGN => AssignOperator::MinusEqual,
        TokenType::PLUSASSIGN => AssignOperator::PlusEqual,
        TokenType::ASSIGN => AssignOperator::Equal,
        TokenType::SLASHASSIGN => AssignOperator::SlashEqual,
        _ => unreachable!(),
    }
}

#[derive(Debug, PartialOrd, Clone, PartialEq, Hash)]
pub enum UnaryOp {
    Bang,
    Minus,
}
