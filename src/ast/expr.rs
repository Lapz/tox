use ast::statement::Statement;
use std::fmt::{Display, Formatter};
use std::fmt;

#[derive(Debug, PartialOrd, Clone, PartialEq)]
pub enum Expression<'a> {
    // The different type of expressions availabe
    IndexExpr(Box<IndexExpr<'a>>),
    Array(Array<'a>),
    Assign(Box<Assign<'a>>),
    Dict(Dictionary<'a>),
    Binary(Box<Binary<'a>>),
    Call(Box<Call<'a>>),
    Grouping(Box<Grouping<'a>>),
    Literal(Literal),
    Logical(Box<Logical<'a>>),
    Ternary(Box<Ternary<'a>>),
    Unary(Box<Unary<'a>>),
    Var(Variable<'a>, VariableUseHandle),
    Func(Box<Func<'a>>),
    Get(Box<Get<'a>>),
    Set(Box<Set<'a>>),
    This(VariableUseHandle),
}

#[derive(Debug, PartialOrd, Clone, PartialEq)]
pub struct IndexExpr<'a> {
    pub target: Expression<'a>,
    pub index: Expression<'a>,
}


#[derive(Debug, PartialOrd, Clone, PartialEq)]
pub struct Array<'a> {
    pub items: Vec<Expression<'a>>,
}

#[derive(Debug, PartialOrd, Clone, PartialEq)]
pub struct Func<'a> {
    pub parameters: Vec<Variable<'a>>,
    pub body: Vec<Statement<'a>>,
}

#[derive(Debug, PartialOrd, Clone, PartialEq)]
pub struct Assign<'a> {
    pub handle: VariableUseHandle,
    pub name: Variable<'a>,
    pub kind: AssignOperator,
    pub value: Expression<'a>,
}

#[derive(Debug, PartialOrd, Clone, PartialEq)]
pub struct Binary<'a> {
    pub left_expr: Expression<'a>,
    pub operator: Operator,
    pub right_expr: Expression<'a>,
}

#[derive(Debug, PartialOrd, Clone, PartialEq)]
pub struct Call<'a> {
    pub callee: Expression<'a>,
    pub arguments: Vec<Expression<'a>>,
}

#[derive(Debug, PartialOrd, Clone, PartialEq)]
pub struct Grouping<'a> {
    pub expr: Expression<'a>,
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


#[derive(Debug, PartialOrd, Clone, PartialEq)]
pub struct Logical<'a> {
    pub left: Expression<'a>,
    pub operator: LogicOperator,
    pub right: Expression<'a>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Dictionary<'a> {
    pub items: Vec<(Expression<'a>, Expression<'a>)>,
}
#[derive(Debug, PartialOrd, Clone, PartialEq)]
pub struct Ternary<'a> {
    pub condition: Expression<'a>,
    pub then_branch: Expression<'a>,
    pub else_branch: Expression<'a>,
}

#[derive(Debug, PartialOrd, Clone, PartialEq)]
pub struct Unary<'a> {
    pub operator: UnaryOperator,
    pub expr: Expression<'a>,
}

#[derive(Debug, PartialOrd, Clone, PartialEq)]
pub struct Get<'a> {
    pub object: Expression<'a>,
    pub name: Variable<'a>,
    pub handle: VariableUseHandle,
}

#[derive(Debug, PartialOrd, Clone, PartialEq)]
pub struct Set<'a> {
    pub object: Expression<'a>,
    pub handle: VariableUseHandle,
    pub name: Variable<'a>,
    pub value: Expression<'a>,
}

#[derive(Debug, PartialOrd, Clone, PartialEq, Eq, Hash)]
pub struct Variable<'a>(pub &'a str);
// Operators


impl<'a> Display for Variable<'a> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
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
pub enum Operator {
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
}


#[derive(Debug, PartialOrd, Clone, PartialEq, Hash)]
pub enum AssignOperator {
    // The possible operators for the binary and unary expression
    Equal,
    MinusEqual,
    PlusEqual,
    StarEqual,
}

use token::TokenType;
#[inline]
pub(crate) fn get_assign_type(token: TokenType) -> AssignOperator {
    match token {
        TokenType::BANGEQUAL => AssignOperator::StarEqual,
        TokenType::MINUSASSIGN => AssignOperator::MinusEqual,
        TokenType::PLUSASSIGN => AssignOperator::PlusEqual,
        TokenType::ASSIGN => AssignOperator::Equal,
        _ => unreachable!(),
    }
}

#[inline]
pub(crate) fn get_operator(token: TokenType) -> Operator {
    match token {
        TokenType::BANGEQUAL => Operator::BangEqual,
        TokenType::EQUALEQUAL => Operator::EqualEqual,
        TokenType::LESSTHAN => Operator::LessThan,
        TokenType::LESSTHANEQUAL => Operator::LessThanEqual,
        TokenType::GREATERTHAN => Operator::GreaterThan,
        TokenType::GREATERTHANEQUAL => Operator::GreaterThanEqual,
        TokenType::PLUS => Operator::Plus,
        TokenType::MINUS => Operator::Minus,
        TokenType::STAR => Operator::Star,
        TokenType::SLASH => Operator::Slash,
        TokenType::MODULO => Operator::Modulo,
        TokenType::EXPONENTIAL => Operator::Exponential,
        _ => unreachable!(),
    }
}

#[inline]
pub(crate) fn get_unary_operator(token: TokenType) -> UnaryOperator {
    match token {
        TokenType::BANG => UnaryOperator::Bang,
        TokenType::MINUS => UnaryOperator::Minus,
        _ => unreachable!(),
    }
}

#[derive(Debug, PartialOrd, Clone, PartialEq, Hash)]
pub enum UnaryOperator {
    Bang,
    Minus,
}

#[derive(Debug, PartialOrd, Clone, PartialEq, Hash)]
pub enum LogicOperator {
    Or,
    And,
}
