use ast::statement::Statement;
use pos::WithPos;
use std::fmt::{Display, Formatter};
use std::fmt;

#[derive(Debug, PartialOrd, Clone, PartialEq)]
pub enum Expression<'a> {
    // The different type of expressions availabe
    IndexExpr {
        target: Box<Expression<'a>>,
        index: Box<Expression<'a>>,
    },
    Array {
        items: Vec<Expression<'a>>,
    },
    Assign {
        handle: VariableUseHandle,
        name: Variable<'a>,
        kind: AssignOperator,
        value: Box<Expression<'a>>,
    },

    Binary {
        left_expr: Box<Expression<'a>>,
        operator: Operator,
        right_expr: Box<Expression<'a>>,
    },
    Call {
        callee: Box<Expression<'a>>,
        arguments: Vec<Expression<'a>>,
    },
    Dict {
        items: Vec<(Expression<'a>, Expression<'a>)>,
    },
    Func {
        parameters: Vec<Variable<'a>>,
        body: Box<WithPos<Statement<'a>>>,
    },
    Get {
        object: Box<Expression<'a>>,
        name: Variable<'a>,
        handle: VariableUseHandle,
    },
    Grouping {
        expr: Box<Expression<'a>>,
    },
    Literal(Literal),
    Logical {
        left: Box<Expression<'a>>,
        operator: LogicOperator,
        right: Box<Expression<'a>>,
    },

    Set {
        object: Box<Expression<'a>>,
        handle: VariableUseHandle,
        name: Variable<'a>,
        value: Box<Expression<'a>>,
    },

    Ternary {
        condition: Box<Expression<'a>>,
        then_branch: Box<Expression<'a>>,
        else_branch: Box<Expression<'a>>,
    },
    Unary {
        operator: UnaryOperator,
        expr: Box<Expression<'a>>,
    },

    This(VariableUseHandle),
    Var(Variable<'a>, VariableUseHandle),
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
    Comma,
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
pub(crate) fn get_assign_operator(token: TokenType) -> AssignOperator {
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
        TokenType::COMMA => Operator::Comma,
        _ => unreachable!(),
    }
}

#[inline]
pub(crate) fn get_unary_operator(token: TokenType) -> UnaryOperator {
    match token {
        TokenType::BANG => UnaryOperator::Bang,
        TokenType::MINUS => UnaryOperator::Minus,
        TokenType::PLUS => UnaryOperator::Plus,
        _ => unreachable!(),
    }
}

#[inline]
pub(crate) fn get_logic_operator(token: TokenType) -> LogicOperator {
    match token {
        TokenType::AND => LogicOperator::And,
        TokenType::OR => LogicOperator::Or,

        _ => unreachable!(),
    }
}

#[derive(Debug, PartialOrd, Clone, PartialEq, Hash)]
pub enum UnaryOperator {
    Bang,
    Minus,
    Plus,
}

#[derive(Debug, PartialOrd, Clone, PartialEq, Hash)]
pub enum LogicOperator {
    Or,
    And,
}
