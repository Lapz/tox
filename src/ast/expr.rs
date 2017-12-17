use ast::statement::Statement;
use pos::WithPos;
use symbol::Symbol;

#[derive(Debug, PartialOrd, Clone, PartialEq)]
pub enum Expression {
    // The different type of expressions availabe
    Array {
        items: Vec<WithPos<Expression>>,
    },
    Assign {
        handle: VariableUseHandle,
        name: Symbol,
        kind: AssignOperator,
        value: Box<WithPos<Expression>>,
    },

    Binary {
        left_expr: Box<WithPos<Expression>>,
        operator: Operator,
        right_expr: Box<WithPos<Expression>>,
    },
    Call {
        callee: Box<WithPos<Expression>>,
        arguments: Vec<WithPos<Expression>>,
    },
    Dict {
        items: Vec<(WithPos<Expression>, WithPos<Expression>)>,
    },
    Func {
        parameters: Vec<(Symbol, Option<Symbol>)>,
        body: Box<WithPos<Statement>>,
        returns: Option<Symbol>,
    },
    Get {
        object: Box<WithPos<Expression>>,
        name: Symbol,
        handle: VariableUseHandle,
    },
    Grouping {
        expr: Box<WithPos<Expression>>,
    },

    IndexExpr {
        target: Box<WithPos<Expression>>,
        index: Box<WithPos<Expression>>,
    },

    Literal(Literal),
    Logical {
        left: Box<WithPos<Expression>>,
        operator: LogicOperator,
        right: Box<WithPos<Expression>>,
    },

    Set {
        object: Box<WithPos<Expression>>,
        handle: VariableUseHandle,
        name: Symbol,
        value: Box<WithPos<Expression>>,
    },

    Ternary {
        condition: Box<WithPos<Expression>>,
        then_branch: Box<WithPos<Expression>>,
        else_branch: Box<WithPos<Expression>>,
    },
    Unary {
        operator: UnaryOperator,
        expr: Box<WithPos<Expression>>,
    },

    This(VariableUseHandle),
    Var(Symbol, VariableUseHandle),
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
    SlashEqual,
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
