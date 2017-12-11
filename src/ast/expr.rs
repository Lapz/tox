use ast::statement::Statement;
use pos::WithPos;
use symbol::Symbol;

use types;
#[derive(Debug, PartialOrd, Clone, PartialEq)]
pub enum Expression {
    // The different type of expressions availabe
    IndexExpr {
        target: Box<Expression>,
        index: Box<Expression>,
    },
    Array {
        items: Vec<Expression>,
    },
    Assign {
        handle: VariableUseHandle,
        name: Symbol,
        kind: AssignOperator,
        value: Box<Expression>,
    },

    Binary {
        left_expr: Box<Expression>,
        operator: Operator,
        right_expr: Box<Expression>,
    },
    Call {
        callee: Box<Expression>,
        arguments: Vec<Expression>,
    },
    Dict {
        items: Vec<(Expression, Expression)>,
    },
    Func {
        parameters: Vec<Symbol>,
        body: Box<WithPos<Statement>>,
    },
    Get {
        object: Box<Expression>,
        name: Symbol,
        handle: VariableUseHandle,
    },
    Grouping {
        expr: Box<Expression>,
    },
    Literal(Literal),
    Logical {
        left: Box<Expression>,
        operator: LogicOperator,
        right: Box<Expression>,
    },

    Set {
        object: Box<Expression>,
        handle: VariableUseHandle,
        name: Symbol,
        value: Box<Expression>,
    },

    Ternary {
        condition: Box<Expression>,
        then_branch: Box<Expression>,
        else_branch: Box<Expression>,
    },
    Unary {
        operator: UnaryOperator,
        expr: Box<Expression>,
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
        TokenType::PLUS => UnaryOperator::Plus,
        _ => unreachable!(),
    }
}

#[inline]
pub(crate) fn get_type(token: TokenType) -> Option<types::Type> {
    use types::Type;

    match token {
        TokenType::TINT => Some(Type::Int),
        TokenType::TFLOAT =>  Some(Type::Float),
        TokenType::TSTR=>  Some(Type::Str),
        TokenType::NIL =>  Some(Type::Nil),
        TokenType::TBOOL =>  Some(Type::Bool),
        _ => None
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

