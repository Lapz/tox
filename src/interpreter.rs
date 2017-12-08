use object::Object;
use ast::expr::*;

pub struct Interpreter;
#[derive(Debug)]
pub enum RuntimeError {
    Unary(&'static str),
    Binary(&'static str),
}


impl Interpreter {
    pub fn new() -> Self {
        Interpreter
    }
    pub fn interpret(&self, expr: &Expression) -> Result<Object, RuntimeError> {
        self.evalute_expression(expr)
    }
    fn evaluate_literal(&self, expression: &Literal) -> Result<Object, RuntimeError> {
        match *expression {
            Literal::Float(i) => Ok(Object::Float(i)),
            Literal::Int(i) => Ok(Object::Int(i)),
            Literal::Str(ref s) => Ok(Object::Str(s.to_owned())),
            Literal::Nil => Ok(Object::Nil),
            Literal::True(ref b) | Literal::False(ref b) => Ok(Object::Bool(*b)),
        }
    }


    fn evalute_expression(&self, expression: &Expression) -> Result<Object, RuntimeError> {
        match *expression {
            Expression::Grouping { ref expr } => self.evalute_expression(expr),
            Expression::Literal(ref lit) => self.evaluate_literal(lit),

            Expression::Binary {
                ref left_expr,
                ref operator,
                ref right_expr,
            } => {
                let left = self.evalute_expression(left_expr)?;
                let right = self.evalute_expression(right_expr)?;

                match *operator {
                    Operator::BangEqual => Ok(Object::Bool(!left == right)),
                    Operator::EqualEqual => Ok(Object::Bool(left == right)),
                    Operator::LessThan => Ok(Object::Bool(left < right)),
                    Operator::LessThanEqual => Ok(Object::Bool(left <= right)),
                    Operator::GreaterThan => Ok(Object::Bool(left > right)),
                    Operator::GreaterThanEqual => Ok(Object::Bool(left >= right)),
                    Operator::Plus => add(left, right),
                    Operator::Minus => minus(left, right),
                    Operator::Star => times(left, right),
                    Operator::Slash => divide(left, right),
                    Operator::Modulo => modulo(left, right),
                    Operator::Exponential => expon(left, right),

                    _ => unimplemented!(),
                }
            }

            Expression::Unary {
                ref operator,
                ref expr,
            } => {
                let right = self.evalute_expression(expr)?;

                match *operator {
                    UnaryOperator::Minus => {
                        match right {
                            Object::Float(f) => Ok(Object::Float(-f)),
                            Object::Int(i) => Ok(Object::Int(-i)),
                            _ => Err(RuntimeError::Unary(
                                "Cant use the unary operator \'-\' on values other than floats or ints",
                            )),

                        }
                    }
                    UnaryOperator::Bang => Ok(!right),
                    UnaryOperator::Plus => {
                        match right {
                            Object::Float(f) => Ok(Object::Float(1.0 * f)),
                            Object::Int(i) => Ok(Object::Int(1 * i)),
                            _ => Err(RuntimeError::Unary(
                                "Cant use the unary operator \'+ \' on values other than floats or ints",
                            )),

                        }
                    }
                }
            }
            _ => unimplemented!(),
        }
    }
}


fn add(lhs: Object, rhs: Object) -> Result<Object, RuntimeError> {
    match (lhs, rhs) {
        (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l + r)),
        (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l + r)),
        (Object::Str(ref mut l), Object::Str(ref r)) => {
            l.push_str(r);

            Ok(Object::Str(l.to_owned()))
        }

        _ => Err(RuntimeError::Binary(
            "Addition is not implemented for these types",
        )), // Format message wiuth the offending types
    }
}

fn times(lhs: Object, rhs: Object) -> Result<Object, RuntimeError> {
    match (lhs, rhs) {
        (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l * r)),
        (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l * r)),
        _ => Err(RuntimeError::Binary(
            "Multiplication is not implemented for these types",
        )), // Format message wiuth the offending types
    }
}

fn modulo(lhs: Object, rhs: Object) -> Result<Object, RuntimeError> {
    match (lhs, rhs) {
        (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l % r)),
        (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l % r)),
        _ => Err(RuntimeError::Binary(
            "Modulo is not implemented for these types",
        )), // Format message wiuth the offending types
    }
}

fn expon(lhs: Object, rhs: Object) -> Result<Object, RuntimeError> {
    match (lhs, rhs) {
        (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l.powf(r))),
        (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l.pow(r as u32))),
        _ => Err(RuntimeError::Binary(
            "Exponential is not implemented for these types",
        )), // Format message wiuth the offending types
    }
}


fn minus(lhs: Object, rhs: Object) -> Result<Object, RuntimeError> {
    match (lhs, rhs) {
        (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l - r)),
        (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l - r)),
        _ => Err(RuntimeError::Binary(
            "Subtraction is not implemented for these types",
        )), // Format message wiuth the offending types
    }
}

fn divide(lhs: Object, rhs: Object) -> Result<Object, RuntimeError> {
    match (lhs, rhs) {
        (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l / r)),
        (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l / r)),
        _ => Err(RuntimeError::Binary(
            "Division is not implemented for these types",
        )), // Format message wiuth the offending types
    }
}
