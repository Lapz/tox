use object::Object;
use ast::expr::*;
use ast::statement::Statement;
use pos::WithPos;

#[derive(Debug)]
pub enum RuntimeError {
    Unary(&'static str),
    Binary(&'static str),
}



    pub fn interpret(statement: &WithPos<Statement>) -> Result<Object, RuntimeError> {
            evaluate_statement(statement)
    }

    fn evaluate_statement(statement:&WithPos<Statement>) -> Result<Object,RuntimeError> {
        match statement.node {
            Statement::ExpressionStmt(ref expr) => evalute_expression(expr),
            _ => unimplemented!(),
        }
    }
   


    fn evalute_expression(expression: &WithPos<Expression>) -> Result<Object, RuntimeError> {
        match expression.node {
            Expression::Grouping { ref expr } => evalute_expression(expr),
            Expression::Literal(ref lit) => evaluate_literal(lit),

            Expression::Binary {
                ref left_expr,
                ref operator,
                ref right_expr,
            } => {
                let left = evalute_expression(left_expr)?;
                let right = evalute_expression(right_expr)?;

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
                }
            }

            Expression::Unary {
                ref operator,
                ref expr,
            } => {
                let right = evalute_expression(expr)?;

                match *operator {
                    UnaryOperator::Minus => {
                        match right {
                            Object::Float(f) => Ok(Object::Float(-f)),
                            Object::Int(i) => Ok(Object::Int(-i)),
                            _ => unreachable!(),
                        }
                    }
                    UnaryOperator::Bang => Ok(!right),
                }
            }
            _ => unimplemented!(),
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
         _ => unreachable!() 
    }
}

fn times(lhs: Object, rhs: Object) -> Result<Object, RuntimeError> {
    match (lhs, rhs) {
        (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l * r)),
        (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l * r)),
         _ => unreachable!() 
    }
}


#[inline]
fn modulo(lhs: Object, rhs: Object) -> Result<Object, RuntimeError> {
    match (lhs, rhs) {
        (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l % r)),
        (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l % r)),
         _ => unreachable!()
    }
}

#[inline]
fn expon(lhs: Object, rhs: Object) -> Result<Object, RuntimeError> {
    match (lhs, rhs) {
        (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l.powf(r))),
        (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l.pow(r as u32))),
        _ => unreachable!()
    }
}


fn minus(lhs: Object, rhs: Object) -> Result<Object, RuntimeError> {
    match (lhs, rhs) {
        (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l - r)),
        (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l - r)),
        _ => unreachable!() 
    }
}

fn divide(lhs: Object, rhs: Object) -> Result<Object, RuntimeError> {
    match (lhs, rhs) {
        (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l / r)),
        (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l / r)),
        _ => unreachable!() 
    }
}

 fn evaluate_literal(expression: &Literal) -> Result<Object, RuntimeError> {
        match *expression {
            Literal::Float(i) => Ok(Object::Float(i)),
            Literal::Int(i) => Ok(Object::Int(i)),
            Literal::Str(ref s) => Ok(Object::Str(s.to_owned())),
            Literal::Nil => Ok(Object::Nil),
            Literal::True(ref b) | Literal::False(ref b) => Ok(Object::Bool(*b)),
        }
    }