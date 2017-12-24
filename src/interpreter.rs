use object::Object;
use ast::expr::*;
use ast::statement::Statement;
use pos::WithPos;
use env::Env;

#[derive(Debug)]
pub enum RuntimeError {
    Unary(&'static str),
    Binary(&'static str),
}



    pub fn interpret(statements: &[WithPos<Statement>],env:&mut Env) -> Result<Object, RuntimeError> {
        let mut result = Object::None;
                    for statement in statements {
                        result = evaluate_statement(statement,env)?
                    }
        Ok(result)       
    }

    fn evaluate_statement(statement:&WithPos<Statement>,env:&mut Env) -> Result<Object,RuntimeError> {
        match statement.node {
            Statement::ExpressionStmt(ref expr) => evaluate_expression(expr,env),
            Statement::Var(ref symbol,ref expression,..) => {
                let value = evaluate_expression(expression,env)?;

                env.add_object(*symbol,value);
                Ok(Object::None)
            }
            Statement::Block(ref statements) => {
                env.begin_scope();
                for statement in statements {
                     evaluate_statement(statement,env)?;
                    }
                env.end_scope();
                    Ok(Object::None)
            }
            _ => unimplemented!(),
        }
    }
   


    fn evaluate_expression(expression: &WithPos<Expression>,env:&mut Env) -> Result<Object, RuntimeError> {
        match expression.node {
            Expression::Array {ref items}  => {
                let mut values = vec![];
                 for item in items {
            values.push(evaluate_expression(item, env)?)
            }
            Ok(Object::Array(values))
            }

            Expression::Assign{ref name,ref kind,ref value,..} => {
                let mut value = evaluate_expression(value,env)?;

                match *kind {
                    AssignOperator::Equal => (),

                    AssignOperator::PlusEqual => {
                    let current = env.look_object(*name).unwrap();

                    match (current,value) {
                        (&Object::Int(x),Object::Int(y)) => value = Object::Int(x+y),
                        (&Object::Float(x),Object::Float(y)) => value = Object::Float(x+y),
                        _ => unreachable!(),
                    }
            },

            AssignOperator::MinusEqual => {
                let current = env.look_object(*name).unwrap();

                match (current,value) {
                        (&Object::Int(x),Object::Int(y)) => value = Object::Int(x-y),
                        (&Object::Float(x),Object::Float(y)) => value = Object::Float(x-y),
                        _ => unreachable!(),
                    }
            },

            AssignOperator::SlashEqual => {
                let current = env.look_object(*name).unwrap();

                match (current,value) {
                        (&Object::Int(x),Object::Int(y)) => value = Object::Int(x/y),
                        (&Object::Float(x),Object::Float(y)) => value = Object::Float(x/y),
                        _ => unreachable!(),
                }
            },

            AssignOperator::StarEqual => {
                let current = env.look_object(*name).unwrap();

                match (current,value) {
                        (&Object::Int(x),Object::Int(y)) => value = Object::Int(x*y),
                        (&Object::Float(x),Object::Float(y)) => value = Object::Float(x*y),
                        _ => unreachable!(),
                    }
                }
            }

            env.assign_object(*name,value.clone());

            Ok(value)

            }
            Expression::Grouping { ref expr } => evaluate_expression(expr,env),
            Expression::Literal(ref lit) => evaluate_literal(lit),
            Expression::Var(ref symbol,..) => {
                let value = env.look_object(*symbol).unwrap().clone();
                Ok(value)
            }

            Expression::Binary {
                ref left_expr,
                ref operator,
                ref right_expr,
            } => {
                let left = evaluate_expression(left_expr,env)?;
                let right = evaluate_expression(right_expr,env)?;

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
                let right = evaluate_expression(expr,env)?;

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