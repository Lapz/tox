use object::Object;
use ast::expr::*;
use ast::statement::Statement;
use pos::WithPos;
use env::Env;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Debug)]
pub enum RuntimeError {
    Break,
    Continue,
    IndexOutOfBound,
    InvalidIndexType,
    NotAnIn,
    UndefinedProperty,
    CantParseAsInt,
}

pub fn interpret(statements: &[WithPos<Statement>], env: &mut Env) -> Result<Object, RuntimeError> {
    let mut result = Object::None;
    for statement in statements {
        result = evaluate_statement(statement, env)?
    }
    Ok(result)
}

pub(crate) fn evaluate_statement(
    statement: &WithPos<Statement>,
    env: &mut Env,
) -> Result<Object, RuntimeError> {
    match statement.node {
        Statement::Block(ref statements) => {
            env.begin_scope();
            for statement in statements {
                evaluate_statement(statement, env)?;
            }
            env.end_scope();
            Ok(Object::None)
        }

        Statement::Break => Err(RuntimeError::Break),
        Statement::Continue => Err(RuntimeError::Continue),
        Statement::Class {
            ref name,
            ref methods,
            ref superclass,
            ..
        } => {
            env.add_object(*name, Object::Nil);

            use symbol::Symbol;

            let mut sym_methods: HashMap<Symbol, Object> = HashMap::new();

            let mut sklass = None;
            let mut s = false;

            if let Some(sclass) = *superclass {
                env.begin_scope();

                let sk = env.look_object(sclass).unwrap().clone();

                env.add_object(Symbol(1), sk);
                sklass = Some(Box::new(env.look_object(sclass).unwrap().clone()));
                s = true;
            }

            for method in methods {
                match method.node {
                    Statement::Function { ref name, ref body } => match body.node {
                        Expression::Func {
                            ref parameters,
                            ref body,
                            ..
                        } => {
                            use symbol::Symbol;

                            let mut params: Vec<Symbol> =
                                parameters.iter().map(|params| params.0).collect();
                            sym_methods
                                .insert(*name, Object::Function(*name, params, body.node.clone()));
                        }
                        _ => unreachable!(),
                    },

                    _ => unimplemented!(),
                }
            }

            if s {
                env.end_scope();
            }

            env.assign_object(*name, Object::Class(*name, sklass, sym_methods));
            Ok(Object::None)
        }

        Statement::DoStmt {
            ref condition,
            ref body,
        } => {
            while evaluate_expression(condition, env)?.is_truthy() {
                match evaluate_statement(body, env) {
                    Ok(value) => value,
                    Err(e) => match e {
                        RuntimeError::Break => break,
                        RuntimeError::Continue => continue,
                        _ => return Err(e),
                    },
                };
            }

            Ok(Object::None)
        }

        Statement::ExpressionStmt(ref expr) => evaluate_expression(expr, env),

        Statement::ForStmt {
            ref initializer,
            ref condition,
            ref increment,
            ref body,
        } => {
            if initializer.is_none() && condition.is_none() && increment.is_none() {
                loop {
                    match evaluate_statement(body, env) {
                        Ok(value) => value,
                        Err(e) => match e {
                            RuntimeError::Break => break,
                            RuntimeError::Continue => continue,
                            _ => return Err(e),
                        },
                    };
                }
                return Ok(Object::None);
            }

            if let &Some(ref init) = initializer {
                evaluate_statement(init, env)?;
            }

            if let &Some(ref cond) = condition {
                while evaluate_expression(cond, env)?.is_truthy() {
                    match evaluate_statement(body, env) {
                        Ok(value) => value,
                        Err(e) => match e {
                            RuntimeError::Break => break,
                            RuntimeError::Continue => continue,
                            _ => return Err(e),
                        },
                    };

                    if let &Some(ref inc) = increment {
                        evaluate_expression(inc, env)?;
                    }
                }
            }

            Ok(Object::None)
        }

        Statement::Function { ref name, ref body } => match body.node {
            Expression::Func {
                ref parameters,
                ref body,
                ..
            } => {
                use symbol::Symbol;

                let mut params: Vec<Symbol> = parameters.iter().map(|params| params.0).collect();

                env.add_object(*name, Object::Function(*name, params, body.node.clone()));
                return Ok(Object::None);
            }
            _ => unreachable!(),
        },

        Statement::IfStmt {
            ref condition,
            ref else_branch,
            ref then_branch,
        } => {
            if evaluate_expression(condition, env)?.is_truthy() {
                evaluate_statement(then_branch, env)
            } else if let Some(ref else_statement) = *else_branch {
                evaluate_statement(else_statement, env)
            } else {
                Ok(Object::None)
            }
        }

        Statement::Print(ref expr) => {
            use std::io;
            use std::io::prelude::*;

            let value = evaluate_expression(expr, env)?;

            println!("{}", value.as_string());
            let _ = io::stdout().flush();

            Ok(Object::None)
        }

        Statement::Return(ref r) => {
            if let Some(ref expr) = *r {
                return Ok(Object::Return(Box::new(evaluate_expression(expr, env)?)));
            }

            Ok(Object::Return(Box::new(Object::Nil)))
        }

        Statement::TypeAlias { .. } => Ok(Object::None),

        Statement::WhileStmt {
            ref body,
            ref condition,
        } => {
            while evaluate_expression(condition, env)?.is_truthy() {
                match evaluate_statement(body, env) {
                    Ok(value) => value,
                    Err(e) => match e {
                        RuntimeError::Break => break,
                        RuntimeError::Continue => continue,
                        _ => return Err(e),
                    },
                };
            }

            Ok(Object::None)
        }

        Statement::Var(ref symbol, ref expression, ..) => {
            if let &Some(ref expr) = expression {
                let value = evaluate_expression(expr, env)?;
                env.add_object(*symbol, value);
            }

            Ok(Object::None)
        }
    }
}

fn evaluate_expression(
    expression: &WithPos<Expression>,
    env: &mut Env,
) -> Result<Object, RuntimeError> {
    match expression.node {
        Expression::Array { ref items } => {
            let mut values = vec![];
            for item in items {
                values.push(evaluate_expression(item, env)?)
            }
            Ok(Object::Array(values))
        }

        Expression::This(_) => {
            use symbol::Symbol;
            let value = env.look_object(Symbol(0)).unwrap().clone();
            Ok(value)
        }

        Expression::Assign {
            ref name,
            ref kind,
            ref value,
            ..
        } => {
            let mut value = evaluate_expression(value, env)?;

            match *kind {
                AssignOperator::Equal => (),

                AssignOperator::PlusEqual => {
                    let current = env.look_object(*name).unwrap();

                    match (current, value) {
                        (&Object::Int(x), Object::Int(y)) => value = Object::Int(x + y),
                        (&Object::Float(x), Object::Float(y)) => value = Object::Float(x + y),
                        _ => unreachable!(),
                    }
                }

                AssignOperator::MinusEqual => {
                    let current = env.look_object(*name).unwrap();

                    match (current, value) {
                        (&Object::Int(x), Object::Int(y)) => value = Object::Int(x - y),
                        (&Object::Float(x), Object::Float(y)) => value = Object::Float(x - y),
                        _ => unreachable!(),
                    }
                }

                AssignOperator::SlashEqual => {
                    let current = env.look_object(*name).unwrap();

                    match (current, value) {
                        (&Object::Int(x), Object::Int(y)) => value = Object::Int(x / y),
                        (&Object::Float(x), Object::Float(y)) => value = Object::Float(x / y),
                        _ => unreachable!(),
                    }
                }

                AssignOperator::StarEqual => {
                    let current = env.look_object(*name).unwrap();

                    match (current, value) {
                        (&Object::Int(x), Object::Int(y)) => value = Object::Int(x * y),
                        (&Object::Float(x), Object::Float(y)) => value = Object::Float(x * y),
                        _ => unreachable!(),
                    }
                }
            }

            env.assign_object(*name, value.clone());

            Ok(value)
        }

        Expression::Binary {
            ref left_expr,
            ref operator,
            ref right_expr,
        } => {
            let left = evaluate_expression(left_expr, env)?;
            let right = evaluate_expression(right_expr, env)?;

            match *operator {
                Operator::BangEqual => Ok(Object::Bool(left != right)),
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

        Expression::Call {
            ref callee,
            ref arguments,
        } => {
            let callee = evaluate_expression(callee, env)?;

            let mut obj_arguments = vec![];

            for expr in arguments {
                obj_arguments.push(evaluate_expression(expr, env)?);
            }

            callee.call(&obj_arguments, env)
        }

        Expression::ClassInstance {
            ref name,
            ref properties,
        } => {
            use symbol::Symbol;

            match env.look_object(*name).unwrap().clone() {
                Object::Class(_, ref superclass, ref methods) => {
                    let mut props: HashMap<Symbol, Object> = HashMap::new();
                    let mut s_class_methods = None;
                    if let &Some(ref sklass) = superclass {
                        match **sklass {
                            Object::Class(_, _, ref methods_) => {
                                s_class_methods = Some(methods_.clone());
                            }
                            _ => unimplemented!(),
                        }
                    }

                    for &(ref name, ref expr) in properties {
                        let value = evaluate_expression(expr, env)?;
                        props.insert(*name, value);
                    }

                    env.add_object(
                        *name,
                        Object::Instance {
                            methods: methods.clone(),
                            fields: Rc::new(RefCell::new(props.clone())),
                            sclassmethods: s_class_methods.clone(),
                        },
                    );

                    return Ok(Object::Instance {
                        methods: methods.clone(),
                        fields: Rc::new(RefCell::new(props)),
                        sclassmethods: s_class_methods,
                    });
                }

                _ => unreachable!(),
            };
        }

        Expression::Dict { ref items } => {
            let mut dict: HashMap<Object, Object> = HashMap::new();

            for &(ref key, ref value) in items {
                let eval_key = evaluate_expression(key, env)?;
                let eval_value = evaluate_expression(value, env)?;

                dict.insert(eval_key, eval_value);
            }

            Ok(Object::Dict(dict))
        }

        Expression::Grouping { ref expr } => evaluate_expression(expr, env),

        Expression::IndexExpr {
            ref target,
            ref index,
        } => {
            let target = evaluate_expression(target, env)?;
            let index = evaluate_expression(index, env)?;

            match target {
                Object::Array(r) => {
                    let index = match index {
                        Object::Int(i) => i,
                        _ => unreachable!(),
                    };

                    if index > (r.len() as i64) || index < 0 {
                        return Err(RuntimeError::IndexOutOfBound);
                    }

                    Ok(r[index as usize].to_owned())
                }
                Object::Dict(r) => {
                    let index = match index {
                        Object::Int(i) => Object::Int(i),
                        Object::Str(r) => Object::Str(r),
                        Object::Bool(b) => Object::Bool(b),
                        _ => return Err(RuntimeError::InvalidIndexType),
                    };

                    let nil = Object::Nil;

                    Ok(r.get(&index).unwrap_or(&nil).clone())
                }
                _ => unimplemented!(),
            }
        }
        Expression::Literal(ref lit) => evaluate_literal(lit),

        Expression::Logical {
            ref left,
            ref operator,
            ref right,
        } => {
            let left = evaluate_expression(left, env)?;

            match *operator {
                LogicOperator::Or => if left.is_truthy() {
                    return Ok(left);
                },
                LogicOperator::And => if !left.is_truthy() {
                    return Ok(left);
                },
            }

            let right = evaluate_expression(right, env)?;

            Ok(right)
        }

        Expression::Func {
            ref parameters,
            ref body,
            ..
        } => {
            use symbol::Symbol;

            let mut params: Vec<Symbol> = parameters.iter().map(|params| params.0).collect();
            Ok(Object::Function(env.unique_id(), params, body.node.clone()))
        }
        Expression::Var(ref symbol, ..) => {
            let value = env.look_object(*symbol).unwrap().clone();
            Ok(value)
        }

        Expression::Set {
            ref object,
            ref name,
            ref value,
            ..
        } => {
            let object = evaluate_expression(object, env)?;
            let value = evaluate_expression(value, env)?;

            match object {
                mut instance @ Object::Instance { .. } => {
                    instance.set(*name, &value);
                }
                _ => return Err(RuntimeError::NotAnIn),
            }

            Ok(value)
        }
        Expression::Super(_) => {
            use symbol::Symbol;
            let value = env.look_object(Symbol(1)).unwrap().clone();
            Ok(value)
        }

        Expression::Get {
            ref object,
            ref property,
            ..
        } => {
            let object = evaluate_expression(object, env)?;

            match object {
                instance @ Object::Instance { .. } => instance.get_property(property, env),
                class @ Object::Class(_, _, _) => class.get_property(property, env),
                _ => {
                    return Err(RuntimeError::NotAnIn);
                }
            }
        }
        Expression::Ternary {
            ref condition,
            ref then_branch,
            ref else_branch,
        } => {
            let condition = evaluate_expression(condition, env)?;

            if condition.is_truthy() {
                evaluate_expression(then_branch, env)
            } else {
                evaluate_expression(else_branch, env)
            }
        }

        Expression::Unary {
            ref operator,
            ref expr,
        } => {
            let right = evaluate_expression(expr, env)?;

            match *operator {
                UnaryOperator::Minus => match right {
                    Object::Float(f) => Ok(Object::Float(-f)),
                    Object::Int(i) => Ok(Object::Int(-i)),
                    _ => unreachable!(),
                },
                UnaryOperator::Bang => Ok(!right),
            }
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
        _ => unreachable!(),
    }
}

fn times(lhs: Object, rhs: Object) -> Result<Object, RuntimeError> {
    match (lhs, rhs) {
        (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l * r)),
        (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l * r)),
        _ => unreachable!(),
    }
}

#[inline]
fn modulo(lhs: Object, rhs: Object) -> Result<Object, RuntimeError> {
    match (lhs, rhs) {
        (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l % r)),
        (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l % r)),
        _ => unreachable!(),
    }
}

#[inline]
fn expon(lhs: Object, rhs: Object) -> Result<Object, RuntimeError> {
    match (lhs, rhs) {
        (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l.powf(r))),
        (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l.pow(r as u32))),
        _ => unreachable!(),
    }
}

fn minus(lhs: Object, rhs: Object) -> Result<Object, RuntimeError> {
    match (lhs, rhs) {
        (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l - r)),
        (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l - r)),
        _ => unreachable!(),
    }
}

fn divide(lhs: Object, rhs: Object) -> Result<Object, RuntimeError> {
    match (lhs, rhs) {
        (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l / r)),
        (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l / r)),
        _ => unreachable!(),
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
