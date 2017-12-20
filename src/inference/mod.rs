mod test;
use ast::expr::*;
use ast::statement::Statement;
use types::{Type, TypeError};
use env::{Entry, Env};
use pos::{Postition, WithPos};
use symbol::Symbol;

type Exp = ();

#[derive(Debug, PartialEq)]
pub struct ExpressionType {
    pub exp: Exp,
    pub ty: Type,
}

pub fn analyse(
    statements: &[WithPos<Statement>],
    env: &mut Env,
) -> Result<Vec<ExpressionType>, Vec<TypeError>> {
    let mut tys = vec![];
    let mut errors = vec![];

    for statement in statements {
        match transform_statement(statement, env) {
            Ok(ty) => tys.push(ty),
            Err(e) => errors.push(e),
        }
    }

    if errors.is_empty() {
        Ok(tys)
    } else {
        Err(errors)
    }
}

/// Checks if two types are eqvilant
fn check_types(expected: &Type, unknown: &Type) -> Result<(), TypeError> {
    if expected != unknown {
        return Err(TypeError::NotSame);
    }
    Ok(())
}

fn get_actual_ty(entry: &Entry) -> Result<Type, TypeError> {
    match *entry {
        Entry::VarEntry(ref ty) => Ok(ty.clone()),
        _ => Err(TypeError::Function),
    }
}

fn transform_var(symbol: &Symbol, env: &mut Env) -> Result<ExpressionType, TypeError> {
    println!("{:?}", symbol);
    match env.look_var(*symbol) {
        Some(ty) => Ok(ExpressionType {
            exp: (),
            ty: get_actual_ty(ty)?,
        }),
        None => Err(TypeError::UndefindedVar),
    }
}

fn transform_statement(
    statement: &WithPos<Statement>,
    env: &mut Env,
) -> Result<ExpressionType, TypeError> {
    match statement.node {
        Statement::ExpressionStmt(ref expr) => transform_expr(expr, env),
        Statement::Class {
            ref name,
            ref methods,
        } => {
            for method in methods {
                println!("{:?}", transform_statement(method, env)?)
            }

            unimplemented!()
        }
        Statement::Var(ref symbol, ref expr, ref ty) => {
            let exp_ty = transform_expr(expr, env)?;

            if let Some(ref ident) = *ty {
                let ty = get_type(ident, env)?;
                check_types(&ty, &exp_ty.ty)?;

                env.add_var(*symbol, Entry::VarEntry(ty.clone()));

                return Ok(ExpressionType { exp: (), ty });
            }

            env.add_var(*symbol, Entry::VarEntry(exp_ty.ty.clone()));

            Ok(exp_ty)
        }

        Statement::Break | Statement::Continue => Ok(ExpressionType {
            exp: (),
            ty: Type::Nil,
        }),

        Statement::Block(ref expressions) => {
            if expressions.is_empty() {
                return Ok(ExpressionType {
                    exp: (),
                    ty: Type::Nil,
                });
            }

            for expr in expressions.iter().rev().skip(1) {
                transform_statement(expr, env)?;
            }

            transform_statement(expressions.last().unwrap(), env)
        }

        Statement::IfStmt {
            ref condition,
            ref then_branch,
            ref else_branch,
        } => {
            let condition_ty = transform_expr(condition, env)?;

            check_bool(&condition_ty, statement.pos)?;

            let then_ty = transform_statement(then_branch, env)?;

            if let &Some(ref else_statement) = else_branch {
                let else_ty = transform_statement(else_statement, env)?;

                check_types(&then_ty.ty, &else_ty.ty)?;

                return Ok(ExpressionType {
                    exp: (),
                    ty: then_ty.ty,
                });
            }

            Ok(then_ty)
        }

        Statement::WhileStmt {
            ref condition,
            ref body,
        }
        | Statement::DoStmt {
            ref condition,
            ref body,
        } => {
            let condition_ty = transform_expr(condition, env)?;

            check_bool(&condition_ty, statement.pos)?;

            let body_ty = transform_statement(body, env)?;

            Ok(body_ty)
        }

        Statement::Return(ref returns) => {
            if let &Some(ref expr) = returns {
                let exp_ty = transform_expr(expr, env)?;
                return Ok(exp_ty);
            }
            Ok(ExpressionType {
                exp: (),
                ty: Type::Nil,
            })
        }

        Statement::Function { ref name, ref body } => {
            match body.node {
                Expression::Func {
                    ref returns,
                    ref parameters,
                    ..
                } => {
                    let return_type = if let Some(ref return_ty) = *returns {
                        get_type(return_ty, env)?
                    } else {
                        Type::Nil
                    };

                    let mut param_names = vec![];
                    let mut param_ty = vec![];

                    for &(param, p_ty) in parameters {
                        param_ty.push(get_type(&p_ty, env)?);
                        param_names.push(param);
                    }

                    env.add_var(
                        *name,
                        Entry::FunEntry {
                            params: param_ty,
                            returns: return_type.clone(),
                        },
                    );
                }
                _ => unreachable!(),
            };

            let body_ty = transform_expr(body, env)?;

            Ok(body_ty)
        }
    }
}

fn get_type(ident: &Symbol, env: &mut Env) -> Result<Type, TypeError> {
    if let Some(ty) = env.look_type(*ident) {
        return Ok(ty.clone());
    }

    Err(TypeError::Undefinded)
}

fn transform_expr(expr: &WithPos<Expression>, env: &mut Env) -> Result<ExpressionType, TypeError> {
    match expr.node {
        Expression::Array { ref items } => {
            if items.is_empty() {
                return Ok(ExpressionType {
                    exp: (),
                    ty: Type::Array(Box::new(Type::Nil)),
                });
            }

            let first_ty = transform_expr(&items[0], env)?;

            for item in items {
                match check_types(&first_ty.ty, &transform_expr(item, env)?.ty) {
                    Ok(_) => (),
                    Err(e) => return Err(e),
                }
            }

            Ok(ExpressionType {
                exp: (),
                ty: Type::Array(Box::new(first_ty.ty)),
            })
        }

        Expression::Assign {
            ref value,
            ref kind,
            ..
        } => {
            use ast::expr::AssignOperator::*;
            match *kind {
                Equal => transform_expr(value, env),
                MinusEqual => s_check_int_float(&transform_expr(value, env)?, value.pos),
                PlusEqual => s_check_int_float(&transform_expr(value, env)?, value.pos),
                StarEqual => s_check_int_float(&transform_expr(value, env)?, value.pos),
                SlashEqual => s_check_int_float(&transform_expr(value, env)?, value.pos),
            }
        }

        Expression::Binary {
            ref left_expr,
            ref right_expr,
            ..
        } => {
            let left = transform_expr(left_expr, env)?;
            let right = transform_expr(right_expr, env)?;
            check_int_float(&left, &right, left_expr.pos)
        }

        Expression::Call {
            ref callee,
            ref arguments,
        } => {
            let callee = match callee.node {
                Expression::Var(sym, _) => sym,
                _ => unreachable!(),
            };

            if let Some(entry) = env.look_var(callee).cloned() {
                match entry {
                    Entry::FunEntry {
                        ref params,
                        ref returns,
                    } => for (arg, param) in arguments.iter().zip(params) {
                        let exp = transform_expr(arg, &mut env.clone())?;

                        check_types(&param, &exp.ty)?;

                        return Ok(ExpressionType {
                            exp: (),
                            ty: returns.clone(),
                        });
                    },

                    _ => unreachable!(), // TODO Add classes
                }
            }

            Err(TypeError::Undefinded)
        }

        Expression::Dict { ref items } => {
            if items.is_empty() {
                return Ok(ExpressionType {
                    exp: (),
                    ty: Type::Dict(Box::new(Type::Nil), Box::new(Type::Nil)),
                });
            }

            let first_key_ty = transform_expr(&items[0].0, env)?;
            let first_value_ty = transform_expr(&items[0].0, env)?;
            for item in items {
                match check_types(&first_key_ty.ty, &transform_expr(&item.0, env)?.ty) {
                    Ok(_) => (),
                    Err(e) => return Err(e),
                };

                match check_types(&first_value_ty.ty, &transform_expr(&item.1, env)?.ty) {
                    Ok(_) => (),
                    Err(e) => return Err(e),
                }
            }

            Ok(ExpressionType {
                exp: (),
                ty: Type::Dict(Box::new(first_key_ty.ty), Box::new(first_value_ty.ty)),
            })
        }

        Expression::Func {
            ref body,
            ref returns,
            ref parameters,
        } => {
            let return_type = if let Some(ref return_ty) = *returns {
                get_type(return_ty, env)?
            } else {
                Type::Nil
            };

            let mut params_ty = vec![];
            let mut param_names = vec![];

            for &(symbol, p_ty) in parameters {
                params_ty.push(get_type(&p_ty, env)?);
                param_names.push(symbol);
            }

            env.begin_scope();

            for (name, ty) in param_names.iter().zip(params_ty) {
                env.add_var(*name, Entry::VarEntry(ty));
            }

            let body_ty = transform_statement(&body, env)?;

            check_types(&return_type, &body_ty.ty)?;

            env.end_scope();
            Ok(body_ty)
        }

        Expression::Grouping { ref expr } => transform_expr(expr, env),

        Expression::IndexExpr {
            ref target,
            ref index,
        } => match target.node {
            Expression::Var(ref symbol, _) => {
                let target_ty = transform_var(symbol, env)?;
                let index_ty = transform_expr(index, env)?;

                check_int(&index_ty, index.pos)?;

                match target_ty.ty {
                    Type::Array(ref exp_ty) => Ok(ExpressionType {
                        exp: (),
                        ty: *exp_ty.clone(),
                    }),

                    Type::Str => Ok(ExpressionType {
                        exp: (),
                        ty: Type::Str,
                    }),
                    _ => Err(TypeError::NotArray),
                }
            }

            _ => Err(TypeError::InvalidIndex),
        },

        Expression::Literal(ref literal) => match *literal {
            Literal::Float(_) => Ok(ExpressionType {
                exp: (),
                ty: Type::Float,
            }),
            Literal::Int(_) => Ok(ExpressionType {
                exp: (),
                ty: Type::Int,
            }),
            Literal::Str(_) => Ok(ExpressionType {
                exp: (),
                ty: Type::Str,
            }),
            Literal::True(_) | Literal::False(_) => Ok(ExpressionType {
                exp: (),
                ty: Type::Bool,
            }),
            Literal::Nil => Ok(ExpressionType {
                exp: (),
                ty: Type::Nil,
            }),
        },

        Expression::Unary { ref expr, .. } => {
            let expr_ty = transform_expr(expr, env)?;

            check_bool(&expr_ty, expr.pos)?;

            Ok(expr_ty)
        }

        Expression::Logical {
            ref left,
            ref right,
            ..
        } => {
            let left_ty = transform_expr(left, env)?;
            check_bool(&left_ty, expr.pos)?;
            let right_ty = transform_expr(right, env)?;
            check_bool(&right_ty, expr.pos)?;

            Ok(ExpressionType {
                exp: (),
                ty: Type::Bool,
            })
        }

        Expression::Ternary {
            ref condition,
            ref then_branch,
            ref else_branch,
        } => {
            let condition_ty = transform_expr(condition, env)?;
            check_bool(&condition_ty, expr.pos)?;

            let then_ty = transform_expr(then_branch, env)?;
            let else_ty = transform_expr(else_branch, env)?;
            check_types(&then_ty.ty, &else_ty.ty)?;

            Ok(ExpressionType {
                exp: (),
                ty: then_ty.ty,
            })
        }

        Expression::Var(ref symbol, _) => transform_var(symbol, env),

        _ => unimplemented!(), // Implement classes or leave them out
    }
}

/// Given two expression types check if they are {int,int} or they are
/// {float,float}

fn check_int_float(
    left: &ExpressionType,
    right: &ExpressionType,
    pos: Postition,
) -> Result<ExpressionType, TypeError> {
    if check_int(left, pos).is_err() || check_int(right, pos).is_err() {
        check_float(left, pos)?;
        check_float(right, pos)?;

        Ok(ExpressionType {
            exp: (),
            ty: Type::Float,
        })
    } else if check_int(left, pos).is_ok() && check_int(left, pos).is_ok() {
        Ok(ExpressionType {
            exp: (),
            ty: Type::Int,
        })
    } else {
        Err(TypeError::Expected(Type::Int, pos))
    }
}

/// Given an ExpressionType check if it an {int} or {float}
fn s_check_int_float(expr: &ExpressionType, pos: Postition) -> Result<ExpressionType, TypeError> {
    if check_int(expr, pos).is_err() {
        check_float(expr, pos)?;
        return Ok(ExpressionType {
            exp: (),
            ty: Type::Float,
        });
    }
    Ok(ExpressionType {
        exp: (),
        ty: Type::Int,
    })
}

/// Checks if ExpressionType is {bool}
fn check_bool(right: &ExpressionType, pos: Postition) -> Result<(), TypeError> {
    if right.ty != Type::Bool {
        return Err(TypeError::Expected(Type::Bool, pos));
    }
    Ok(())
}

/// Checks if ExpressionType is {int}
fn check_int(expr: &ExpressionType, pos: Postition) -> Result<(), TypeError> {
    if expr.ty != Type::Int {
        return Err(TypeError::Expected(Type::Int, pos));
    }
    Ok(())
}

/// Checks if ExpressionType is {float}
fn check_float(expr: &ExpressionType, pos: Postition) -> Result<(), TypeError> {
    if expr.ty != Type::Float {
        return Err(TypeError::Expected(Type::Int, pos));
    }
    Ok(())
}
