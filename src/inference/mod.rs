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
    let expected = actual_type(expected);
    let unknown = actual_type(unknown);

    if expected != unknown {
        let msg = format!("Expected {} but found {}", expected, unknown);
        return Err(TypeError::NotSame(msg));
    }
    Ok(())
}

fn get_actual_ty(entry: &Entry) -> Result<Type, TypeError> {
    match *entry {
        Entry::VarEntry(ref ty) => Ok(ty.clone()),
        _ => Err(TypeError::Function),
    }
}

fn actual_type(ty: &Type) -> &Type {
    match *ty {
        Type::Name(_, ref name) => name,
        ref others => others,
    }
}

fn transform_var(
    symbol: &Symbol,
    pos: Postition,
    env: &mut Env,
) -> Result<ExpressionType, TypeError> {
    match env.look_var(*symbol) {
        Some(ty) => Ok(ExpressionType {
            exp: (),
            ty: actual_type(&get_actual_ty(ty)?).clone(),
        }),
        None => Err(TypeError::UndefindedVar(env.name(*symbol), pos)),
    }
}

fn transform_statement(
    statement: &WithPos<Statement>,
    env: &mut Env,
) -> Result<ExpressionType, TypeError> {
    match statement.node {
        Statement::ExpressionStmt(ref expr) => transform_expr(expr, env),
        Statement::Print(ref expr) => transform_expr(expr, env),
        Statement::Class {
            ref name,
            ref methods,
            ref properties,
        } => {
            let mut properties_ty = vec![];
            let mut class_methods: Vec<(Symbol, Entry)> = vec![];

            for &(property, ref ty) in properties {
                let ty = get_type(ty, env)?;
                properties_ty.push((property, ty))
            }

            for &WithPos { ref node, .. } in methods {
                match *node {
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

                                class_methods.push((
                                    *name,
                                    Entry::FunEntry {
                                        params: param_ty,
                                        returns: return_type.clone(),
                                    },
                                ));
                            }
                            _ => unreachable!(),
                        };
                    }
                    _ => unreachable!(),
                }
            }

            let ty = Type::Class {
                name: *name,
                methods: class_methods,
                fields: properties_ty,
            };

            env.add_var(*name, Entry::VarEntry(ty.clone()));

            Ok(ExpressionType { exp: (), ty })
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

        Statement::TypeAlias { ref alias, ref ty } => {
            let alias_ty = get_type(ty, env)?;
            env.add_type(
                *alias,
                Type::Name(alias.clone(), Box::new(alias_ty.clone())),
            );

            Ok(ExpressionType {
                exp: (),
                ty: alias_ty,
            })
        }

        Statement::Block(ref expressions) => {
            if expressions.is_empty() {
                return Ok(ExpressionType {
                    exp: (),
                    ty: Type::Nil,
                });
            }

            env.begin_scope();

            for expr in expressions.iter().rev().skip(1) {
                transform_statement(expr, env)?;
            }

            env.end_scope();

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
            ref operator,
        } => {
            let left = transform_expr(left_expr, env)?;
            let right = transform_expr(right_expr, env)?;

            use ast::expr::Operator;
            match *operator {

                Operator::BangEqual | Operator::EqualEqual => {
                    Ok(ExpressionType {
                        exp: (),
                        ty: Type::Bool,
                    })
                }
                Operator::LessThan
                | Operator::LessThanEqual
                | Operator::GreaterThan
                | Operator::GreaterThanEqual => {
                    check_int_float_str(&left, &right, left_expr.pos)?;
                    Ok(ExpressionType {
                        exp: (),
                        ty: Type::Bool,
                    })
                }

                Operator::Plus => {
                    check_int_float_str(&left, &right, left_expr.pos)
                }

                Operator::Slash
                | Operator::Star
                | Operator::Modulo
                | Operator::Minus
                | Operator::Exponential => {
                     check_int_float_str(&left, &right, left_expr.pos)
                }

                // _ => unimplemented!(),
            }
        }

        Expression::Call {
            ref callee,
            ref arguments,
        } => {
            let callee = match callee.node {
                Expression::Var(sym, _) => sym,
                Expression::Get {
                    ref object,
                    ref property,
                    ..
                } => {
                    let sym = match object.node {
                        Expression::Var(sym, _) => sym,
                        _ => unimplemented!(),
                    };

                    if let Some(entry) = env.look_var(sym).cloned() {
                        match entry {
                            Entry::VarEntry(ref class) => match class {
                                &Type::Class { ref methods, .. } => {
                                    let mut found = false;
                                    for &(ref key, ref value) in methods {
                                        if key == property {
                                            found = true;

                                            match value {
                                                &Entry::VarEntry(ref ty) => {
                                                    return Ok(ExpressionType {
                                                        exp: (),
                                                        ty: ty.clone(),
                                                    })
                                                }

                                                &Entry::FunEntry {
                                                    ref params,
                                                    ref returns,
                                                } => {
                                                    for (arg, param) in arguments.iter().zip(params)
                                                    {
                                                        let exp =
                                                            transform_expr(arg, &mut env.clone())?;

                                                        check_types(&param, &exp.ty)?;
                                                    }
                                                    return Ok(ExpressionType {
                                                        exp: (),
                                                        ty: actual_type(returns).clone(),
                                                    });
                                                }
                                            }
                                        }
                                    }

                                    if !found {
                                        return Err(TypeError::NotProperty(env.name(sym), expr.pos));
                                    }
                                }

                                _ => unimplemented!("TODO ADD AN ERROR"),
                            },

                            _ => unreachable!(),
                        }
                    }

                    return Err(TypeError::Undefinded);
                }
                _ => unreachable!(),
            };

            if let Some(entry) = env.look_var(callee).cloned() {
                match entry {
                    Entry::FunEntry {
                        ref params,
                        ref returns,
                    } => {
                        for (arg, param) in arguments.iter().zip(params) {
                            let exp = transform_expr(arg, &mut env.clone())?;

                            check_types(&param, &exp.ty)?;
                        }
                        return Ok(ExpressionType {
                            exp: (),
                            ty: actual_type(returns).clone(),
                        });
                    }

                    _ => unreachable!(),
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

        Expression::Get {
            ref object,
            ref property,
            ..
        } => {
            let instance = transform_expr(object, env)?;

            let mut ty = Type::Nil;

            match instance.ty {
                Type::Class { ref fields, .. } => {
                    let mut found = false;

                    for prop in fields {
                        if prop.0 == *property {
                            found = true;
                            ty = prop.1.clone();
                        }
                    }
                    if !found {
                        return Err(TypeError::NotProperty(env.name(*property), expr.pos));
                    }
                }

                _ => unreachable!(),
            }

            Ok(ExpressionType { exp: (), ty })
        }

        Expression::Grouping { ref expr } => transform_expr(expr, env),

        Expression::IndexExpr {
            ref target,
            ref index,
        } => match target.node {
            Expression::Var(ref symbol, _) => {
                let target_ty = transform_var(symbol, expr.pos, env)?;
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

        Expression::ClassInstance {
            ref properties,
            ref name,
        } => {
            let class = transform_var(name, expr.pos, env)?;

            match class.ty {
                Type::Class { ref fields, .. } => {
                    let mut found = true;
                    for &(ref key, ref value) in fields {
                        for &(ref instance_name, ref instance_val) in properties {
                            if instance_name == key {
                                found = true;

                                let instance_val_ty = transform_expr(instance_val, env)?;

                                check_types(value, &instance_val_ty.ty)?;
                            }
                        }

                        if !found {
                            return Err(TypeError::NotProperty(env.name(*key), expr.pos));
                        }
                    }

                    if fields.len() < properties.len() {
                        return Err(TypeError::TooManyProperty);
                    } else if fields.len() > properties.len() {
                        return Err(TypeError::TooLittleProperty);
                    }
                }
                _ => unimplemented!(), //TODO CHANGE INTO HARD ERROR
            };

            Ok(class)
        }

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

        Expression::Unary {
            ref expr,
            ref operator,
        } => {
            let expr_ty = transform_expr(expr, env)?;

            match *operator {
                UnaryOperator::Bang => check_bool(&expr_ty, expr.pos)?,
                UnaryOperator::Minus => {
                    s_check_int_float(&expr_ty, expr.pos)?;
                }
            };

            Ok(expr_ty)
        }

        Expression::This(_) => Ok(ExpressionType {
            exp: (),
            ty: Type::Nil,
        }),

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

        Expression::Set {
            ref object,
            ref name,
            ref value,
            ..
        } => {
            let instance = transform_expr(object, env)?;
            let mut ty = Type::Nil;

            match instance.ty {
                Type::Class { ref fields, .. } => {
                    let mut found = false;

                    for prop in fields {
                        if prop.0 == *name {
                            found = true;
                            let value_ty = transform_expr(value, env)?;
                            check_types(&prop.1, &value_ty.ty)?;
                            ty = prop.1.clone();
                        }
                    }
                    if !found {
                        return Err(TypeError::NotProperty(env.name(*name), expr.pos));
                    }
                }

                _ => unreachable!(),
            }

            Ok(ExpressionType { exp: (), ty })
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

        Expression::Var(ref symbol, _) => transform_var(symbol, expr.pos, env),
    }
}

/// Given two expression types check if they are {int,int} or they are
/// {float,float}

fn check_int_float_str(
    left: &ExpressionType,
    right: &ExpressionType,
    pos: Postition,
) -> Result<ExpressionType, TypeError> {
    if check_int(left, pos).is_err() || check_int(right, pos).is_err() {
        if check_float(left, pos).is_ok() {
            check_float(right, pos)?;
            Ok(ExpressionType {
                exp: (),
                ty: Type::Float,
            })
        } else {
            check_str(left, pos)?;
            check_str(right, pos)?;
            Ok(ExpressionType {
                exp: (),
                ty: Type::Str,
            })
        }
    } else if check_int(left, pos).is_ok() && check_int(right, pos).is_ok() {
        Ok(ExpressionType {
            exp: (),
            ty: Type::Int,
        })
    } else {
        Err(TypeError::Expected(Type::Int, right.ty.clone(), pos))
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
        return Err(TypeError::Expected(Type::Bool, right.ty.clone(), pos));
    }
    Ok(())
}

/// Checks if ExpressionType is {int}
fn check_int(expr: &ExpressionType, pos: Postition) -> Result<(), TypeError> {
    if expr.ty != Type::Int {
        return Err(TypeError::Expected(Type::Int, expr.ty.clone(), pos));
    }
    Ok(())
}

/// Checks if ExpressionType is {str}
fn check_str(expr: &ExpressionType, pos: Postition) -> Result<(), TypeError> {
    if expr.ty != Type::Str {
        return Err(TypeError::Expected(Type::Str, expr.ty.clone(), pos));
    }
    Ok(())
}
/// Checks if ExpressionType is {float}
fn check_float(expr: &ExpressionType, pos: Postition) -> Result<(), TypeError> {
    if expr.ty != Type::Float {
        return Err(TypeError::Expected(Type::Float, expr.ty.clone(), pos));
    }
    Ok(())
}
