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

pub fn analyse(expr: &WithPos<Statement>, env: &mut Env) -> Result<ExpressionType, TypeError> {
    trans_statement(expr, env)
}

/// Checks if two types are eqvilant
fn check_types(expected: &Type,unknown:&Type) -> Result<(),TypeError> {
    if expected != unknown  {
        return Err(TypeError::NotSame)
    }
    Ok(())
}

fn trans_var(symbol: &Symbol, env: &mut Env) -> Result<ExpressionType, TypeError> {
    match env.look_type(*symbol) {
        Some(ty) => Ok(ExpressionType {
            exp: (),
            ty: ty.clone(),
        }),
        None => Err(TypeError::Undefinded),
    }
}


fn trans_statement(
    statement: &WithPos<Statement>,
    env: &mut Env,
) -> Result<ExpressionType, TypeError> {
    match statement.node {
        Statement::ExpressionStmt(ref expr) => transform_expr(expr, env),
        Statement::Var(ref symbol, ref expr, ref ty) => {
            let exp_ty = transform_expr(expr, env)?;

            println!("{:?}",exp_ty );

            println!("{:?}",symbol );

            if let Some(ref ident) = *ty {
                let ty = get_type(symbol,env)?;

                check_types(&ty,&exp_ty.ty)?;

                return Ok(ExpressionType{exp:(),ty})

            }

            env.add_var(*symbol, Entry::VarEntry(Type::Nil));

            Ok(exp_ty)
        }

        Statement::Break | Statement::Continue => Ok(ExpressionType{exp:(),ty:Type::Nil}),
        
        // Statement::Block(ref expressions) => {
        //     for expr in expressions {
        //         transform_expr(expr,env)?;
        //     }
        // }

        Statement::IfStmt{ref condition,ref then_branch, ref else_branch} => {
            let condition_ty = transform_expr(condition,env)?;

            check_bool(condition_ty, statement.pos)?;

            let then_ty = trans_statement(then_branch,env)?;

            if let &Some(ref else_statement) = else_branch {
                let else_ty = trans_statement(else_statement,env)?;

                check_types(&then_ty.ty,&else_ty.ty)?;

                return Ok(ExpressionType{exp:(),ty:then_ty.ty})
            }

            check_types(&Type::Nil,&then_ty.ty)?;

            Ok(ExpressionType{exp:(),ty:then_ty.ty})
        }


        Statement::WhileStmt{ref condition,ref body} => {
            let condition_ty = transform_expr(condition,env)?;

            check_bool(condition_ty, statement.pos)?;


        }


        _ => unimplemented!(),
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
        Expression::Array{ref items} => {

            if items.is_empty() {
                return Ok(ExpressionType{exp:(),ty:Type::Array(Box::new(Type::Nil))})
            }

            let first_ty = transform_expr(&items[0],env)?;

            for item in items {
                match check_types(&first_ty.ty,&transform_expr(item,env)?.ty) {
                    Ok(_) => (),
                    Err(e) => return Err(e)
                }
            }

            Ok(ExpressionType{exp:(),ty:Type::Array(Box::new(first_ty.ty))})
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
                    Entry::FunEntry {ref params,ref returns} => {
                        for (arg,param) in arguments.iter().zip(params) {
                            
                        let exp = transform_expr(arg,&mut env.clone())?;

                        check_types(&param,&exp.ty)?;

                        return Ok(ExpressionType{exp:(),ty:returns.clone()})
                    }

                },
                
                _ => unimplemented!()

            }
          
        }

         Err(TypeError::Undefinded)

        }

        Expression::Dict{ref items} => {

            if items.is_empty() {
                return Ok(ExpressionType{exp:(),ty:Type::Dict(Box::new(Type::Nil),Box::new(Type::Nil))})
            }

            let first_key_ty = transform_expr(&items[0].0,env)?;
            let first_value_ty = transform_expr(&items[0].0,env)?;
            for item in items {
                match check_types(&first_key_ty.ty,&transform_expr(&item.0,env)?.ty) {
                    Ok(_) => (),
                    Err(e) => return Err(e)
                };

                match check_types(&first_value_ty.ty,&transform_expr(&item.1,env)?.ty) {
                    Ok(_) => (),
                    Err(e) => return Err(e)
                }
            }

            Ok(ExpressionType{exp:(),ty:Type::Dict(Box::new(first_key_ty.ty),Box::new(first_value_ty.ty))})
        }

         Expression::Func{ref body,ref returns,..} => {
            let body_ty = trans_statement(&body, env)?;

            if let Some(ref return_ty) = *returns {
                check_types(return_ty, &body_ty.ty)?;

                return Ok(ExpressionType{exp:(),ty:return_ty.clone()})
            }

            Ok(body_ty)

        }

       
        Expression::Grouping { ref expr } => transform_expr(expr, env),

        Expression::IndexExpr{ref target, ref index} => {
            unimplemented!()
        }

        Expression::Literal(ref literal) => {
            match *literal {
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
            }
        },


        Expression::Logical {ref left,ref right,..} => {

            let left_ty = transform_expr(left,env)?;
            check_bool(left_ty,expr.pos)?;
            let right_ty = transform_expr(left,env)?;
            check_bool(right_ty,expr.pos)?;

             Ok(ExpressionType {
                exp: (),
                ty: Type::Bool,
            })
        }

        Expression::Ternary {ref condition, ref then_branch, ref else_branch} => {
            let condition_ty = transform_expr(condition,env)?;
            check_bool(condition_ty,expr.pos)?;

            let then_ty = transform_expr(then_branch, env)?;
            let else_ty = transform_expr(else_branch, env)?;
            check_types(&then_ty.ty, &else_ty.ty)?;

            Ok(ExpressionType{exp:(),ty:then_ty.ty})

        }

    

         Expression::Var(ref symbol, _) => trans_var(symbol, env),

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
    if check_int(left, pos).is_err() {
        check_float(left, pos)?;
        check_float(right, pos)?;

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
fn check_bool(right: ExpressionType, pos: Postition) -> Result<(), TypeError> {
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

// fn trans_statement(env: &mut Env, statement: WithPos<Statement>) -> ExpressionType {
//     unimplemented!()
// }

fn trans_ty(env: &mut Env, symbol: Symbol) -> Result<ExpressionType, TypeError> {
    match env.types.look(symbol) {
        Some(ty) => Ok(ExpressionType {
            exp: (),
            ty: ty.clone(),
        }),
        None => Err(TypeError::Undefinded),
    }
}
