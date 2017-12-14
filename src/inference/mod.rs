mod test;
use ast::expr::*;
use ast::statement::Statement;
use types::{Type, TypeError};
use env::{Env,Entry};
use pos::{Postition, WithPos};
use symbol::Symbol;

type Exp = ();

#[derive(Debug, PartialEq)]
pub struct ExpressionType {
    pub exp: Exp,
    pub ty: Type,
}

pub fn analyse(expr: &WithPos<Statement>,env: &mut Env) -> Result<ExpressionType, TypeError> {
    trans_statement(expr,env)
}

fn trans_var(symbol: &Symbol,env: &mut Env) ->  Result<ExpressionType, TypeError> {
    match env.look(*symbol) {
        Some(ty) => Ok(ExpressionType{exp:(),ty:ty.clone()}),
        None => Err(TypeError::Undefinded)
    }
}

fn trans_statement(statement: &WithPos<Statement>,env: &mut Env) -> Result<ExpressionType, TypeError> {
    match statement.node {
        Statement::ExpressionStmt(ref expr) => transform_expr(expr,env),
        Statement::Var(symbol,ref expr, ref ty) => {
            let exp_ty = transform_expr(expr, env);

            if let Some(ref ident) = *ty {
                let ty = get_type(ident)?;

                check_types(&ty)?;

                return Ok(ExpressionType{exp:(),ty})
                
            }

            env.add_var(symbol, Entry::VarEntry(ty));

            unimplemented!()
        }
        _ => unimplemented!(),
    }
}

fn get_type(ident:&Symbol,env:&mut Env) -> Result<Type,TypeError> {
    if let Some(ty) = env.look_type(*ident) {
        return Ok(ty.clone())
    }

    Err(TypeError::Undefinded)
}

fn transform_expr(expr: &WithPos<Expression>,env: &mut Env) -> Result<ExpressionType, TypeError> {
    match expr.node {
        // Expression::Array{ref items} => {
        //     for item in items {
        //         transform_expr(item)
        //     }

        // }
        Expression::Assign {
            ref value,
            ref kind,
            ..
        } => {
            use ast::expr::AssignOperator::*;
            match *kind {
                Equal => transform_expr(value,env),
                MinusEqual => s_check_int_float(&transform_expr(value,env)?, value.pos),
                PlusEqual => s_check_int_float(&transform_expr(value,env)?, value.pos),
                StarEqual => s_check_int_float(&transform_expr(value,env)?, value.pos),
                SlashEqual => s_check_int_float(&transform_expr(value,env)?, value.pos),
            }
        }

        Expression::Binary {
            ref left_expr,
            ref right_expr,
            ..
        } => {
            let left = transform_expr(left_expr,env)?;
            let right = transform_expr(right_expr,env)?;

            check_int_float(&left, &right, left_expr.pos)
        }

        Expression::Call {ref callee,ref arguments} => {
            let callee = match callee.node {
                Expression::Var(sym,_) => sym,
                _ => unreachable!()
            };

            if let Some(entry) = env.look(callee) {

            }

            Err(TypeError::Undefinded)
        }

        Expression::Var(ref symbol,_) => trans_var(symbol,env),
        Expression::Grouping { ref expr } => transform_expr(expr,env),

        Expression::Unary { ref expr, .. } => {
            let unary_expr = transform_expr(&expr,env)?;
            check_bool(unary_expr, expr.pos)?;
            Ok(ExpressionType {
                exp: (),
                ty: Type::Bool,
            })
        }

        Expression::Literal(ref literal) => {
            use ast::expr::Literal::*;
            match *literal {
                Float(_) => Ok(ExpressionType {
                    exp: (),
                    ty: Type::Float,
                }),
                Int(_) => Ok(ExpressionType {
                    exp: (),
                    ty: Type::Int,
                }),
                Str(_) => Ok(ExpressionType {
                    exp: (),
                    ty: Type::Str,
                }),
                True(_) | False(_) => Ok(ExpressionType {
                    exp: (),
                    ty: Type::Bool,
                }),
                Nil => Ok(ExpressionType {
                    exp: (),
                    ty: Type::Nil,
                }),
            }
        }

        _ => unimplemented!(),
    }
}

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

fn check_bool(right: ExpressionType,pos:Postition) -> Result<(), TypeError> {
    if right.ty != Type::Bool {
        return Err(TypeError::Expected(Type::Bool, pos));
    }
    Ok(())
}

fn check_int(expr: &ExpressionType, pos: Postition) -> Result<(), TypeError> {
    if expr.ty != Type::Int {
        return Err(TypeError::Expected(Type::Int, pos));
    }
    Ok(())
}

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
