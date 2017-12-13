mod test;
use ast::expr::*;
use ast::statement::Statement;
use types::{Type, TypeError};
use env::Env;
use pos::{Postition, WithPos};
use symbol::Symbol;

type Exp = ();

#[derive(Debug, PartialEq)]
pub struct ExpressionType {
    pub exp: Exp,
    pub ty: Type,
}

pub fn analyse(expr: &WithPos<Statement>) -> Result<ExpressionType, TypeError> {
    trans_statement(expr)
}

fn trans_var(env: &mut Env, var: Expression) -> ExpressionType {
    unimplemented!()
}

fn trans_statement(statement: &WithPos<Statement>) -> Result<ExpressionType, TypeError> {
    match statement.node {
        Statement::ExpressionStmt(ref expr) => transform_expr(expr),
        _ => unimplemented!(),
    }
}

fn transform_expr(expr: &WithPos<Expression>) -> Result<ExpressionType, TypeError> {
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
                Equal => transform_expr(value),
                MinusEqual => s_check_int_float(&transform_expr(value)?, value.pos),
                PlusEqual => s_check_int_float(&transform_expr(value)?, value.pos),
                StarEqual => s_check_int_float(&transform_expr(value)?, value.pos),
                SlashEqual => s_check_int_float(&transform_expr(value)?, value.pos),
            }
        }

        Expression::Binary {
            ref left_expr,
            ref right_expr,
            ..
        } => {
            let left = transform_expr(left_expr)?;
            let right = transform_expr(right_expr)?;

            check_int_float(&left, &right, left_expr.pos)
        }

        Expression::Grouping { ref expr } => transform_expr(expr),

        Expression::Unary { ref expr, .. } => {
            let unary_expr = transform_expr(&expr)?;
            check_int(&unary_expr, expr.pos)?;

            Ok(ExpressionType {
                exp: (),
                ty: Type::Int,
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

fn check_unary(right: ExpressionType) -> Result<ExpressionType, TypeError> {
    unimplemented!()
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
