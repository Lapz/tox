use ast::expr::*;
use types::{Type, TypeError};


type Exp = ();
#[derive(Debug)]
pub struct ExpressionType {
    pub exp: Exp,
    pub ty: Type,
}

pub fn analyse(expr: &Expression) -> Result<ExpressionType, TypeError> {

    transform_expr(expr)

}

/// Function that takes an expression and transforms it to
/// a simply typed AST
fn transform_expr(expr: &Expression) -> Result<ExpressionType, TypeError> {
    match *expr {
        Expression::Grouping { ref expr } => transform_expr(expr),

        Expression::Binary {
            ref left_expr,
            ref right_expr,
            ..
        } => {
            let left = transform_expr(left_expr)?;
            let right = transform_expr(right_expr)?;

            check_binary(left, right)
        }

        Expression::Unary { ref expr, .. } => {
            let expr = transform_expr(expr)?;
            check_int(expr)?;

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

fn check_binary(left: ExpressionType, right: ExpressionType) -> Result<ExpressionType, TypeError> {
    check_int(left)?;
    check_int(right)?;

    Ok(ExpressionType {
        exp: (),
        ty: Type::Int,
    })
}
fn check_unary(right: ExpressionType) -> Result<ExpressionType, TypeError> {
    unimplemented!()
}
fn check_int(expr: ExpressionType) -> Result<(), TypeError> {
    if expr.ty != Type::Int {
        return Err(TypeError::Expected(Type::Int));
    }
    Ok(())
}
