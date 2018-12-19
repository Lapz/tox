use ast as t;
use ctx::CompileCtx;
use infer::env::VarEntry;
use infer::types::{Type, TypeCon, TypeVar};
use infer::{Infer, InferResult};
use syntax::ast::{AssignOperator, Expression, Function, Literal, Op, UnaryOp};
use util::pos::{Span, Spanned};
use util::symbol::Symbol;

impl Infer {
    pub(crate) fn infer_literal(
        &mut self,
        literal: Literal,
        whole_span: Span,
        ctx: &mut CompileCtx,
    ) -> InferResult<Spanned<t::TypedExpression>> {
        let ty = match &literal {
            Literal::Float(_) => Type::App(TypeCon::Float, vec![]),

            Literal::False(_) | Literal::True(_) => Type::App(TypeCon::Bool, vec![]),

            Literal::Str(_) => Type::App(TypeCon::Str, vec![]),

            Literal::Nil => Type::Nil, // Nil is given the type void as only statements return Nil

            Literal::Int(_) => Type::App(TypeCon::Int, vec![]),
        };

        Ok(Spanned::new(
            t::TypedExpression {
                expr: Box::new(Spanned::new(t::Expression::Literal(literal), whole_span)),
                ty,
            },
            whole_span,
        ))
    }
}
