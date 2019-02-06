use ast as t;
use infer::{Infer, InferResult};
use infer::types::{Type, TypeCon};
use syntax::ast::Literal;
use util::pos::{Span, Spanned};

impl Infer {
    pub(crate) fn infer_literal(
        &mut self,
        literal: Literal,
        whole_span: Span,
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
