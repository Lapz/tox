use super::super::types;
use ast as t;
use ctx::CompileCtx;
use infer::{Infer, InferResult};
use syntax::ast::{Call, ClassLiteral, Expression};
use util::pos::Spanned;

impl Infer {
    pub(crate) fn infer_expr(
        &mut self,
        expr: Spanned<Expression>,
        ctx: &mut CompileCtx,
    ) -> InferResult<Spanned<t::TypedExpression>> {
        match expr.value {
            Expression::Array { items } => self.infer_array(items, expr.span, ctx),

            Expression::Assign {
                name, kind, value, ..
            } => self.infer_assign(name, kind, value, expr.span, ctx),

            Expression::Binary { lhs, op, rhs } => self.infer_binary(lhs, op, rhs, expr.span, ctx),

            Expression::Call(call) => {
                let whole_span = expr.span;
                match call.value {
                    Call::Simple { args, callee } => {
                        self.infer_call(*callee, args, whole_span, ctx)
                    }
                    Call::Instantiation {
                        types,
                        callee,
                        args,
                    } => self.infer_call_instantiated(*callee, args, types, whole_span, ctx),
                }
            }

            Expression::Closure(function) => {
                let whole_span = expr.span;
                let closure = self.infer_function(*function, ctx)?;
                let mut params: Vec<types::Type> = closure
                    .params
                    .iter()
                    .map(|param| param.ty.clone())
                    .collect();
                params.push(closure.returns.clone());
                let ty = types::Type::Generic(
                    vec![],
                    Box::new(types::Type::App(types::TypeCon::Arrow, params)),
                );

                Ok(Spanned {
                    value: t::TypedExpression {
                        expr: Box::new(Spanned::new(
                            t::Expression::Closure(Box::new(closure)),
                            whole_span,
                        )),
                        ty,
                    },
                    span: whole_span,
                })
            }

            Expression::ClassLiteral(class_literal) => {
                let whole_span = expr.span;

                match class_literal.value {
                    ClassLiteral::Simple { symbol, props } => {
                        self.infer_class_literal(symbol, props, whole_span, ctx)
                    }
                    ClassLiteral::Instantiation {
                        symbol,
                        types,
                        props,
                    } => {
                        self.infer_class_instantiated_literal(symbol, props, types, whole_span, ctx)
                    }
                }
            }

            Expression::Grouping { expr: inner } => self.infer_grouping(*inner, expr.span, ctx),

            Expression::Get { object, property } => {
                self.infer_get(*object, property, expr.span, ctx)
            }

            Expression::SubScript { target, index } => {
                self.infer_subscript(*target, *index, expr.span, ctx)
            }

            Expression::Literal(literal) => self.infer_literal(literal, expr.span),

            Expression::Set {
                object,
                name,
                value,
            } => self.infer_set(*object, name, *value, expr.span, ctx),

            Expression::Ternary {
                condition,
                then_branch,
                else_branch,
            } => self.infer_ternary(*condition, *then_branch, *else_branch, expr.span, ctx),

            Expression::Unary { expr: operand, op } => {
                self.infer_unary(op, *operand, expr.span, ctx)
            }

            Expression::Var(var) => self.infer_var(var, expr.span, ctx),
        }
    }
}
