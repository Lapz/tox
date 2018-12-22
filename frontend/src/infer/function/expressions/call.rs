use ast as t;
use ctx::CompileCtx;
use infer::env::VarEntry;
use infer::types::{self, Property, TypeCon, TypeVar};
use infer::{Infer, InferResult};
use std::collections::HashMap;
use syntax::ast::{
    AssignOperator, Call, ClassLiteralField, Expression, Function, Literal, Op, Type, UnaryOp,
};
use util::pos::{Span, Spanned};
use util::symbol::Symbol;

impl Infer {
    pub(crate) fn infer_call(
        &mut self,
        callee: Spanned<Expression>,
        args: Vec<Spanned<Expression>>,
        whole_span: Span,
        ctx: &mut CompileCtx,
    ) -> InferResult<Spanned<t::TypedExpression>> {
        match callee.value {
            Expression::Call(call) => {
                let whole_span = call.span;

                match call.value {
                    Call::Simple { callee, args } => {
                        self.infer_call(*callee, args, whole_span, ctx)
                    }
                    _ => unimplemented!(),
                }
            }
            Expression::Var(ref symbol) => {
                let func = if let Some(func) = ctx.look_var(symbol.value).cloned() {
                    func
                } else {
                    let msg = format!("Undefined function `{}`", ctx.name(symbol.value));

                    ctx.error(msg, symbol.span);

                    return Err(());
                };

                match func.get_ty() {
                    types::Type::Generic(ref typevars, ref ty) => match **ty {
                        types::Type::App(types::TypeCon::Arrow, ref func_types) => {
                            if func_types.len() - 1 != args.len() {
                                // minus one because the return type is stored along with the argument types

                                let msg = format!(
                                    "Expected `{}` args found `{}` ",
                                    func_types.len() - 1,
                                    args.len()
                                );
                                ctx.error(msg, whole_span);
                                return Err(());
                            }

                            let mut mappings = HashMap::new();

                            let mut arg_types = Vec::new();

                            struct CallExpression {
                                expr: Spanned<t::TypedExpression>,
                                ty: types::Type,
                            };

                            for arg in args {
                                let span = arg.span;
                                let typed_expr = self.infer_expr(arg, ctx)?;
                                let ty = typed_expr.value.ty.clone();

                                arg_types.push(Spanned {
                                    value: CallExpression {
                                        expr: typed_expr,
                                        ty,
                                    },
                                    span,
                                });
                            }

                            if !typevars.is_empty() {
                                for arg in arg_types.iter() {
                                    for type_var in typevars {
                                        mappings.insert(*type_var, arg.value.ty.clone());
                                    }
                                }
                            }

                            for (call_expression, def_type) in arg_types.iter_mut().zip(func_types)
                            {
                                self.unify(
                                    &self.subst(def_type, &mut mappings),
                                    &self.subst(&call_expression.value.ty, &mut mappings),
                                    call_expression.span,
                                    ctx,
                                )?;

                                call_expression.value.ty =
                                    self.subst(&call_expression.value.ty, &mut mappings);
                            }

                            Ok(Spanned {
                                value: t::TypedExpression {
                                    expr: Box::new(Spanned {
                                        value: t::Expression::Call(
                                            symbol.value,
                                            arg_types
                                                .into_iter()
                                                .map(|arg| arg.value.expr)
                                                .collect(),
                                        ),
                                        span: whole_span,
                                    }),
                                    ty: self.subst(func_types.last().unwrap(), &mut mappings),
                                },
                                span: whole_span,
                            })
                        }

                        _ => unreachable!(), // Only other possible generic types are structs. Structs are stored in a different environment and they it cannot be a struct
                    },
                    _ => {
                        let msg = format!("`{}` is not callable", ctx.name(symbol.value));

                        ctx.error(msg, callee.span);
                        Err(())
                    }
                }
            }
            Expression::Get { .. } => unimplemented!(),
            _ => unimplemented!(),
        }
    }

    pub(crate) fn infer_call_instantiated(
        &mut self,
        symbol: Spanned<Symbol>,
        props: Vec<Spanned<ClassLiteralField>>,
        types: Spanned<Vec<Spanned<Type>>>,
        whole_span: Span,
        ctx: &mut CompileCtx,
    ) -> InferResult<Spanned<t::TypedExpression>> {
        unimplemented!()
    }
}
