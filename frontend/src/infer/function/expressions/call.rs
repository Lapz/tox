use crate::ast as t;
use crate::ctx::CompileCtx;
use crate::infer::types;
use crate::infer::{Infer, InferResult};
use std::collections::HashMap;
use syntax::ast::{Call, Expression, Type};
use util::pos::{Span, Spanned};

impl Infer {
    pub(crate) fn infer_call(
        &mut self,
        callee: Spanned<Expression>,
        args: Vec<Spanned<Expression>>,
        types: Spanned<Vec<Spanned<Type>>>,
        whole_span: Span,
        ctx: &mut CompileCtx,
    ) -> InferResult<Spanned<t::TypedExpression>> {
        match callee.value {
            Expression::Call(call) => {
                let whole_span = call.span;

                match call.value {
                    Call {
                        types,
                        callee,
                        args,
                    } => self.infer_call(*callee, args, types, whole_span, ctx),
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

                            for (ty, type_var) in types.value.into_iter().zip(typevars.iter()) {
                                mappings.insert(*type_var, self.trans_type(&ty, ctx)?);
                            }

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

            Expression::Get { object, property } => {
                let expression = self.infer_get(*object, property, whole_span, ctx)?;

                match expression.value.ty {
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
                            } // we only build the mappings if its a generic function

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

                            match expression.value.expr.value {
                                t::Expression::GetMethod {
                                    method_name,
                                    method,
                                } => match method.value.expr.value {
                                    t::Expression::Var(_, types::Type::Generic(_, ref ty)) => {
                                        match **ty {
                                            types::Type::Class(class_name, _, _, _) => {
                                                Ok(Spanned {
                                                    value: t::TypedExpression {
                                                        expr: Box::new(Spanned {
                                                            value:
                                                                t::Expression::StaticMethodCall {
                                                                    class_name,
                                                                    method_name,
                                                                    params: arg_types
                                                                        .into_iter()
                                                                        .map(|arg| arg.value.expr)
                                                                        .collect(),
                                                                },
                                                            span: whole_span,
                                                        }),
                                                        ty: self.subst(
                                                            func_types.last().unwrap(),
                                                            &mut mappings,
                                                        ),
                                                    },
                                                    span: whole_span,
                                                })
                                            }

                                            _ => Ok(Spanned {
                                                value: t::TypedExpression {
                                                    expr: Box::new(Spanned {
                                                        value: t::Expression::InstanceMethodCall {
                                                            method_name,
                                                            instance: method.clone(),
                                                            params: arg_types
                                                                .into_iter()
                                                                .map(|arg| arg.value.expr)
                                                                .collect(),
                                                        },
                                                        span: whole_span,
                                                    }),
                                                    ty: self.subst(
                                                        func_types.last().unwrap(),
                                                        &mut mappings,
                                                    ),
                                                },
                                                span: whole_span,
                                            }),
                                        }
                                    }

                                    t::Expression::ClassLiteral { .. } => Ok(Spanned {
                                        value: t::TypedExpression {
                                            expr: Box::new(Spanned {
                                                value: t::Expression::InstanceMethodCall {
                                                    method_name,
                                                    instance: method.clone(),
                                                    params: arg_types
                                                        .into_iter()
                                                        .map(|arg| arg.value.expr)
                                                        .collect(),
                                                },
                                                span: whole_span,
                                            }),
                                            ty: self
                                                .subst(func_types.last().unwrap(), &mut mappings),
                                        },
                                        span: whole_span,
                                    }),

                                    ref e => unreachable!("{:?}", e),
                                },

                                _ => unreachable!(),
                            }
                        }
                        _ => unreachable!(), // Only other possible generic types are structs. Structs are stored in a different environment and they it cannot be a struct
                    },

                    ref ty => {
                        let msg = format!("`{:?}` is not callable", ty);

                        ctx.error(msg, callee.span);
                        Err(())
                    }
                }
            }

            Expression::Closure(function) => {
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

            _ => {
                ctx.error("Not callable", whole_span);
                Err(())
            }
        }
    }
}
