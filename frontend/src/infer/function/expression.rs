use ast as t;
use ctx::CompileCtx;
use infer::env::VarEntry;
use infer::types::{Type, TypeCon};
use infer::{Infer, InferResult};

use syntax::ast::{Call, ClassLiteral, Expression, Literal, Op, UnaryOp};
use util::pos::Spanned;

impl Infer {
    pub(crate) fn infer_expr(
        &mut self,
        expr: Spanned<Expression>,
        ctx: &mut CompileCtx,
    ) -> InferResult<Spanned<t::TypedExpression>> {
        match expr.value {
            Expression::Array { mut items } => self.infer_array(items, expr.span, ctx),

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
                // let returns = if let Some(ref ty) = function.value.returns {
                //     self.trans_type(&ty, ctx)?
                // } else {
                //     Type::Nil
                // };

                // let mut param_types = Vec::with_capacity(function.value.params.value.len());
                // let mut env_types = Vec::with_capacity(function.value.params.value.len());

                // for param in function.value.params.value.iter() {
                //     let ty = self.trans_type(&param.value.ty, ctx)?;

                //     env_types.push(ty.clone());
                //     param_types.push(t::FunctionParam {
                //         name: param.value.name.value,
                //         ty,
                //     })
                // }

                // let fn_signature = Type::Fun(env_types.clone(), Box::new(returns.clone()), true);

                // ctx.add_var(
                //     function.value.name.value,
                //     VarEntry::Fun {
                //         ty: fn_signature.clone(),
                //     },
                // );

                // ctx.begin_scope();

                // for param in param_types.iter() {
                //     ctx.add_var(param.name, VarEntry::Var(param.ty.clone()))
                // }

                // let span = function.value.body.span;
                // let name = function.value.name.value;
                // let body = self.infer_statement(function.value.body, ctx)?;

                // ctx.end_scope();

                // (
                //     Spanned::new(
                //         t::Expression::Closure(Box::new(t::Function {
                //             name,
                //             params: param_types,
                //             body: Box::new(body),
                //             returns: returns.clone(),
                //         })),
                //         span,
                //     ),
                //     fn_signature,
                // )

                unimplemented!()
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

            Expression::Literal(literal) => self.infer_literal(literal, expr.span, ctx),

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

    // fn call(
    //     &mut self,
    //     call: Spanned<Expression>,
    //     ctx: &mut CompileCtx,
    // ) -> InferResult<(Spanned<t::Expression>, Type)> {
    //     match call.value {
    //         Expression::Call { callee, args } => match callee.value {
    //             Expression::Call { .. } => return self.infer_call(*callee, ctx),
    //             Expression::Var(ref sym) => {
    //                 if let Some(ty) = ctx.look_var(sym.value).cloned() {
    //                     let ty = ty.get_ty();

    //                     match ty {
    //                         Type::Fun(ref targs, ref ret, ref is_closure) => {
    //                             if args.len() != targs.len() {
    //                                 let msg = format!(
    //                                     "Expected `{}` args found `{}` ",
    //                                     targs.len(),
    //                                     args.len()
    //                                 );
    //                                 ctx.error(msg, call.span);
    //                                 return Err(());
    //                             }

    //                             let mut callee_exprs = Vec::with_capacity(args.len());

    //                             for (arg, param_type) in args.into_iter().zip(targs) {
    //                                 let span = arg.span;
    //                                 let ty_expr = self.infer_expr(arg, ctx)?;

    //                                 self.unify(param_type, &ty_expr.value.ty, span, ctx)?;

    //                                 callee_exprs.push(ty_expr)
    //                             }

    //                             Ok((
    //                                 Spanned::new(
    //                                     t::Expression::Call(sym.value, callee_exprs),
    //                                     call.span,
    //                                 ),
    //                                 if *is_closure {
    //                                     ty.clone()
    //                                 } else {
    //                                     *ret.clone()
    //                                 },
    //                             ))
    //                         }

    //                         _ => {
    //                             let msg = format!("`{}` is not callable", ctx.name(sym.value));

    //                             ctx.error(msg, callee.span);

    //                             return Err(());
    //                         }
    //                     }
    //                 } else {
    //                     let msg = format!("Undefined variable '{}' ", ctx.name(sym.value));
    //                     ctx.error(msg, sym.span);
    //                     return Err(());
    //                 }
    //             }

    //             Expression::Get { .. } => {
    //                 let (callee, ty) = self.infer_object_get(*callee, ctx)?;

    //                 match ty {
    //                     Type::Fun(ref param_types, ref returns, _) => {
    //                         if args.len() != param_types.len() {
    //                             let msg = format!(
    //                                 "Expected `{}` args found `{}` ",
    //                                 param_types.len(),
    //                                 args.len()
    //                             );
    //                             ctx.error(msg, call.span);
    //                             return Err(());
    //                         }

    //                         let mut callee_exprs = Vec::with_capacity(args.len());

    //                         for (arg, param_type) in args.into_iter().zip(param_types) {
    //                             let span = arg.span;
    //                             let ty_expr = self.infer_expr(arg, ctx)?;

    //                             self.unify(param_type, &ty_expr.value.ty, span, ctx)?;

    //                             callee_exprs.push(ty_expr)
    //                         }

    //                         match callee.value {
    //                             t::Expression::GetMethod {
    //                                 method_name,
    //                                 method,
    //                             } => match method.value.expr.value {
    //                                 t::Expression::Var(_, Type::Class(klass_name, _, _, _)) => {
    //                                     // type inference returns the type of the main classs
    //                                     Ok((
    //                                         Spanned::new(
    //                                             t::Expression::StaticMethodCall {
    //                                                 class_name: klass_name,
    //                                                 method_name,
    //                                                 params: callee_exprs,
    //                                             },
    //                                             call.span,
    //                                         ),
    //                                         *returns.clone(),
    //                                     ))
    //                                 }

    //                                 _ => Ok((
    //                                     Spanned::new(
    //                                         t::Expression::InstanceMethodCall {
    //                                             method_name,
    //                                             instance: method,
    //                                             params: callee_exprs,
    //                                         },
    //                                         call.span,
    //                                     ),
    //                                     *returns.clone(),
    //                                 )),
    //                             },

    //                             _ => unreachable!(),
    //                         }
    //                     }

    //                     _ => {
    //                         let msg = format!("Type {} is not callable", ty.print(ctx));
    //                         ctx.error(msg, callee.span);
    //                         return Err(());
    //                     }
    //                 }
    //             }
    //             Expression::Closure(function) => {
    //                 let returns = if let Some(ref ty) = function.value.returns {
    //                     self.trans_type(&ty, ctx)?
    //                 } else {
    //                     Type::Nil
    //                 };

    //                 let mut param_types = Vec::with_capacity(function.value.params.value.len());
    //                 let mut env_types = Vec::with_capacity(function.value.params.value.len());

    //                 for param in function.value.params.value.iter() {
    //                     let ty = self.trans_type(&param.value.ty, ctx)?;

    //                     env_types.push(ty.clone());
    //                     param_types.push(t::FunctionParam {
    //                         name: param.value.name.value,
    //                         ty,
    //                     })
    //                 }

    //                 let fn_signature =
    //                     Type::Fun(env_types.clone(), Box::new(returns.clone()), true);

    //                 ctx.add_var(
    //                     function.value.name.value,
    //                     VarEntry::Fun {
    //                         ty: fn_signature.clone(),
    //                     },
    //                 );

    //                 ctx.begin_scope();

    //                 for param in param_types.iter() {
    //                     ctx.add_var(param.name, VarEntry::Var(param.ty.clone()))
    //                 }

    //                 let span = function.value.body.span;
    //                 let name = function.value.name.value;
    //                 let body = self.infer_statement(function.value.body, ctx)?;

    //                 ctx.end_scope();

    //                 Ok((
    //                     Spanned::new(
    //                         t::Expression::Closure(Box::new(t::Function {
    //                             name,
    //                             params: param_types,
    //                             body: Box::new(body),
    //                             returns: returns.clone(),
    //                         })),
    //                         span,
    //                     ),
    //                     fn_signature,
    //                 )) // todo move to a method
    //             }
    //             _ => {
    //                 ctx.error(" Not callable", callee.span);
    //                 return Err(());
    //             }
    //         },

    //         _ => {
    //             ctx.error(" Not callable", call.span);
    //             Err(())
    //         }
    //     }
    // }
}
