use std::collections::HashMap;

use crate::{
    hir::{ExprId, FunctionAstMap, TypeId},
    infer::{InferDataCollector, Type, TypeCon},
    typed,
    util::Span,
    HirDatabase,
};

use super::TypeVar;
use syntax::TextUnit;
use tracing::instrument;

impl<'a, DB> InferDataCollector<&'a DB>
where
    DB: HirDatabase,
{
    #[inline]
    pub fn infer_call_expr(
        &mut self,
        callee: &Span<ExprId>,
        args: &[Span<ExprId>],
        type_args: &Span<Vec<Span<TypeId>>>,
        arg_types: &[Type],
        vars: &[TypeVar],
        span: (TextUnit, TextUnit),
        map: &FunctionAstMap,
    ) -> typed::Typed<typed::Expr> {
        let ret = arg_types
            .last()
            .unwrap_or(&Type::Con(TypeCon::Void))
            .clone();

        if arg_types.len() == 0 {
            if args.len() > arg_types.len() {
                self.reporter.error(
                    "Too many arguments",
                    format!("Expected 0 arguments",),
                    callee.as_reporter_span(),
                );
            }
        } else if args.len() > arg_types.len() - 1 {
            self.reporter.error(
                "Missing arguments",
                format!(
                    "Too many arguments,expected `{}` found `{}",
                    arg_types.len(),
                    args.len()
                ),
                callee.as_reporter_span(),
            );
        } else if args.len() < arg_types.len() - 1 {
            self.reporter.error(
                "Missing arguments",
                format!(
                    "Too few arguments,expected `{}` found `{}",
                    arg_types.len(),
                    args.len()
                ),
                callee.as_reporter_span(),
            );
        }

        let mut subst = HashMap::new();

        let mut callee_exprs = Vec::new();

        for (var, type_arg) in vars.iter().zip(type_args.item.iter()) {
            let ty = self
                .resolver
                .lookup_intern_type(&type_arg.item)
                .unwrap_or(Type::Unknown);

            subst.insert(*var, ty);
        }

        if type_args.item.is_empty() {
            for (var, expr) in vars.iter().zip(args.iter()) {
                let inferred_expr = self.infer_expr(map, expr);
                callee_exprs.push(inferred_expr.clone());

                subst.insert(*var, inferred_expr.ty.clone());
            }
        }

        for (i, arg) in args.iter().enumerate() {
            let inferred = self.infer_expr(map, arg);

            let inferred_expr = self.subst(&inferred.ty, &mut subst);

            let ty = self.subst(arg_types.get(i).unwrap_or(&Type::Unknown), &mut subst);

            self.unify(
                &ty,
                &inferred_expr,
                arg.as_reporter_span(),
                Some("Wrong arguments provided to this function".into()),
                true,
            )
        }

        typed::Typed::new(
            typed::Expr::Call {
                callee: Box::new(self.infer_expr(map, callee)),
                args: args.iter().map(|expr| self.infer_expr(map, expr)).collect(),
                type_args: type_args
                    .item
                    .iter()
                    .map(|type_arg| {
                        self.resolver
                            .lookup_intern_type(&type_arg.item)
                            .unwrap_or(Type::Unknown)
                    })
                    .collect(),
            },
            self.subst(&ret, &mut subst),
            span,
        )
    }

    #[instrument(skip(self))]
    pub(crate) fn infer_call(
        &mut self,
        callee: &Span<ExprId>,
        args: &[Span<ExprId>],
        type_args: &Span<Vec<Span<TypeId>>>,
        span: (TextUnit, TextUnit),
        map: &FunctionAstMap,
    ) -> typed::Typed<typed::Expr> {
        let inferred_callee = self.infer_expr(map, callee);
        let ty = inferred_callee.ty.clone();
        match ty {
            Type::Poly(vars, inner) => match &*inner {
                Type::App(arg_types) => {
                    self.infer_call_expr(callee, args, type_args, arg_types, &vars, span, map)
                }
                ty => {
                    match ty {
                        Type::Unknown => {} // use of undefined function
                        _ => {
                            let msg = format!("Expected a function found `{:?}`", ty);

                            self.reporter.error(
                                msg,
                                "A call expression requires a function",
                                callee.as_reporter_span(),
                            );
                        }
                    }

                    typed::Typed::new(
                        typed::Expr::Call {
                            callee: Box::new(inferred_callee),
                            args: args.iter().map(|expr| self.infer_expr(map, expr)).collect(),
                            type_args: type_args
                                .item
                                .iter()
                                .map(|type_arg| {
                                    self.resolver
                                        .lookup_intern_type(&type_arg.item)
                                        .unwrap_or(Type::Unknown)
                                })
                                .collect(),
                        },
                        Type::Unknown,
                        span,
                    )
                }
            },
            Type::App(arg_types) => {
                self.infer_call_expr(callee, args, type_args, &arg_types, &vec![], span, map)
            }

            ty => {
                match ty {
                    Type::Unknown => {}
                    _ => {
                        let msg = format!("Expected a function found `{:?}`", ty);

                        self.reporter.error(
                            msg,
                            "A call expression requires a function",
                            callee.as_reporter_span(),
                        );
                    }
                }

                typed::Typed::new(
                    typed::Expr::Call {
                        callee: Box::new(inferred_callee),
                        args: args.iter().map(|expr| self.infer_expr(map, expr)).collect(),
                        type_args: type_args
                            .item
                            .iter()
                            .map(|type_arg| {
                                self.resolver
                                    .lookup_intern_type(&type_arg.item)
                                    .unwrap_or(Type::Unknown)
                            })
                            .collect(),
                    },
                    Type::Unknown,
                    span,
                )
            }
        }
    }
}
