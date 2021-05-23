use std::collections::HashMap;

use crate::{
    hir::{ExprId, FunctionAstMap, TypeId},
    infer::{InferDataCollector, Type, TypeCon},
    util::Span,
    HirDatabase,
};

use super::TypeVar;
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
        map: &FunctionAstMap,
    ) -> Type {
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

                subst.insert(*var, inferred_expr);
            }
        }

        for (i, arg) in args.iter().enumerate() {
            let inferred = self.infer_expr(map, arg);

            let inferred_expr = self.subst(&inferred, &mut subst);

            let ty = self.subst(arg_types.get(i).unwrap_or(&Type::Unknown), &mut subst);

            self.unify(
                &ty,
                &inferred_expr,
                arg.as_reporter_span(),
                Some("Wrong arguments provided to this function".into()),
                true,
            )
        }

        self.subst(&ret, &mut subst)
    }

    #[instrument(skip(self))]
    pub(crate) fn infer_call(
        &mut self,
        callee: &Span<ExprId>,
        args: &[Span<ExprId>],
        type_args: &Span<Vec<Span<TypeId>>>,
        map: &FunctionAstMap,
    ) -> Type {
        let inferred_callee = self.infer_expr(map, callee);

        match inferred_callee {
            Type::Poly(vars, inner) => match &*inner {
                Type::App(arg_types) => {
                    self.infer_call_expr(callee, args, type_args, arg_types, &vars, map)
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

                    Type::Unknown
                }
            },
            Type::App(arg_types) => {
                self.infer_call_expr(callee, args, type_args, &arg_types, &vec![], map)
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

                Type::Unknown
            }
        }
    }
}
