use std::collections::HashMap;

use crate::{
    hir::{ExprId, FunctionAstMap, NameId},
    infer::{InferDataCollector, Type},
    typed,
    util::Span,
    HirDatabase,
};

use syntax::TextUnit;

impl<'a, DB> InferDataCollector<&'a DB>
where
    DB: HirDatabase,
{
    #[inline]
    pub fn infer_enum_expr(
        &mut self,
        def: &Span<NameId>,
        variant: &Span<NameId>,
        expr: &Option<Span<ExprId>>,
        span: (TextUnit, TextUnit),
        map: &FunctionAstMap,
    ) -> typed::Typed<typed::Expr> {
        let inferred_def = self.db.resolve_named_type(self.file, def.item);

        let mut subst = HashMap::new();

        // TODO handle matching multiple type vars aka check record literal code

        match inferred_def {
            Type::Poly(ref vars, ref inner) => match &**inner {
                Type::Enum(_, ref variants) => {
                    if let Some(v) = variants.get(&variant.item) {
                        match (expr, &v.ty) {
                            (None, None) => typed::Typed::new(
                                typed::Expr::Enum {
                                    def: *def,
                                    variant: *variant,
                                    expr: None,
                                },
                                self.subst(&inferred_def, &mut subst),
                                span,
                            ),
                            (None, Some(variant_ty)) => {
                                let msg = format!("Missing enum variant constructor",);
                                self.reporter.error(
                                        msg,
                                        format!("Expected an enum variant constructor of type {:?} but found none",variant_ty),
                                        variant.as_reporter_span(),
                                    );
                                typed::Typed::new(
                                    typed::Expr::Enum {
                                        def: *def,
                                        variant: *variant,
                                        expr: None,
                                    },
                                    self.subst(&inferred_def, &mut subst),
                                    span,
                                )
                            }
                            (Some(id), None) => {
                                let msg = format!("Unexpected enum variant constructor",);
                                let name = self.db.lookup_intern_name(variant.item);
                                self.reporter.error(
                                    msg,
                                    format!("enum variant `{}` does not have a constructor", name),
                                    variant.as_reporter_span(),
                                );

                                let expr = self.infer_expr(map, id);

                                typed::Typed::new(
                                    typed::Expr::Enum {
                                        def: *def,
                                        variant: *variant,
                                        expr: Some(Box::new(expr)),
                                    },
                                    self.subst(&inferred_def, &mut subst),
                                    span,
                                )
                            }
                            (Some(expr), Some(variant_ty)) => {
                                let inferred_expr = self.infer_expr(map, expr);

                                for ty_var in vars {
                                    subst.insert(*ty_var, inferred_expr.ty.clone());
                                }

                                let variant_ty = self.subst(&variant_ty, &mut subst);

                                self.unify(
                                    &inferred_expr.ty,
                                    &variant_ty,
                                    expr.as_reporter_span(),
                                    None,
                                    true,
                                );

                                typed::Typed::new(
                                    typed::Expr::Enum {
                                        def: *def,
                                        variant: *variant,
                                        expr: Some(Box::new(inferred_expr)),
                                    },
                                    self.subst(&inferred_def, &mut subst),
                                    span,
                                )
                            }
                        }
                    } else {
                        //TODO report unknown variant
                        typed::Typed::new(
                            typed::Expr::Enum {
                                def: *def,
                                variant: *variant,
                                expr: expr.map(|id| Box::new(self.infer_expr(map, &id))),
                            },
                            Type::Unknown,
                            span,
                        )
                    }
                }

                _ => {
                    // Error reported in resolver

                    typed::Typed::new(
                        typed::Expr::Enum {
                            def: *def,
                            variant: *variant,
                            expr: expr.map(|id| Box::new(self.infer_expr(map, &id))),
                        },
                        Type::Unknown,
                        span,
                    )
                }
            },

            _ => {
                // Error reported in resolver
                typed::Typed::new(
                    typed::Expr::Enum {
                        def: *def,
                        variant: *variant,
                        expr: expr.map(|id| Box::new(self.infer_expr(map, &id))),
                    },
                    Type::Unknown,
                    span,
                )
            }
        }
    }
}
