use std::collections::HashMap;

use crate::{
    hir::{ExprId, FunctionAstMap, NameId},
    infer::{InferDataCollector, Type},
    typed,
    util::Span,
    HirDatabase,
};

use indexmap::IndexMap;
use syntax::TextUnit;

impl<'a, DB> InferDataCollector<&'a DB>
where
    DB: HirDatabase,
{
    #[inline]
    pub fn infer_record_expr(
        &mut self,
        def: &Span<NameId>,
        fields: &[(Span<NameId>, Span<ExprId>)],
        span: (TextUnit, TextUnit),
        map: &FunctionAstMap,
    ) -> typed::Typed<typed::Expr> {
        let inferred_def = self.db.resolve_named_type(self.file, def.item);

        let mut subst = HashMap::new();

        match inferred_def {
            Type::Poly(vars, inner) => {
                match &*inner {
                    Type::Class {
                        name: class_name,
                        fields: field_types,
                        methods,
                        ..
                    } => {
                        let mut inferred_fields = IndexMap::with_capacity(fields.len());
                        // TODO check if this code is right ???
                        for (var, (name, expr)) in vars.iter().zip(fields.iter()) {
                            let inferred_expr = self.infer_expr(map, expr);

                            subst.insert(*var, inferred_expr.ty.clone());

                            inferred_fields.insert(*name, inferred_expr);
                        }

                        for (field, expr) in fields {
                            let inferred_expr = inferred_fields.get(field).unwrap();

                            if let Some(ty) = field_types.get(&field.item) {
                                match ty {
                                    Type::Var(tv) => {
                                        subst.insert(*tv, inferred_expr.ty.clone());
                                    }
                                    _ => {}
                                }
                            }

                            let expected = self.subst(
                                field_types.get(&field.item).unwrap_or(&Type::Unknown),
                                &mut subst,
                            );

                            self.unify(
                                &expected,
                                &inferred_expr.ty,
                                field.as_reporter_span(),
                                None,
                                true,
                            );
                        }

                        let instance_ty = Type::Poly(
                            vars.clone(),
                            Box::new(Type::Class {
                                name: *class_name,
                                fields: inferred_fields
                                    .iter()
                                    .map(|(name, expr)| (name.item, expr.ty.clone()))
                                    .collect(),
                                methods: methods.clone(),
                            }),
                        );

                        typed::Typed::new(
                            typed::Expr::RecordLiteral {
                                def: *def,
                                fields: inferred_fields.into_iter().collect::<Vec<(_, _)>>(),
                            },
                            self.subst(&instance_ty, &mut subst),
                            span,
                        )
                    }

                    _ => {
                        // Error reported in resolver
                        typed::Typed::new(
                            typed::Expr::RecordLiteral {
                                def: *def,
                                fields: fields
                                    .iter()
                                    .map(|(name, expr)| (*name, self.infer_expr(map, expr)))
                                    .collect::<Vec<(_, _)>>(),
                            },
                            Type::Unknown,
                            span,
                        )
                    }
                }
            }
            _ => {
                // Error reported in resolver
                typed::Typed::new(
                    typed::Expr::RecordLiteral {
                        def: *def,
                        fields: fields
                            .iter()
                            .map(|(name, expr)| (*name, self.infer_expr(map, expr)))
                            .collect::<Vec<(_, _)>>(),
                    },
                    Type::Unknown,
                    span,
                )
            }
        }
    }
}
