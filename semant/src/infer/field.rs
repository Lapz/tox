use crate::{
    hir::{Expr, ExprId, FunctionAstMap, Literal},
    infer::{InferDataCollector, Type},
    util::Span,
    HirDatabase,
};

use tracing::instrument;

impl<'a, DB> InferDataCollector<&'a DB>
where
    DB: HirDatabase,
{
    #[instrument(skip(self, map))]
    pub(crate) fn infer_field_exprs(
        &mut self,
        exprs: &[Span<ExprId>],
        ty: &Type,
        map: &FunctionAstMap,
    ) -> Type {
        if exprs.is_empty() {
            return ty.clone();
        }

        let expr = map.expr(&exprs[0].item);

        match expr {
            Expr::Call {
                callee,
                args,
                type_args,
            } => self.infer_call(callee, args, type_args, map),

            Expr::Field(exprs) => self.infer_field_exprs(exprs, ty, map),
            Expr::Ident(ident) => match ty {
                Type::Poly(_, inner) => match &**inner {
                    Type::Class { fields, .. } => {
                        if let Some(ty) = fields.get(&ident.item) {
                            self.infer_field_exprs(&exprs[1..], ty, map)
                        } else {
                            let msg = format!(
                                "Unknown record field `{}`",
                                self.db.lookup_intern_name(ident.item),
                            );

                            self.reporter.error(msg, "", exprs[0].as_reporter_span());

                            Type::Unknown
                        }
                    }
                    ty => {
                        let msg = format!(
                            "`{}` does not exist on `{:?}`",
                            self.db.lookup_intern_name(ident.item),
                            ty
                        );

                        self.reporter.error(msg, "", exprs[0].as_reporter_span());

                        Type::Unknown
                    }
                },

                Type::Class { fields, .. } => {
                    if let Some(ty) = fields.get(&ident.item) {
                        self.infer_field_exprs(&exprs[1..], ty, map)
                    } else {
                        let msg = format!(
                            "Unknown record field `{}`",
                            self.db.lookup_intern_name(ident.item),
                        );

                        self.reporter.error(msg, "", exprs[0].as_reporter_span());

                        Type::Unknown
                    }
                }

                ty => {
                    let msg = format!(
                        "`{}` does not exist on `{:?}`",
                        self.db.lookup_intern_name(ident.item),
                        ty
                    );

                    self.reporter.error(msg, "", exprs[0].as_reporter_span());

                    Type::Unknown
                }
            },

            Expr::Literal(literal) => {
                let lit = self.db.lookup_intern_literal(*literal);
                match lit {
                    Literal::Int(int) => {
                        let index: usize = int.parse().expect("Couldn't parse to i32");

                        match ty {
                            Type::Tuple(types) => {
                                if let Some(ty) = types.get(index) {
                                    self.infer_field_exprs(&exprs[1..], ty, map)
                                } else {
                                    let msg = format!("Unknown tuple field `{}`", index);

                                    self.reporter.error(msg, "", exprs[0].as_reporter_span());

                                    Type::Unknown
                                }
                            }
                            _ => {
                                let msg = format!("`{}` does not exist on `{:?}`", index, ty);

                                self.reporter.error(
                                    msg,
                                    "Numbered field access can only be used on tuples",
                                    exprs[0].as_reporter_span(),
                                );

                                Type::Unknown
                            }
                        }
                    }
                    _ => {
                        let msg = format!("`{:?}` is not a valid tuple field ", lit);

                        self.reporter.error(
                            msg,
                            "Tuple fields can only be numbers",
                            exprs[0].as_reporter_span(),
                        );

                        Type::Unknown
                    }
                }
            }

            _ => {
                self.reporter
                    .error("Unknown field".to_string(), "", exprs[0].as_reporter_span());

                Type::Unknown
            }
        }
    }
}
