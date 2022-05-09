use crate::{
    hir::{Expr, ExprId, FunctionAstMap, Literal},
    infer::{InferDataCollector, Type},
    typed,
    util::Span,
    HirDatabase,
};

use syntax::TextUnit;
use tracing::instrument;

impl<'a, DB> InferDataCollector<&'a DB>
where
    DB: HirDatabase,
{
    #[instrument(skip(self, map))]
    pub(crate) fn infer_field_exprs(
        &mut self,
        fields: &[Span<ExprId>],
        span: (TextUnit, TextUnit),
        map: &FunctionAstMap,
    ) -> typed::Typed<typed::Expr> {
        let base = self.infer_expr(map, &fields[0]);

        let ty = base.ty.clone();

        let mut inferred_fields = vec![base];

        match ty {
            Type::Poly(_, ref inner) => match &**inner {
                _ty @ Type::Class { .. } => {}
                ty => {
                    let msg = format!(
                        "Expected expression of type `class` instead found `{:?}`",
                        ty
                    );

                    self.reporter.error(
                        msg,
                        "Field access only works on classes",
                        fields[0].as_reporter_span(),
                    );

                    return typed::Typed::new(typed::Expr::Field(vec![]), Type::Unknown, span);
                }
            },

            Type::Tuple(_) => {}

            Type::Unknown => {
                return typed::Typed::new(typed::Expr::Field(vec![]), Type::Unknown, span)
            }
            _ => {
                let msg = format!(
                    "Expected expression of type `class` instead found `{:?}`",
                    ty
                );

                self.reporter.error(
                    msg,
                    "Field access only works on classes",
                    fields[0].as_reporter_span(),
                );

                return typed::Typed::new(typed::Expr::Field(vec![]), Type::Unknown, span);
            }
        }

        for field in fields.iter().skip(1) {
            let expr = map.expr(&field.item);

            match expr {
                Expr::Call {
                    callee,
                    args,
                    type_args,
                } => inferred_fields.push(self.infer_call(callee, args, type_args, span, map)),

                Expr::Field(exprs) => {
                    inferred_fields.push(self.infer_field_exprs(
                        exprs,
                        (field.start, field.end),
                        map,
                    ));
                    // Todo
                }
                Expr::Ident(ident) => match &ty {
                    Type::Poly(_, ref inner) => match &**inner {
                        Type::Class { fields, .. } => {
                            if let Some(ty) = fields.get(&ident.item) {
                                inferred_fields.push(typed::Typed::new(
                                    typed::Expr::Ident(*ident),
                                    ty.clone(),
                                    (field.start, field.end),
                                ));

                                // self.infer_field_exprs(&exprs[1..], ty, span, map)
                            } else {
                                let msg = format!(
                                    "Unknown record field `{}`",
                                    self.db.lookup_intern_name(ident.item),
                                );

                                self.reporter.error(msg, "", field.as_reporter_span());

                                break;
                            }
                        }
                        ty => {
                            let msg = format!(
                                "`{}` does not exist on `{:?}`",
                                self.db.lookup_intern_name(ident.item),
                                ty
                            );

                            self.reporter.error(msg, "", field.as_reporter_span());

                            break;
                        }
                    },

                    Type::Class { fields, .. } => {
                        if let Some(_ty) = fields.get(&ident.item) {
                            // self.infer_field_exprs(&exprs[1..], ty, map)
                        } else {
                            let msg = format!(
                                "Unknown record field `{}`",
                                self.db.lookup_intern_name(ident.item),
                            );

                            self.reporter.error(msg, "", field.as_reporter_span());

                            break;
                        }
                    }

                    ty => {
                        let msg = format!(
                            "`{}` does not exist on `{:?}`",
                            self.db.lookup_intern_name(ident.item),
                            ty
                        );

                        self.reporter.error(msg, "", field.as_reporter_span());

                        break;
                    }
                },

                Expr::Literal(literal) => {
                    let lit = self.db.lookup_intern_literal(*literal);
                    match lit {
                        Literal::Int(int) => {
                            let index: usize = int.parse().expect("Couldn't parse to i32");

                            match ty {
                                Type::Tuple(ref types) => {
                                    if let Some(_ty) = types.get(index) {
                                    } else {
                                        let msg = format!("Unknown tuple field `{}`", index);

                                        self.reporter.error(msg, "", field.as_reporter_span());

                                        break;
                                    }
                                }
                                _ => {
                                    let msg = format!("`{}` does not exist on `{:?}`", index, ty);

                                    self.reporter.error(
                                        msg,
                                        "Numbered field access can only be used on tuples",
                                        field.as_reporter_span(),
                                    );

                                    break;
                                }
                            }
                        }
                        _ => {
                            let msg = format!("`{:?}` is not a valid tuple field ", lit);

                            self.reporter.error(
                                msg,
                                "Tuple fields can only be numbers",
                                field.as_reporter_span(),
                            );

                            break;
                        }
                    }
                }

                _ => {
                    self.reporter
                        .error("Unknown field".to_string(), "", field.as_reporter_span());

                    break;
                }
            }
        }

        let ty = inferred_fields.last().unwrap().ty.clone();

        typed::Typed::new(typed::Expr::Field(inferred_fields), ty, span)
    }
}
