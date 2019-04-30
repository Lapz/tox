use crate::ast as t;
use crate::ctx::CompileCtx;
use crate::infer::types;
use crate::infer::{Infer, InferResult};
use std::collections::HashMap;
use syntax::ast::Expression;
use util::pos::{Span, Spanned};
use util::symbol::Symbol;

impl Infer {
    pub(crate) fn infer_variant(
        &mut self,
        enum_name: Spanned<Symbol>,
        variant: Spanned<Symbol>,
        inner: Option<Box<Spanned<Expression>>>,
        whole_span: Span,
        ctx: &mut CompileCtx,
    ) -> InferResult<Spanned<t::TypedExpression>> {
        let ty = self.infer_symbol_type(&enum_name, ctx)?;

        match ty {
            types::Type::Generic(ref typevars, ref ty) => match **ty {
                types::Type::Enum {
                    ref name,
                    ref variants,
                } => {
                    if *name != enum_name.value {
                        let msg = format!(
                            "Expected `{}` enum found `{}` enum ",
                            ctx.name(*name),
                            ctx.name(enum_name.value)
                        );
                        ctx.error(msg, whole_span);
                        return Err(());
                    }

                    let variant_ty = variants.get(&variant.value);

                    if variant_ty.is_none() {
                        let msg = format!("Unknown enum variant `{}`", ctx.name(variant.value));
                        ctx.error(msg, variant.span);
                        return Err(());
                    }

                    let variant_ty = variant_ty.unwrap();

                    if variant_ty.inner.is_none() && inner.is_some() {
                        let msg = format!(
                            "The variant `{}` dosen't store any name",
                            ctx.name(variant.value)
                        );
                        ctx.error(msg, variant.span);
                        Err(())
                    } else if variant_ty.inner.is_some() && inner.is_none() {
                        let msg = format!(
                            "The variant `{}` should store the data of type {} ",
                            ctx.name(variant.value),
                            variant_ty.inner.as_ref().unwrap().print(ctx.symbols())
                        );
                        ctx.error(msg, variant.span);
                        Err(())
                    } else if variant_ty.inner.is_some() && inner.is_some() {
                        let inner = inner.unwrap();
                        let span = inner.span;
                        let inner_ty = self.infer_expr(*inner, ctx)?;

                        let mut mappings = HashMap::new();

                        mappings.insert(
                            typevars[(variant_ty.tag) as usize],
                            inner_ty.value.ty.clone(),
                        );

                        self.unify(
                            &self.subst(variant_ty.inner.as_ref().unwrap(), &mut mappings),
                            &self.subst(&inner_ty.value.ty, &mut mappings),
                            span,
                            ctx,
                        )?;

                        let expr = Spanned::new(
                            t::Expression::VariantWithData {
                                enum_name,
                                tag: variant_ty.tag,
                                inner: inner_ty,
                            },
                            whole_span,
                        );

                        Ok(Spanned::new(
                            t::TypedExpression {
                                expr: Box::new(expr),
                                ty: *ty.clone(),
                            },
                            whole_span,
                        ))
                    } else {
                        let expr = Spanned::new(
                            t::Expression::VariantNoData {
                                enum_name,
                                tag: variant_ty.tag,
                            },
                            whole_span,
                        );

                        Ok(Spanned::new(
                            t::TypedExpression {
                                expr: Box::new(expr),
                                ty: *ty.clone(),
                            },
                            whole_span,
                        ))
                    }
                }

                _ => {
                    let msg = format!("`{}` is not an enum", ctx.name(enum_name.value));

                    ctx.error(msg, enum_name.span);
                    Err(())
                }
            },

            _ => {
                let msg = format!("`{}` is not an enum", ctx.name(enum_name.value));

                ctx.error(msg, enum_name.span);
                Err(())
            }
        }
    }
}
