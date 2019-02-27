use ast as t;
use ctx::CompileCtx;
use infer::types;
use infer::{Infer, InferResult};
use std::collections::HashMap;
use syntax::ast::{Expression, UnaryOp};
use util::pos::{Span, Spanned};
use util::symbol::Symbol;

impl Infer {
    pub(crate) fn infer_constructor(
        &mut self,
        enum_name: Spanned<Symbol>,
        variant: Spanned<Symbol>,
        constructors: Vec<Spanned<Expression>>,
        whole_span: Span,
        ctx: &mut CompileCtx,
    ) -> InferResult<Spanned<t::TypedExpression>> {
        let ty = self.infer_symbol_type(&enum_name, ctx)?;

        match ty {
            types::Type::Generic(ref typevars, ref ty) => {
                match **ty {
                    types::Type::Enum {
                        ref name,
                        ref variants,
                    } => {
                        let mut con = variants.get(&variant.value);

                        if con.is_none() {
                            let msg = format!("Unknown enum variant `{}`", ctx.name(variant.value));
                            ctx.error(msg, variant.span);
                            return Err(());
                        }

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

                        let variant_ty = variant_ty.unwrap();

                        if constructors.len() != variant_ty.constructor.arity() {
                            let msg = format!("Expected `{}` number of constructor arguments for `{}` but found `{}`",variant_ty.constructor.arity(),ctx.name(variant.value),constructors.len());
                            ctx.error(msg, variant.span);
                            return Err(());
                        }

                        let mut mappings = HashMap::new();
                        let mut con_args = Vec::new();

                        for (expr, tvar) in constructors.into_iter().zip(typevars.iter()) {
                            let ty_expr = self.infer_expr(expr, ctx)?;

                            mappings.insert(*tvar, ty_expr.value.ty.clone());

                            con_args.push(ty_expr);
                        }

                        let constructor_types = variant_ty.constructor.types();

                        for (c_ty, expr_ty) in constructor_types.iter().zip(con_args.iter()) {
                            self.unify(
                                &self.subst(c_ty, &mut mappings),
                                &self.subst(&expr_ty.value.ty, &mut mappings),
                                expr_ty.span,
                                ctx,
                            )?
                        }

                        let expr = Spanned::new(
                            t::Expression::Constructor {
                                enum_name,
                                tag: variant_ty.tag,
                                args: con_args,
                            },
                            whole_span,
                        );

                        Ok(Spanned::new(
                            t::TypedExpression {
                                expr: Box::new(expr),

                                ty: self.subst(
                                    &ty,
                                    &mut mappings,
                                ),
                            },
                            whole_span,
                        ))
                    }

                    _ => {
                        let msg = format!("`{}` is not an enum", ctx.name(enum_name.value));

                        ctx.error(msg, enum_name.span);
                        Err(())
                    }
                }
            }

            _ => {
                let msg = format!("`{}` is not an enum", ctx.name(enum_name.value));

                ctx.error(msg, enum_name.span);
                Err(())
            }
        }

        // unimplemented!()
    }
}
