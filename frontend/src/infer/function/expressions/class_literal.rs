use ast as t;
use ctx::CompileCtx;
use infer::env::VarEntry;
use infer::types::{self, Property, TypeCon, TypeVar};
use infer::{Infer, InferResult};
use std::collections::HashMap;
use syntax::ast::{
    AssignOperator, ClassLiteralField, Expression, Function, Literal, Op, Type, UnaryOp,
};
use util::pos::{Span, Spanned};
use util::symbol::Symbol;

impl Infer {
    pub(crate) fn infer_class_literal(
        &mut self,
        symbol: Spanned<Symbol>,
        props: Vec<Spanned<ClassLiteralField>>,
        whole_span: Span,
        ctx: &mut CompileCtx,
    ) -> InferResult<Spanned<t::TypedExpression>> {
        let class_type = if let Some(ty) = ctx.look_type(symbol.value).cloned() {
            ty
        } else {
            let msg = format!("Undefined class `{}`", ctx.name(symbol.value));
            ctx.error(msg, symbol.span);
            return Err(());
        };

        match class_type {
            types::Type::Generic(ref typevars, ref ty) => match **ty {
                types::Type::Class(_, ref properties, ref methods, ref unique) => {
                    let mut mappings = HashMap::new();
                    let mut type_lit_expressions = Vec::new();
                    let mut types = Vec::new();
                    let mut unknown = false;

                    for (def_property, property) in properties.iter().zip(props.into_iter()) {
                        if def_property.name == property.value.symbol.value {
                            type_lit_expressions.push(Spanned {
                                value: t::ClassLiteralProperty {
                                    name: property.value.symbol.value,
                                    expr: self.infer_expr(property.value.expr, ctx)?,
                                },
                                span: property.span,
                            });
                        } else {
                            unknown = true;

                            let msg = format!(
                                "`{}` is not a member of `{}`",
                                ctx.name(property.value.symbol.value),
                                ctx.name(symbol.value)
                            );

                            ctx.error(msg, property.span)
                        }
                    }

                    for (type_var, literal_expression) in
                        typevars.iter().zip(type_lit_expressions.iter())
                    {
                        mappings.insert(*type_var, literal_expression.value.expr.value.ty.clone());
                    }

                    for (mut literal_expression, def_property) in
                        type_lit_expressions.iter_mut().zip(properties)
                    {
                        self.unify(
                            &self.subst(&def_property.ty, &mut mappings),
                            &self.subst(&literal_expression.value.expr.value.ty, &mut mappings),
                            literal_expression.value.expr.span,
                            ctx,
                        )?;

                        let ty = self.subst(&literal_expression.value.expr.value.ty, &mut mappings);

                        literal_expression.value.expr.value.ty = ty.clone(); // change the type so its the proper type

                        types.push(types::Property {
                            name: literal_expression.value.name,
                            ty,
                        });
                    }

                    if properties.len() > type_lit_expressions.len() {
                        let msg = format!("class `{}` is missing fields", ctx.name(symbol.value));
                        ctx.error(msg, whole_span);
                    } else if properties.len() < type_lit_expressions.len() {
                        let msg = format!("class `{}` has too many fields", ctx.name(symbol.value));
                        ctx.error(msg, whole_span);
                    } else if unknown {
                        // Encountered an unknown field
                        return Err(());
                    }

                    Ok(Spanned {
                        value: t::TypedExpression {
                            expr: Box::new(Spanned {
                                value: t::Expression::ClassLiteral {
                                    symbol: symbol.value,
                                    properties: type_lit_expressions,
                                },
                                span: whole_span,
                            }),
                            ty: types::Type::Class(symbol.value, types, methods.clone(), *unique),
                        },
                        span: whole_span,
                    })
                }
                _ => unreachable!(),
            },
            _ => {
                let msg = format!("`{}` is not a class", ctx.name(symbol.value));

                ctx.error(msg, symbol.span);
                Err(())
            }
        }
    }

    pub(crate) fn infer_class_instantiated_literal(
        &mut self,
        symbol: Spanned<Symbol>,
        props: Vec<Spanned<ClassLiteralField>>,
        types: Spanned<Vec<Spanned<Type>>>,
        whole_span: Span,
        ctx: &mut CompileCtx,
    ) -> InferResult<Spanned<t::TypedExpression>> {
        let class_type = if let Some(ty) = ctx.look_type(symbol.value).cloned() {
            ty
        } else {
            let msg = format!("Undefined class `{}`", ctx.name(symbol.value));
            ctx.error(msg, symbol.span);
            return Err(());
        };

        match class_type {
            types::Type::Generic(ref typevars, ref ty) => match **ty {
                types::Type::Class(_, ref properties, ref methods, ref unique) => {
                    if typevars.len() != types.value.len() {
                        let msg = format!(
                            "Found `{}` type params expected `{}`",
                            types.value.len(),
                            typevars.len()
                        );

                        ctx.error(msg, types.span);

                        return Err(());
                    }

                    let mut mappings = HashMap::new();

                    let mut type_lit_expressions = Vec::new();
                    let mut literal_types = Vec::new();
                    let mut unknown = false;

                    for (ty, type_var) in types.value.into_iter().zip(typevars.iter()) {
                        mappings.insert(*type_var, self.trans_type(&ty, ctx)?);
                    }

                    for (def_property, property) in properties.iter().zip(props.into_iter()) {
                        if def_property.name == property.value.symbol.value {
                            type_lit_expressions.push(Spanned {
                                value: t::ClassLiteralProperty {
                                    name: property.value.symbol.value,
                                    expr: self.infer_expr(property.value.expr, ctx)?,
                                },
                                span: property.span,
                            });
                        } else {
                            unknown = true;

                            let msg = format!(
                                "`{}` is not a member of `{}`",
                                ctx.name(property.value.symbol.value),
                                ctx.name(symbol.value)
                            );

                            ctx.error(msg, property.span)
                        }
                    }

                    for (type_var, literal_expression) in
                        typevars.iter().zip(type_lit_expressions.iter())
                    {
                        mappings.insert(*type_var, literal_expression.value.expr.value.ty.clone());
                    }

                    for (mut literal_expression, def_property) in
                        type_lit_expressions.iter_mut().zip(properties)
                    {
                        self.unify(
                            &self.subst(&def_property.ty, &mut mappings),
                            &self.subst(&literal_expression.value.expr.value.ty, &mut mappings),
                            literal_expression.value.expr.span,
                            ctx,
                        )?;

                        let ty = self.subst(&literal_expression.value.expr.value.ty, &mut mappings);

                        literal_expression.value.expr.value.ty = ty.clone();

                        literal_types.push(types::Property {
                            name: literal_expression.value.name,
                            ty,
                        });
                    }

                    if properties.len() > type_lit_expressions.len() {
                        let msg = format!("class `{}` is missing fields", ctx.name(symbol.value));
                        ctx.error(msg, whole_span);
                    } else if properties.len() < type_lit_expressions.len() {
                        let msg = format!("class `{}` has too many fields", ctx.name(symbol.value));
                        ctx.error(msg, whole_span);
                    } else if unknown {
                        // Encountered an unknown field
                        return Err(());
                    }

                    Ok(Spanned {
                        value: t::TypedExpression {
                            expr: Box::new(Spanned {
                                value: t::Expression::ClassLiteral {
                                    symbol: symbol.value,
                                    properties: type_lit_expressions,
                                },
                                span: whole_span,
                            }),
                            ty: types::Type::Class(
                                symbol.value,
                                literal_types,
                                methods.clone(),
                                *unique,
                            ),
                        },
                        span: whole_span,
                    })
                }

                _ => unreachable!(), // for (typevar, literal_expression) in typevars.iter().zip(p)
            },
            _ => {
                let msg = format!("`{}` is not a class", ctx.name(symbol.value));

                ctx.error(msg, symbol.span);
                Err(())
            }
        }
    }
}
