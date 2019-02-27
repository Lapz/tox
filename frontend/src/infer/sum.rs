use ctx::CompileCtx;

use infer::types::{CSpan, Constructor, PatternVar, Type, TypeVar, Variant};
use infer::{Infer, InferResult};
use std::collections::HashMap;
use syntax::ast::Enum;
use util::pos::Spanned;

impl Infer {
    pub fn infer_enum(&mut self, _enum: Spanned<Enum>, ctx: &mut CompileCtx) -> InferResult<()> {
        let mut generic_type_vars = Vec::with_capacity(_enum.value.name.value.type_params.len());
        let mut type_params = Vec::with_capacity(_enum.value.name.value.type_params.len());

        ctx.begin_scope();

        for symbol in _enum.value.name.value.type_params.iter() {
            let type_var = TypeVar::new();
            ctx.add_type(symbol.value, Type::Var(type_var));
            generic_type_vars.push(type_var);
            type_params.push(symbol.value)
        }

        ctx.add_type(
            _enum.value.name.value.name.value,
            Type::Generic(
                generic_type_vars.clone(),
                Box::new(Type::Enum {
                    name: _enum.value.name.value.name.value,
                    variants: HashMap::new(),
                }),
            ),
        ); // For recursive types we need to add the empty enum

        let mut variants = HashMap::with_capacity(_enum.value.variants.len());

        let arity = _enum.value.variants.len(); // the arity is the same as the number of variants defined on the enum

        for (i, variant) in _enum.value.variants.into_iter().enumerate() {
            let mut args = vec![];

            for arg in variant.constructor.iter() {
                args.push(self.trans_type(arg, ctx)?);
            }

            let span = CSpan::Range(args.len());

            let v = Variant {
                tag: i as u32,
                constructor: Constructor::new(variant.name.value, args, arity, span),
            };

            variants.insert(variant.name.value, v);
        }

        ctx.add_pattern_type(
            PatternVar::new(),
            Type::Enum {
                name: _enum.value.name.value.name.value,
                variants: variants.clone(),
            },
        );

        ctx.add_constructor(
            Type::Enum {
                name: _enum.value.name.value.name.value,
                variants: variants.clone(),
            },
            variants
                .iter()
                .map(|(_, v)| v.constructor.clone())
                .collect(),
        );

        ctx.end_scope();

        ctx.add_type(
            _enum.value.name.value.name.value,
            Type::Generic(
                generic_type_vars.clone(),
                Box::new(Type::Enum {
                    name: _enum.value.name.value.name.value,
                    variants,
                }),
            ),
        ); // Add the proper fully type checked enum

        Ok(())
    }
}
