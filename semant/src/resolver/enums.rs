use super::{data::ResolverDataCollector, TypeKind};
use crate::{
    hir::Enum,
    infer::{Type, Variant},
    HirDatabase,
};
use std::collections::HashMap;

impl<'a, DB> ResolverDataCollector<&'a DB>
where
    DB: HirDatabase,
{
    pub fn resolve_enum(&mut self, enum_def: &Enum) -> Result<(), ()> {
        self.begin_scope();
        let mut poly_tvs = Vec::new();

        for type_param in &enum_def.type_params {
            let type_param = enum_def.ast_map.type_param(&type_param.item);

            let tv = self.ctx.type_var();

            self.insert_type(&type_param.name, Type::Var(tv), TypeKind::Type);

            poly_tvs.push(tv);
        }

        let mut variants = HashMap::new();

        for (tag, variant) in enum_def.variants.iter().enumerate() {
            if variants.contains_key(&variant.item.name) {
                let msg = format!(
                    "Duplicate enum variant `{}`",
                    self.db.lookup_intern_name(variant.item.name)
                );

                let span = variant.as_reporter_span();

                self.reporter.error(msg, "", span);

                continue;
            }

            let ty = if let Some(variant_ty) = variant.item.ty {
                Some(self.resolve_type(&variant_ty))
            } else {
                None
            };

            variants.insert(variant.item.name, Variant { tag, ty });
        }

        self.end_scope();

        self.insert_type(
            &enum_def.name,
            Type::Poly(poly_tvs, Box::new(Type::Enum(enum_def.name.item, variants))),
            TypeKind::Enum,
        );

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::create_test;

    create_test!(basic_enum);

    create_test!(enum_dup_variant, is_err);

    create_test!(recursive_enum, is_err);
}
