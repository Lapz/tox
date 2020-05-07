use super::data::{ResolverDataCollector, TypeKind};
use crate::{hir::TypeAlias, infer::Type, HirDatabase};

impl<'a, DB> ResolverDataCollector<&'a DB>
where
    DB: HirDatabase,
{
    pub(crate) fn resolve_alias(&mut self, alias: &TypeAlias) -> Result<(), ()> {
        let name = alias.name;

        let mut poly_tvs = Vec::new();

        self.begin_scope();

        for type_param in &alias.type_params {
            let type_param = alias.ast_map.type_param(&type_param.item);

            let tv = self.ctx.type_var();

            self.insert_type(&type_param.name, Type::Var(tv), TypeKind::Type)?;

            poly_tvs.push(tv)
        }

        let ty = self.resolve_type(&alias.ty)?;

        self.end_scope();

        self.insert_type(&name, Type::Poly(poly_tvs, Box::new(ty)), TypeKind::Alias)?;

        if alias.exported {
            self.exported_items.insert(alias.name.item);
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use crate::create_test;
    create_test!(import_alias);
}
