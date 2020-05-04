use super::data::ResolverDataCollector;
use crate::{hir::TypeAlias, infer::Type, HirDatabase};

impl<'a, DB> ResolverDataCollector<&'a DB>
where
    DB: HirDatabase,
{
    pub(crate) fn resolve_alias(&mut self, alias: &TypeAlias) -> Result<(), ()> {
        let name = alias.name;

        self.begin_scope();

        let poly_tvs = alias
            .type_params
            .iter()
            .map(|type_param| {
                let type_param = alias.ast_map.type_param(&type_param.item);

                let tv = self.ctx.type_var();

                self.ctx.insert_type(type_param.name, Type::Var(tv));

                tv
            })
            .collect::<Vec<_>>();

        let ty = self.resolve_type(&alias.ty)?;

        self.end_scope();

        self.ctx
            .insert_type(name.item, Type::Poly(poly_tvs, Box::new(ty)));
        Ok(())
    }
}
