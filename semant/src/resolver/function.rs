mod expression;

use super::{data::ResolverDataCollector, TypeKind};
use crate::{
    hir::{Function, FunctionAstMap, NameId, StmtId},
    infer::{Type, TypeCon},
    util, HirDatabase,
};

impl<'a, DB> ResolverDataCollector<&'a DB>
where
    DB: HirDatabase,
{
    pub fn resolve_function_signature(&mut self, function: &Function) -> Result<Type, ()> {
        let mut poly_tvs = Vec::new();

        for type_param in &function.type_params {
            let type_param = function.ast_map.type_param(&type_param.item);

            let tv = self.ctx.type_var();

            self.insert_type(&type_param.name, Type::Var(tv), TypeKind::Type);

            poly_tvs.push(tv);
        }

        let mut signature = Vec::new();

        for param in &function.params {
            let param = function.ast_map.param(&param.item);

            self.resolve_pattern(function.name.item, &param.pat, &function.ast_map)?;

            signature.push(self.resolve_type(&param.ty));
        }

        if let Some(returns) = &function.returns {
            signature.push(self.resolve_type(&returns))
        } else {
            signature.push(Type::Con(TypeCon::Void))
        }

        Ok(Type::Poly(poly_tvs, Box::new(Type::App(signature))))
    }

    pub fn resolve_function(&mut self, function: &Function) -> Result<(), ()> {
        let name = function.name;

        self.begin_scope();

        let signature = self.resolve_function_signature(function)?;

        self.begin_function_scope(name.item);

        if let Some(body) = &function.body {
            for stmt in body {
                self.resolve_statement(&function.name, stmt, &function.ast_map)?;
            }
        }

        self.end_function_scope(name.item);

        self.end_scope();

        self.insert_type(&name, signature, TypeKind::Function);

        Ok(())
    }

    pub(crate) fn resolve_statement(
        &mut self,
        fn_name: &util::Span<NameId>,
        stmt: &util::Span<StmtId>,
        ast_map: &FunctionAstMap,
    ) -> Result<(), ()> {
        let stmt = ast_map.stmt(&stmt.item);

        match stmt {
            crate::hir::Stmt::Let {
                pat,

                initializer,
                ascribed_type,
            } => {
                self.resolve_pattern(fn_name.item, pat, ast_map)?;

                if let Some(expr) = initializer {
                    self.resolve_expression(fn_name, expr, ast_map)?;
                }

                if let Some(ascribed_type) = ascribed_type {
                    let _ = self.resolve_type(ascribed_type);
                }
            }
            crate::hir::Stmt::Expr(expr) => self.resolve_expression(fn_name, expr, ast_map)?,
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::create_test;

    create_test!(import_fn_as_type, is_err);
}
