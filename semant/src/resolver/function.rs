mod expression;

use super::data::ResolverDataCollector;
use crate::{
    hir::{Function, FunctionAstMap, NameId, StmtId},
    infer::{Type, TypeCon},
    util, HirDatabase,
};

impl<'a, DB> ResolverDataCollector<&'a DB>
where
    DB: HirDatabase,
{
    pub fn resolve_function(&mut self, function: &Function) -> Result<(), ()> {
        let name = function.name;

        self.begin_scope();

        let poly_tvs = function
            .type_params
            .iter()
            .map(|type_param| {
                let type_param = function.ast_map.type_param(&type_param.item);

                let tv = self.ctx.type_var();

                self.ctx.insert_type(type_param.name, Type::Var(tv));

                tv
            })
            .collect::<Vec<_>>();

        let mut signature = Vec::new();

        for param in &function.params {
            let param = function.ast_map.param(&param.item);

            let _ = self.resolve_pattern(function.name.item, &param.pat, &function.ast_map);

            signature.push(self.resolve_type(&param.ty)?);
        }

        if let Some(returns) = &function.returns {
            signature.push(self.resolve_type(&returns)?)
        } else {
            signature.push(Type::Con(TypeCon::Void))
        }

        if let Some(body) = &function.body {
            for stmt in body {
                let _ = self.resolve_statement(&function.name, stmt, &function.ast_map);
            }
        }

        self.end_scope();

        self.ctx.insert_type(
            name.item,
            Type::Poly(poly_tvs, Box::new(Type::App(signature))),
        );

        Ok(())
    }

    pub(crate) fn resolve_statement(
        &mut self,
        fn_name: &util::Span<NameId>,
        stmt: &StmtId,
        ast_map: &FunctionAstMap,
    ) {
        let stmt = ast_map.stmt(stmt);

        match stmt {
            crate::hir::Stmt::Let {
                pat,
                initializer,
                ascribed_type,
            } => {
                let _ = self.resolve_pattern(fn_name.item, pat, ast_map);

                if let Some(expr) = initializer {
                    let _ = self.resolve_expression(fn_name, expr, ast_map);
                }

                if let Some(ascribed_type) = ascribed_type {
                    let _ = self.resolve_type(ascribed_type);
                }
            }
            crate::hir::Stmt::Expr(expr) => self.resolve_expression(fn_name, expr, ast_map),
        }
    }
}
