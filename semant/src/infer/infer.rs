use super::StackedMap;
use crate::infer::{self, Type, TypeCon};
use crate::resolver::Resolver;
use crate::{
    hir::{self, ExprId, Function, FunctionAstMap, NameId, StmtId},
    util, HirDatabase,
};
use errors::{FileId, Reporter, WithError};
use std::{collections::HashMap, sync::Arc};

#[derive(Debug)]
pub(crate) struct InferDataCollector<DB> {
    pub(crate) db: DB,
    pub(crate) resolver: Arc<Resolver>,
    pub(crate) reporter: Reporter,
    types: StackedMap<NameId, Type>,
    return_ty: Option<Type>,
    tvar_count: u32,
}

impl<'a, DB> InferDataCollector<&'a DB>
where
    DB: HirDatabase,
{
    pub(crate) fn type_var(&mut self) -> infer::TypeVar {
        let tv = infer::TypeVar::from(self.tvar_count);
        self.tvar_count += 1;

        tv
    }
    fn infer_type(&mut self, id: &util::Span<hir::TypeId>) -> infer::Type {
        let ty = self.db.lookup_intern_type(id.item);

        match ty {
            hir::Type::ParenType(types) => {
                let mut signature = vec![];

                for id in &types {
                    signature.push(self.infer_type(id))
                }

                infer::Type::Tuple(signature)
            }
            hir::Type::ArrayType { ty, size } => Type::Con(TypeCon::Array {
                ty: Box::new(self.infer_type(&ty)),
                size,
            }),
            hir::Type::FnType { params, ret } => {
                let mut signature = vec![];

                for id in &params {
                    signature.push(self.infer_type(id))
                }

                if let Some(returns) = &ret {
                    signature.push(self.infer_type(returns))
                } else {
                    signature.push(Type::Con(TypeCon::Void))
                }

                Type::App(signature)
            }
            hir::Type::Poly { name, type_args } => {
                let ty = self.resolver.ctx.get_type(&name).unwrap();

                match ty {
                    Type::Poly(type_vars, inner) => {
                        let mut substitutions = HashMap::new();

                        for (type_var, type_arg) in type_vars.iter().zip(type_args) {
                            substitutions.insert(*type_var, self.infer_type(&type_arg));
                        }
                        match &*inner {
                            ty @ Type::Enum(_, _) => self.subst(&ty, &mut substitutions),
                            ty @ Type::Class { .. } => self.subst(&ty, &mut substitutions),
                            _ => unreachable!(),
                        }
                    }

                    _ => unreachable!(),
                }
            }
            hir::Type::Ident(name) => self.resolver.ctx.get_type(&name).unwrap(),
        }
    }

    fn infer_expr(&mut self, expr: &ExprId, ast_map: &FunctionAstMap) -> Type {
        let expr = ast_map.expr(expr);
        match expr {
            crate::hir::Expr::Array(_) => {}
            crate::hir::Expr::Binary { lhs, op, rhs } => {}
            crate::hir::Expr::Block(_) => {}
            crate::hir::Expr::Break => {}
            crate::hir::Expr::Call {
                callee,
                args,
                type_args,
            } => {}
            crate::hir::Expr::Cast { expr, ty } => {}
            crate::hir::Expr::Continue => {}
            crate::hir::Expr::If {
                cond,
                then_branch,
                else_branch,
            } => {}
            crate::hir::Expr::Ident(_) => {}
            crate::hir::Expr::Index { base, index } => {}
            crate::hir::Expr::While { cond, body } => {}
            crate::hir::Expr::Literal(_) => {}
            crate::hir::Expr::Paren(_) => {}
            crate::hir::Expr::Tuple(_) => {}
            crate::hir::Expr::Unary { op, expr } => {}
            crate::hir::Expr::Return(_) => {}
            crate::hir::Expr::Match { expr, arms } => {}
            crate::hir::Expr::Enum { def, variant, expr } => {}
            crate::hir::Expr::RecordLiteral { def, fields } => {}
        }

        unimplemented!()
    }

    fn infer_statement(&mut self, stmt: &StmtId, ast_map: &FunctionAstMap) -> Result<(), ()> {
        let stmt = ast_map.stmt(stmt);

        match stmt {
            crate::hir::Stmt::Let {
                pat,
                initializer,
                ascribed_type,
            } => match (initializer, ascribed_type) {
                (None, None) => unimplemented!(),
                (None, Some(ty)) => {
                    let ty = self.infer_type(ty);

                    unimplemented!()
                }
                (Some(expr), None) => {
                    let expr_ty = self.infer_expr(expr, ast_map);

                    unimplemented!()
                }
                (Some(expr), Some(ascribed_type)) => {
                    let expected = self.infer_type(ascribed_type);
                    let expr_ty = self.infer_expr(expr, ast_map);

                    // self.unify(&expected, &expr_ty, stmt, unimplemented!())

                    unimplemented!()
                }
            },

            crate::hir::Stmt::Expr(expr) => {
                self.infer_expr(expr, ast_map);
                unimplemented!()
            }
        }
    }

    fn infer_function(&mut self, function: &Function) {
        let name = function.name.item;

        let expected = self.resolver.ctx.get_type(&name).unwrap();

        self.return_ty = Some(expected.clone());

        if let Some(body) = &function.body {
            for stmt in body {
                let _ = self.infer_statement(stmt, &function.ast_map);
            }
        } else {
            // self.unify(&expected, &Type::Con(TypeCon::Void));
        };
    }
}

pub fn infer_query(db: &impl HirDatabase, file: FileId) -> WithError<()> {
    let program = db.lower(file)?;
    let resolver = db.resolve_source_file(file).unwrap();

    let mut collector = InferDataCollector {
        db,
        tvar_count: resolver.ctx.tvar_count,
        reporter: resolver.reporter.clone(),
        resolver,
        types: StackedMap::new(),
        return_ty: None,
    };

    for function in &program.functions {
        collector.infer_function(function);
        //
    }

    Ok(())
}
