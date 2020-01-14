use crate::db::HirDatabase;
use crate::hir;
use crate::ty;
use syntax::{
    ast, children, AstNode, FnDefOwner, NameOwner, TypeAscriptionOwner, TypeParamsOwner, TypesOwner,
};

pub fn infer_alias(db: &impl HirDatabase, alias: ast::TypeAliasDef) -> hir::TypeAliasId {
    if let Some(type_param_list) = alias.type_param_list() {
        let mut poly_tvs = Vec::new();

        for type_param in type_param_list.type_params() {
            let tv = ty::TypeVar::new();
            let ty = ty::Ty::Var(tv);
            poly_tvs.push(tv);
        }

        // ty::Ty::Generic(poly_tvs, db.infer_ty())
    }
    // else {
    // }

    unimplemented!()
}
