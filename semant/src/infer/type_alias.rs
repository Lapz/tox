use crate::db::HirDatabase;
use crate::hir;
use crate::ty;
use std::sync::Arc;

pub fn infer_alias(db: &impl HirDatabase, alias: hir::TypeAlias) -> Arc<Result<(), ()>> {
    let mut poly_tvs = Vec::new();

    for _type_param in &alias.type_params {
        let tv = db.ctx_mut().tv();
        let _ty = ty::Ty::Var(tv);

        // let name = db.lookup(alias).get_map();

        // db.ctx_mut().add_type(type_param, ty);
        poly_tvs.push(tv);
    }

    unimplemented!()
}
