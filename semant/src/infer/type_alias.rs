// use crate::db::HirDatabase;
// use crate::hir::{self, Ctx};
// use std::collections::HashMap;
// use syntax::Ty::*;
// use syntax::{
//     ast, child, children, AstNode, FnDefOwner, NameOwner, TypeAscriptionOwner, TypeParamsOwner,
//     TypesOwner,
// };

// pub fn infer_alias(db: &impl HirDatabase, alias: ast::TypeAliasDef) -> hir::TypeAliasId {
//     if let Some(type_param_list) = alias.type_param_list() {
//         let mut poly_tvs = Vec::new();

//         for type_param in type_param_list.type_params() {
//             let tv = TypeVar::new();
//             poly_tvs.push(tv);

//             // db.set_
//         }
//     }
//     // else {
//     // }

//     unimplemented!()
// }

fn foo() {
    a + b;
}
