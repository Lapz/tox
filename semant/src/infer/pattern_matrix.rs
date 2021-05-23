// mod matrix;
// mod pattern;

// use crate::{
//     hir::{self, FunctionAstMap, Literal},
//     infer::InferDataCollector,
//     HirDatabase,
// };
// pub use matrix::{PatternMatrix, Row};
// pub use pattern::{wcard, Constructor, Pattern};

// impl<'a, DB> InferDataCollector<&'a DB>
// where
//     DB: HirDatabase,
// {
//     pub(crate) fn to_matrix_pattern(
//         &self,
//         pattern: &hir::Pattern,
//         map: &FunctionAstMap,
//     ) -> Pattern {
//         match pattern {
//             hir::Pattern::Bind { .. } => wcard(),
//             hir::Pattern::Placeholder => wcard(),
//             hir::Pattern::Tuple(pats) => Pattern::Con(
//                 Constructor {
//                     name: "Tuple".into(),
//                     arity: pats.len(),
//                     span: 0,
//                 },
//                 pats.iter()
//                     .map(|id| {
//                         let pat = map.pat(&id.item);
//                         self.to_matrix_pattern(pat, map)
//                     })
//                     .collect(),
//             ),
//             hir::Pattern::Literal(lit_id) => {
//                 let lit = self.db.lookup_intern_literal(*lit_id);

//                 Pattern::Con(
//                     match lit {
//                         Literal::String(_) => Constructor {
//                             name: "String".into(),
//                             arity: 0,
//                             span: usize::MAX,
//                         },
//                         Literal::Nil => Constructor {
//                             name: "Nil".into(),
//                             arity: 0,
//                             span: 1,
//                         },
//                         Literal::True => Constructor {
//                             name: "true".into(),
//                             arity: 0,
//                             span: 2,
//                         },
//                         Literal::False => Constructor {
//                             name: "false".into(),
//                             arity: 0,
//                             span: 2,
//                         },
//                         Literal::Int(_) => Constructor {
//                             name: "true".into(),
//                             arity: 0,
//                             span: i32::MAX as usize,
//                         },
//                         Literal::Float(_) => Constructor {
//                             name: "f32".into(),
//                             arity: 0,
//                             span: f32::MAX as usize,
//                         },
//                     },
//                     vec![],
//                 );

//                 unimplemented!()
//             }
//         }
//     }
// }
