use crate::hir::{self, Ctx};
use std::collections::HashMap;
use syntax::{ast, AstNode, FnDefOwner, NameOwner, TypeAscriptionOwner};
// pub struct FunctionData {
//     names: HashMap<NameId, hir::Name>,
//     patterns: HashMap<PatternId, hir::Pattern>,
//     expressions: HashMap<ExprId, hir::Expression>,
// }

#[derive(Debug, Clone, Default)]
pub(crate) struct FunctionDataCollector {
    name_id_count: u32,
    pattern_id_count: u32,
    expr_id_count: u32,
    patterns: HashMap<hir::PatId, hir::Pattern>,
    // expressions: HashMap<hir::ExprId, hir::Expression>,
}

impl FunctionDataCollector {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_pattern(&mut self, pat: hir::Pattern) -> hir::PatId {
        let id = hir::PatId(self.pattern_id_count);

        self.pattern_id_count += 1;

        self.patterns.insert(id, pat);
        id
    }

    pub(crate) fn lower_pattern(&mut self, pat: ast::Pat) -> hir::PatId {
        let pattern = match &pat {
            ast::Pat::BindPat(binding) => {
                let name: crate::hir::Name = binding
                    .name()
                    .map(|n| n.into())
                    .unwrap_or(crate::hir::Name::missing());
                crate::hir::Pattern::Bind { name }
            }
            ast::Pat::PlaceholderPat(_) => crate::hir::Pattern::Placeholder,
            ast::Pat::TuplePat(variants) => crate::hir::Pattern::Tuple(
                variants
                    .args()
                    .map(|pat| self.lower_pattern(pat))
                    .collect::<Vec<_>>(),
            ),
            ast::Pat::LiteralPat(literal) => unimplemented!(),
        };

        self.add_pattern(pattern)
    }
}

pub fn lower_ast(source: ast::SourceFile) {
    let mut ctx = Ctx::new();

    for function in source.functions() {
        let mut collector = FunctionDataCollector::new();
        let name: Option<crate::hir::Name> = function.name().map(|name| name.into());
        // let mut params = Vec::new();
        if let Some(param_list) = function.param_list() {
            for param in param_list.params() {
                if let Some(p) = param.pat() {
                    collector.lower_pattern(p);
                }
            }
        }

        println!("{:#?}", collector);
    }
}

pub(crate) fn lower_param(param: ast::Param) {
    let pat = param.pat();

    let ty = param.ascribed_type();

    println!(
        "{:?} {:?}",
        pat.map(|p| p.syntax().text()),
        ty.map(|p| p.syntax().text()),
    );
}

pub(crate) fn lower_pat(pat: ast::Pat) {}
