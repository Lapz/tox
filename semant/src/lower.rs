use crate::hir::Ctx;
use syntax::{ast, FnDefOwner};

fn lower(source: ast::SourceFile) {
    let mut ctx = Ctx::new();

    for function in source.functions() {
        let function_id = ctx.add_function(function);
    }
}
