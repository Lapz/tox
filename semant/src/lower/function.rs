use crate::db::HirDatabase;
use crate::{hir, impl_collector, util, TextRange};

use std::sync::Arc;

use syntax::{
    ast, ArgListOwner, AstNode, LoopBodyOwner, NameOwner, TypeAscriptionOwner, TypeParamsOwner,
    TypesOwner, VisibilityOwner,
};

#[derive(Debug)]
pub(crate) struct FunctionDataCollector<DB> {
    db: DB,
    type_param_count: u64,
    param_id_count: u64,
    stmt_id_count: u64,
    expr_id_count: u64,
    block_id_count: u64,
    pat_id_count: u64,
    ast_map: hir::FunctionAstMap,
    params: Vec<util::Span<hir::ParamId>>, // expressions: HashMap<hir::ExprId, hir::Expr>,
    type_params: Vec<util::Span<hir::TypeParamId>>,
}

impl_collector!(FunctionDataCollector);

impl<'a, DB> FunctionDataCollector<&'a DB>
where
    DB: HirDatabase,
{
    pub fn finish(
        self,
        exported: bool,
        name: util::Span<hir::NameId>,
        body: Option<Vec<util::Span<hir::StmtId>>>,
        returns: Option<util::Span<hir::TypeId>>,
        span: TextRange,
    ) -> hir::Function {
        let params = self.params;
        let type_params = self.type_params;
        let ast_map = self.ast_map;
        hir::Function {
            exported,
            name,
            ast_map,
            params,
            type_params,
            returns,
            body,
            span,
        }
    }

    pub fn add_param(&mut self, ast_node: &ast::Param, param: hir::Param) {
        let current = self.param_id_count;

        self.param_id_count += 1;

        let id = hir::ParamId(current);

        self.ast_map.insert_param(id, param);

        self.params.push(util::Span::from_ast(id, ast_node));
    }

    fn add_pat(&mut self, ast_node: &ast::Pat, pat: hir::Pattern) -> util::Span<hir::PatId> {
        let current = self.pat_id_count;
        self.pat_id_count += 1;
        let id = hir::PatId(current);
        self.ast_map.insert_pat(id, pat);

        util::Span::from_ast(id, ast_node)
    }

    pub fn add_stmt(&mut self, stmt: hir::Stmt) -> hir::StmtId {
        let current = self.stmt_id_count;

        self.stmt_id_count += 1;

        let id = hir::StmtId(current);

        self.ast_map.insert_stmt(id, stmt);

        id
    }

    pub fn expr_to_stmt(&mut self, expr: util::Span<hir::ExprId>) -> util::Span<hir::StmtId> {
        let current = self.stmt_id_count;

        self.stmt_id_count += 1;

        let id = hir::StmtId(current);

        self.ast_map.insert_stmt(id, hir::Stmt::Expr(expr));

        util::Span::new(id, expr.start, expr.end)
    }

    pub fn add_expr(&mut self, expr: hir::Expr) -> hir::ExprId {
        let current = self.expr_id_count;

        self.expr_id_count += 1;

        let id = hir::ExprId(current);

        self.ast_map.insert_expr(id, expr);

        id
    }

    pub fn add_block(&mut self, block: hir::Block) -> hir::BlockId {
        let current = self.block_id_count;

        self.block_id_count += 1;

        let id = hir::BlockId(current);

        self.ast_map.insert_block(id, block);

        id
    }

    pub(crate) fn lower_pattern(&mut self, pat: ast::Pat) -> util::Span<hir::PatId> {
        let pattern = match &pat {
            ast::Pat::BindPat(binding) => {
                let name = self.db.intern_name(
                    binding
                        .name()
                        .map(|n| n.into())
                        .unwrap_or_else(crate::hir::Name::missing),
                );

                let name = util::Span::from_ast(name, &binding.name().unwrap());
                crate::hir::Pattern::Bind { name }
            }
            ast::Pat::PlaceholderPat(_) => crate::hir::Pattern::Placeholder,
            ast::Pat::TuplePat(variants) => crate::hir::Pattern::Tuple(
                variants
                    .args()
                    .map(|pat| self.lower_pattern(pat))
                    .collect::<Vec<_>>(),
            ),
            ast::Pat::LiteralPat(literal) => crate::hir::Pattern::Literal(self.db.intern_literal(
                hir::Literal::from_token(literal.literal().unwrap().token_kind()),
            )),
        };

        self.add_pat(&pat, pattern)
    }

    pub(crate) fn lower_param(&mut self, param: ast::Param) {
        let pat = self.lower_pattern(param.pat().unwrap());

        let ty = self.lower_type(param.ascribed_type().unwrap());

        self.add_param(&param, hir::Param { pat, ty });
    }

    pub fn lower_stmt(&mut self, node: ast::Stmt) -> util::Span<hir::StmtId> {
        let hir_stmt = match node {
            ast::Stmt::LetStmt(ref let_stmt) => {
                let pat = self.lower_pattern(let_stmt.pat().unwrap());

                let initializer = if let Some(initializer) = let_stmt.initializer() {
                    Some(self.lower_expr(initializer))
                } else {
                    None
                };

                let ascribed_type = if let Some(ascribed_type) = let_stmt.ascribed_type() {
                    Some(self.lower_type(ascribed_type))
                } else {
                    None
                };

                hir::Stmt::Let {
                    pat,
                    initializer,
                    ascribed_type,
                }
            }
            ast::Stmt::ExprStmt(ref expr_stmt) => {
                hir::Stmt::Expr(self.lower_expr(expr_stmt.expr().unwrap()))
            }
        };

        util::Span::from_ast(self.add_stmt(hir_stmt), &node)
    }

    pub fn lower_expr(&mut self, node: ast::Expr) -> util::Span<hir::ExprId> {
        let hir_expr = match node {
            ast::Expr::ArrayExpr(ref array) => {
                hir::Expr::Array(array.exprs().map(|expr| self.lower_expr(expr)).collect())
            }
            ast::Expr::BinExpr(ref bin_expr) => {
                let lhs = self.lower_expr(bin_expr.lhs().unwrap());
                let rhs = self.lower_expr(bin_expr.rhs().unwrap());

                let op = hir::BinOp::from_kind(bin_expr.op_kind().unwrap()).unwrap(); // TODO fix the unwraps

                hir::Expr::Binary { lhs, op, rhs }
            }
            ast::Expr::BlockExpr(ref block) => {
                println!("{:?}", block.is_expr());

                let block = hir::Block(
                    block
                        .block()
                        .unwrap()
                        .statements()
                        .map(|st| self.lower_stmt(st))
                        .collect(),
                );

                hir::Expr::Block(self.add_block(block))
            }

            ast::Expr::BreakExpr(_) => hir::Expr::Break,
            ast::Expr::CallExpr(ref call_expr) => {
                let callee = self.lower_expr(call_expr.expr().unwrap());
                let args = if let Some(arg_list) = call_expr.arg_list() {
                    arg_list.args().map(|arg| self.lower_expr(arg)).collect()
                } else {
                    Vec::new()
                };

                let type_args = if let Some(type_args) = call_expr.type_args() {
                    util::Span::from_ast(
                        type_args
                            .types()
                            .map(|ty| self.lower_type(ty))
                            .collect::<Vec<_>>(),
                        &type_args,
                    )
                } else {
                    util::Span::from_ast(Vec::new(), &call_expr.expr().unwrap())
                };

                hir::Expr::Call {
                    callee,
                    args,
                    type_args,
                }
            }
            ast::Expr::CastExpr(ref cast_expr) => {
                let ty = self.lower_type(cast_expr.type_ref().unwrap());
                let expr = self.lower_expr(cast_expr.expr().unwrap());

                hir::Expr::Cast { expr, ty }
            }
            ast::Expr::RecordLiteralExpr(ref record_lit) => {
                let def = util::Span::from_ast(
                    self.db
                        .intern_name(record_lit.ident().unwrap().name().unwrap().into()),
                    &record_lit.ident().unwrap(),
                );

                let mut fields = Vec::new();

                let fields_iter = record_lit.named_field_list().unwrap().fields();

                for field in fields_iter {
                    let name = util::Span::from_ast(
                        self.db.intern_name(field.name().unwrap().into()),
                        &field.name().unwrap(),
                    );

                    let expr = self.lower_expr(field.expr().unwrap());

                    fields.push((name, expr));
                }

                hir::Expr::RecordLiteral { def, fields }
            }
            ast::Expr::ClosureExpr(ref _closure_expr) => {
                // let args = closure_expr.a

                unimplemented!()
            }
            ast::Expr::ContinueExpr(_) => hir::Expr::Continue,
            ast::Expr::FieldExpr(ref _field_expr) => unimplemented!(),
            ast::Expr::ForExpr(ref for_expr) => {
                let init = self.lower_stmt(for_expr.init().unwrap());
                let cond = self.lower_expr(for_expr.cond().unwrap());
                let increment = self.lower_expr(for_expr.increment().unwrap());

                let loop_body = for_expr.loop_body().unwrap().block().unwrap();
                let mut body = loop_body
                    .statements()
                    .map(|st| self.lower_stmt(st))
                    .collect::<Vec<_>>();

                body.push(self.expr_to_stmt(increment));

                let body_block = hir::Block(body);

                let body = self.add_block(body_block);

                let while_expr =
                    util::Span::from_ast(self.add_expr(hir::Expr::While { cond, body }), for_expr);

                let block = hir::Block(vec![init, self.expr_to_stmt(while_expr)]);

                let block = self.add_block(block);

                hir::Expr::Block(block)
            }
            ast::Expr::IdentExpr(ref ident_expr) => hir::Expr::Ident(util::Span::from_ast(
                self.db.intern_name(ident_expr.name().unwrap().into()),
                &ident_expr.name().unwrap(),
            )),
            ast::Expr::IfExpr(ref if_expr) => {
                let cond = self.lower_expr(if_expr.condition().unwrap().expr().unwrap());
                let then_branch = self.lower_expr(ast::Expr::from(if_expr.then_branch().unwrap()));
                let else_branch = if let Some(else_branch) = if_expr.else_branch() {
                    Some(self.lower_expr(else_branch.expr()))
                } else {
                    None
                };

                hir::Expr::If {
                    cond,
                    then_branch,
                    else_branch,
                }
            }
            ast::Expr::IndexExpr(ref index_expr) => {
                let base = self.lower_expr(index_expr.base().unwrap());
                let index = self.lower_expr(index_expr.index().unwrap());
                hir::Expr::Index { base, index }
            }

            ast::Expr::Literal(ref literal_expr) => {
                let literal = hir::Literal::from_token(literal_expr.token_kind());
                hir::Expr::Literal(self.db.intern_literal(literal))
            }
            ast::Expr::MatchExpr(ref match_expr) => {
                let expr = self.lower_expr(match_expr.expr().unwrap());

                hir::Expr::Match {
                    expr,
                    arms: match_expr
                        .match_arm_list()
                        .unwrap()
                        .arms()
                        .map(|match_arm| {
                            let pats = match_arm
                                .pats()
                                .map(|pat| self.lower_pattern(pat))
                                .collect();

                            let expr = self.lower_expr(match_arm.expr().unwrap());
                            hir::MatchArm { pats, expr }
                        })
                        .collect(),
                }
            }
            ast::Expr::ParenExpr(ref paren_expr) => {
                let expr = paren_expr.expr().unwrap();

                hir::Expr::Paren(self.lower_expr(expr))
            }
            ast::Expr::PrefixExpr(ref prefix_expr) => {
                let op = hir::UnaryOp::from_kind(prefix_expr.op_kind().unwrap()).unwrap(); // TODO fix the unwraps
                let expr = self.lower_expr(prefix_expr.expr().unwrap());

                hir::Expr::Unary { op, expr }
            }
            ast::Expr::ReturnExpr(ref return_expr) => {
                let id = if let Some(expr) = return_expr.expr() {
                    Some(self.lower_expr(expr))
                } else {
                    None
                };

                hir::Expr::Return(id)
            }

            ast::Expr::WhileExpr(ref while_expr) => {
                let cond = self.lower_expr(while_expr.condition().unwrap().expr().unwrap());

                let loop_body = while_expr.loop_body().unwrap().block().unwrap();
                let block = hir::Block(
                    loop_body
                        .statements()
                        .map(|st| self.lower_stmt(st))
                        .collect::<Vec<_>>(),
                );

                let body = self.add_block(block);

                hir::Expr::While { cond, body }
            }
            ast::Expr::TupleExpr(ref tuple_expr) => {
                let exprs = tuple_expr
                    .exprs()
                    .map(|expr| self.lower_expr(expr))
                    .collect();

                hir::Expr::Tuple(exprs)
            }

            ast::Expr::EnumExpr(ref enum_expr) => {
                let def = enum_expr.segments().nth(0).unwrap();
                let variant = enum_expr.segments().nth(1).unwrap();

                let def = util::Span::from_ast(
                    self.db.intern_name(def.name().unwrap().into()),
                    &def.name().unwrap(),
                );

                let variant = util::Span::from_ast(
                    self.db.intern_name(variant.name().unwrap().into()),
                    &variant.name().unwrap(),
                );

                let expr = if let Some(expr) = enum_expr.expr() {
                    Some(self.lower_expr(expr))
                } else {
                    None
                };

                hir::Expr::Enum { def, variant, expr }
            }
        };

        util::Span::from_ast(self.add_expr(hir_expr), &node)
    }
}

pub(crate) fn lower_function_query(
    db: &impl HirDatabase,
    fun_id: hir::FunctionId,
) -> Arc<hir::Function> {
    let mut collector = FunctionDataCollector {
        db,
        param_id_count: 0,
        type_param_count: 0,
        stmt_id_count: 0,
        expr_id_count: 0,
        block_id_count: 0,
        pat_id_count: 0,
        params: Vec::new(),
        type_params: Vec::new(),
        ast_map: hir::FunctionAstMap::default(),
    };

    let function = db.lookup_intern_function(fun_id);

    let exported = function.visibility().is_some();

    let name: Option<crate::hir::Name> = function.name().map(|name| name.into());

    if let Some(type_params_list) = function.type_param_list() {
        for type_param in type_params_list.type_params() {
            collector.lower_type_param(type_param);
        }
    }

    if let Some(param_list) = function.param_list() {
        for param in param_list.params() {
            collector.lower_param(param);
        }
    }

    let body = if let Some(body) = function.body() {
        Some(
            body.block()
                .unwrap()
                .statements()
                .map(|statement| collector.lower_stmt(statement))
                .collect(),
        )
    } else {
        None
    };

    let returns = if let Some(ret) = function.ret_type() {
        Some(collector.lower_type(ret.type_ref().unwrap()))
    } else {
        None
    };

    let span = function.syntax().text_range();

    let name = util::Span::from_ast(db.intern_name(name.unwrap()), &function.name().unwrap());

    Arc::new(collector.finish(exported, name, body, returns, span))
}
