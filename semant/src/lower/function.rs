use crate::db::HirDatabase;
use crate::hir::{self};
use std::sync::Arc;

use syntax::{
    ast, ArgListOwner, AstNode, AstPtr, LoopBodyOwner, NameOwner, TypeAscriptionOwner,
    TypeParamsOwner, TypesOwner, VisibilityOwner,
};

#[derive(Debug)]
pub(crate) struct FunctionDataCollector<DB> {
    db: DB,
    type_param_count: u64,
    param_id_count: u64,
    stmt_id_count: u64,
    expr_id_count: u64,
    block_id_count: u64,
    ast_map: hir::FunctionAstMap,
    params: Vec<hir::ParamId>, // expressions: HashMap<hir::ExprId, hir::Expr>,
    type_params: Vec<hir::TypeParamId>,
}

impl<'a, DB> FunctionDataCollector<&'a DB>
where
    DB: HirDatabase,
{
    pub fn finish(
        self,
        exported: bool,
        name: hir::Name,
        body: Option<Vec<hir::StmtId>>,
        span: hir::Span,
    ) -> hir::Function {
        let params = self.params;
        let type_params = self.type_params;
        let map = self.ast_map;
        let name = self.db.intern_name(name);
        hir::function::Function {
            exported,
            name,
            map,
            params,
            type_params,
            body,
            span,
        }
    }

    pub fn add_param(&mut self, ast_node: &ast::Param, param: hir::Param) {
        let current = self.param_id_count;

        self.param_id_count += 1;

        let id = hir::ParamId(current);

        self.ast_map.insert_param(id, param, AstPtr::new(ast_node));

        self.params.push(id);
    }

    pub fn add_stmt(&mut self, ast_node: &ast::Stmt, stmt: hir::Stmt) -> hir::StmtId {
        let current = self.stmt_id_count;

        self.stmt_id_count += 1;

        let id = hir::StmtId(current);

        self.ast_map.insert_stmt(id, stmt, AstPtr::new(ast_node));

        id
    }

    pub fn expr_to_stmt(&mut self, expr: hir::ExprId) -> hir::StmtId {
        let node = self.ast_map.get_expr_ptr(expr);

        let current = self.stmt_id_count;

        self.stmt_id_count += 1;

        let id = hir::StmtId(current);

        self.ast_map.insert_stmt(
            id,
            hir::Stmt::Expr(expr),
            AstPtr::from_ptr(node.syntax_node_ptr()),
        );

        id
    }

    pub fn add_expr(&mut self, ast_node: &ast::Expr, expr: hir::Expr) -> hir::ExprId {
        let current = self.expr_id_count;

        self.expr_id_count += 1;

        let id = hir::ExprId(current);

        self.ast_map.insert_expr(id, expr, AstPtr::new(ast_node));

        id
    }

    pub fn add_block(&mut self, ast_node: &ast::Block, block: hir::Block) -> hir::BlockId {
        let current = self.block_id_count;

        self.block_id_count += 1;

        let id = hir::BlockId(current);

        self.ast_map.insert_block(id, block, AstPtr::new(ast_node));

        id
    }

    pub fn add_type_param(&mut self, ast_node: &ast::TypeParam, type_param: hir::TypeParam) {
        let current = self.type_param_count;

        self.type_param_count += 1;

        let id = hir::TypeParamId(current);

        self.ast_map
            .insert_type_param(id, type_param, AstPtr::new(ast_node));
    }

    pub(crate) fn lower_pattern(&mut self, pat: ast::Pat) -> hir::PatId {
        let pattern = match &pat {
            ast::Pat::BindPat(binding) => {
                let name: crate::hir::Name = binding
                    .name()
                    .map(|n| n.into())
                    .unwrap_or_else(crate::hir::Name::missing);
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

        self.db.intern_pattern(pattern)
    }

    pub(crate) fn lower_param(&mut self, param: ast::Param) {
        let pat = self.lower_pattern(param.pat().unwrap());

        let ty = self.lower_type(param.ascribed_type().unwrap());

        self.add_param(&param, hir::Param { pat, ty });
    }

    pub(crate) fn lower_type_param(&mut self, type_param: ast::TypeParam) {
        let name = self.db.intern_name(type_param.name().unwrap().into());

        self.add_type_param(&type_param, hir::TypeParam { name });
    }

    pub(crate) fn lower_type(&mut self, ty: ast::TypeRef) -> hir::TypeId {
        match ty {
            ast::TypeRef::ParenType(paren_ty) => {
                let mut types = Vec::new();

                for c in paren_ty.types() {
                    types.push(self.lower_type(c))
                }

                self.db.intern_type(hir::Type::ParenType(types))
            }
            ast::TypeRef::ArrayType(array_ty) => {
                let ty = self.lower_type(array_ty.type_ref().unwrap());

                self.db.intern_type(hir::Type::ArrayType { ty, size: None })
            }
            ast::TypeRef::IdentType(ident_ty) => {
                let name: hir::Name = ident_ty.into();

                self.db
                    .intern_type(hir::Type::Ident(self.db.intern_name(name)))
            }

            ast::TypeRef::FnType(fn_ty) => {
                let mut params = Vec::new();

                for param in fn_ty.types() {
                    params.push(self.lower_type(param))
                }

                let ret = fn_ty
                    .ret_type()
                    .and_then(|ret| ret.type_ref().map(|ty| self.lower_type(ty)));

                self.db.intern_type(hir::Type::FnType { params, ret })
            }
        }
    }

    pub fn lower_stmt(&mut self, node: ast::Stmt) -> hir::StmtId {
        let hir_stmt = match node {
            ast::Stmt::LetStmt(ref let_stmt) => {
                let pat = self.lower_pattern(let_stmt.pat().unwrap());

                let initializer = if let Some(initializer) = let_stmt.initializer() {
                    Some(self.lower_expr(initializer))
                } else {
                    None
                };

                hir::Stmt::Let { pat, initializer }
            }
            ast::Stmt::ExprStmt(ref expr_stmt) => {
                hir::Stmt::Expr(self.lower_expr(expr_stmt.expr().unwrap()))
            }
        };

        self.add_stmt(&node, hir_stmt)
    }

    pub fn lower_expr(&mut self, node: ast::Expr) -> hir::ExprId {
        let expr = match node {
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
                let node = block.block().unwrap();
                let block = hir::Block(
                    block
                        .block()
                        .unwrap()
                        .statements()
                        .map(|st| self.lower_stmt(st))
                        .collect(),
                );

                hir::Expr::Block(self.add_block(&node, block))
            }

            ast::Expr::BreakExpr(_) => hir::Expr::Break,
            ast::Expr::CallExpr(ref call_expr) => {
                let callee = self.lower_expr(call_expr.expr().unwrap());
                let args = if let Some(arg_list) = call_expr.arg_list() {
                    arg_list.args().map(|arg| self.lower_expr(arg)).collect()
                } else {
                    Vec::new()
                };

                hir::Expr::Call { callee, args }
            }
            ast::Expr::CastExpr(ref cast_expr) => {
                let ty = self.lower_type(cast_expr.type_ref().unwrap());
                let expr = self.lower_expr(cast_expr.expr().unwrap());

                hir::Expr::Cast { expr, ty }
            }
            ast::Expr::RecordLiteralExpr(ref _record_lit) => unimplemented!(),
            ast::Expr::ClosureExpr(ref closure_expr) => {
                // let args = closure_expr.a

                unimplemented!()
            }
            ast::Expr::ContinueExpr(_) => hir::Expr::Continue,
            ast::Expr::FieldExpr(ref _field_expr) => unimplemented!(),
            ast::Expr::ForExpr(ref for_expr) => {
                let init = self.lower_expr(for_expr.init().unwrap());
                let cond = self.lower_expr(for_expr.cond().unwrap());
                let increment = self.lower_expr(for_expr.increment().unwrap());

                let loop_body = for_expr.loop_body().unwrap().block().unwrap();
                let mut body = loop_body
                    .statements()
                    .map(|st| self.lower_stmt(st))
                    .collect::<Vec<_>>();

                body.push(self.expr_to_stmt(increment));

                let body_block = hir::Block(body);

                let body = self.add_block(&loop_body, body_block);

                let while_expr = self.add_expr(&node, hir::Expr::While { cond, body });

                let block =
                    hir::Block(vec![self.expr_to_stmt(init), self.expr_to_stmt(while_expr)]);
                let block = self.add_block(&loop_body, block);

                hir::Expr::Block(block)
            }
            ast::Expr::IdentExpr(ref ident_expr) => {
                hir::Expr::Ident(self.db.intern_name(ident_expr.name().unwrap().into()))
            }
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
                let mut block = hir::Block(
                    loop_body
                        .statements()
                        .map(|st| self.lower_stmt(st))
                        .collect::<Vec<_>>(),
                );

                let body = self.add_block(&loop_body, block);

                hir::Expr::While { cond, body }
            }
            ast::Expr::TupleExpr(ref tuple_expr) => {
                let exprs = tuple_expr
                    .exprs()
                    .map(|expr| self.lower_expr(expr))
                    .collect();

                hir::Expr::Tuple(exprs)
            }
        };

        self.add_expr(&node, expr)
    }
}

pub(crate) fn lower_function_query(
    db: &impl HirDatabase,
    function: ast::FnDef,
) -> Arc<hir::Function> {
    let mut collector = FunctionDataCollector {
        db,
        param_id_count: 0,
        type_param_count: 0,
        stmt_id_count: 0,
        expr_id_count: 0,
        block_id_count: 0,
        params: Vec::new(),
        type_params: Vec::new(),
        ast_map: hir::FunctionAstMap::default(),
    };

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

    let span = function.syntax().text_range();

    Arc::new(collector.finish(exported, name.unwrap(), body, span))
}
