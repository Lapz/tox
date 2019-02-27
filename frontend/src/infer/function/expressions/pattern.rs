use ast as t;
use ctx::CompileCtx;

use infer::types::{CSpan, Constructor, PatternVar, Type, TypeCon};
use infer::{Infer, InferResult};
use syntax::ast::{self, Expression, MatchArm};
use util::pos::{Span, Spanned};
use util::symbol::Symbol;
use util::symbol::Symbols;

use infer::coverage::*;
use std::collections::HashMap;

impl Infer {
    pub(crate) fn infer_match(
        &mut self,
        cond: Spanned<Expression>,
        patterns: Spanned<Vec<Spanned<MatchArm>>>,
        whole_span: Span,
        ctx: &mut CompileCtx,
    ) -> InferResult<Spanned<t::TypedExpression>> {
        let cond = self.infer_expr(cond, ctx)?;

        let cond_con = type_to_constructor(ctx.symbols_mut(), cond.value.ty.clone()); // type of the pattern

        let mut clauses = vec![];





        for pattern in patterns.value {
            let clause = ast_to_coverage(&pattern.value.lhs, ctx);
            clauses.push(ClausePattern(clause))
        }




        let ideal = cond_con.into_iter().map(|con|
            {
                let (refine_to, _) = self.constructor_to_pattern(con,cond.span);
                IdealPattern(Spanned::new(refine_to, cond.span))
            }
        ).collect();

        if let Err(ref e) = self.check_coverage(ideal, &mut clauses, ctx) {

            match e {
                CoverageError::RedundantClause(ref clauses) => {
                    for clause in clauses {
                        ctx.error("Redundant pattern",clause.0.span);
                    }

                    return Err(());
                },

                _ => unimplemented!()
            }
        }

        unimplemented!()
    }
}

fn ast_to_coverage(p: &Spanned<ast::Pattern>, ctx: &mut CompileCtx) -> Spanned<Pattern> {
    let span = p.span;
    match &p.value {
        ast::Pattern::Var(ref var) => {
            let p = PatternVar::new();
            ctx.add_pattern_var(var.value, p);

            Spanned::new(Pattern::Bind(p), span)
        }

        ast::Pattern::Con(ref name, ref inner) => Spanned::new(
            Pattern::Const(
                name.value,
                inner.iter().map(|p| ast_to_coverage(p, ctx)).collect(),
            ),
            span,
        ),
    }
}

fn type_to_constructor(symbols: &mut Symbols<()>, t: Type) -> Vec<Constructor> {
    match t {
        Type::App(type_con, args) => match type_con {
            TypeCon::Int => vec![Constructor::new(symbols.symbol("int"), vec![], 0, CSpan::Infinity)],
            TypeCon::Float => vec![Constructor::new(symbols.symbol("float"), vec![], 0, CSpan::Infinity)],
            TypeCon::Str => vec![Constructor::new(symbols.symbol("str"), vec![], 0, CSpan::Infinity)],
            TypeCon::Bool => vec![Constructor::new(symbols.symbol("bool"), vec![], 0, CSpan::Range(2))],
            TypeCon::Void => vec![Constructor::new(symbols.symbol("nil"), vec![], 0, CSpan::Range(1))],
            TypeCon::Array(ty) => unimplemented!(),
            TypeCon::Arrow => {
                let len = args.len();
                vec![Constructor::new(symbols.symbol(""), args, 1, CSpan::Range(len))]
            }
        },
        Type::Nil => vec![Constructor::new(symbols.symbol("nil"), vec![], 0, CSpan::Range(1))],
        Type::Var(_) => vec![Constructor::new(symbols.symbol(""), vec![], 0, CSpan::Range(1))],
        Type::Variant(c) => vec![c],
        Type::Enum { ref variants,.. } => {

            let mut cons = Vec::new();


            for (_,variant) in variants {
                cons.push(variant.constructor.clone());
            }
            cons
        }

        Type::Class(name, properties, _, _) => {
            let arity = properties.len();
            vec![Constructor::new(
                name,
                properties.into_iter().map(|p| p.ty).collect(),
                arity,
                CSpan::Range(1),
            )]
        }

        Type::Generic(_, inner) => type_to_constructor(symbols, *inner),
    }
}
