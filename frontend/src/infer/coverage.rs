use ast as t;
use ctx::CompileCtx;

use infer::types::{CSpan, Constructor, PatternVar, Type, TypeCon};
use infer::{Infer, InferResult};
use std::collections::HashMap;
use syntax::ast::{self, Expression, MatchArm};
use util::pos::{Span, Spanned};
use util::symbol::Symbol;
use util::symbol::Symbols;
use std::collections::HashSet;
use std::hash::Hash;

/// A substitution is either injective, or not-injective due to a binding
#[derive(Debug, Clone)]
pub enum IsInjectiveResult {
    Injective,
    NonInjective(PatternVar),
}

/// The pattern from the match statement
#[derive(Debug, Clone,PartialEq,Eq,Hash)]
pub struct ClausePattern(pub Spanned<Pattern>);

/// The pattern that we would like to get
#[derive(Debug, Clone)]
pub struct IdealPattern(pub Spanned<Pattern>);

/// Keeps a mapping between a pattern var and its pattern dec
#[derive(Debug, Clone)]
pub struct Subst(HashMap<PatternVar, Pattern>);

#[derive(Debug, Clone)]
pub enum CoverageError {
    NoTypeFound(PatternVar),
    NoConstructorsFound(Type),
    EmptyType(Type),
    RedundantClause(Vec<ClausePattern>),
    MalformedPattern(Pattern),
    CannotCover(IdealPattern),
}

#[derive(Debug, Clone,PartialEq,Eq,Hash)]
pub enum Pattern {
    /// binding like match foo{
    ///     x => ...
    /// }
    Bind(PatternVar),
    /// Cons(10,x)
    Const(Symbol, Vec<Spanned<Pattern>>),
}

/// How many times a pattern var occurs and the pattern itself
#[derive(Debug, Clone,PartialEq,Eq,Hash)]
pub struct Clause {
    usage: u32,
    pattern: ClausePattern,
}

impl Infer {
    pub fn covered_by_refined(
        &mut self,
        fname: PatternVar,
        clauses: &mut Vec<Clause>,
        con: Constructor,
        ideal: &IdealPattern,
        span:Span,
        ctx: &mut CompileCtx,
    ) -> Result<Vec<Clause>, CoverageError> {
        let (refine_to, refined_types) = self.constructor_to_pattern(con,span);
        let mut subst = Subst::new();

        subst.insert(fname, refine_to);
        let refined = subst.apply(&ideal);

        ctx.with_types(refined_types.into_iter().collect::<HashMap<_, _>>());
        self.covered_by(refined, clauses, ctx)
    }

    pub fn constructor_to_pattern(&mut self, c: Constructor,span:Span) -> (Pattern, Vec<(PatternVar, Type)>) {
        let args_len = c.types().len();

        let fresh_names = (0..args_len).map(|_| PatternVar::new()).collect::<Vec<_>>();
        let type_assocs = fresh_names
            .iter()
            .zip(c.types().clone().into_iter())
            .map(|(nm, ty)| (*nm, ty))
            .collect::<Vec<_>>();

        (
            Pattern::Const(
                c.symbol(),
                fresh_names
                    .into_iter()
                    .map(|nm| Spanned::new(Pattern::Bind(nm),span))
                    .collect(),
            ),
            type_assocs,
        )
    }

    pub fn has_subst(
        &mut self,
        ideal: Pattern,
        clause: Pattern,
    ) -> Result<Option<Subst>, CoverageError> {
        match (ideal, clause) {
            (Pattern::Bind(x), pat) => {
                let mut subst = Subst::new();
                subst.insert(x, pat);
                Ok(Some(subst))
            }

            (Pattern::Const(nm1, pats1), Pattern::Const(nm2, pats2)) => {
                if nm1 != nm2 {
                    Ok(None)
                } else if pats1.len() != pats2.len() {
                    Err(CoverageError::MalformedPattern(Pattern::Const(nm2, pats2)))
                } else if pats1.is_empty() {
                    Ok(Some(Subst::new()))
                } else {
                    let mut substs = Vec::new();

                    for (p1, p2) in pats1.into_iter().zip(pats2.into_iter()) {
                        if let Some(subst) = self.has_subst(p1.value, p2.value)? {
                            substs.push(subst);
                        }

                    }

                    Ok(Some(substs.into_iter().fold(
                        Subst::new(),
                        |mut acc, item| {
                            acc.concat(item);
                            acc
                        },
                    )))
                }
            }

            (Pattern::Const(_, _), Pattern::Bind(_)) => Ok(Some(Subst::new())),
        }
    }

    pub fn covered_by(
        &mut self,
        ideal: IdealPattern,
        clauses: &mut Vec<Clause>,
        ctx: &mut CompileCtx,
    ) -> Result<Vec<Clause>, CoverageError> {
        if clauses.is_empty() {
            Err(CoverageError::CannotCover(ideal))
        } else {
            let clause = clauses.remove(0);

            let span = clause.pattern.0.span;
            match self.has_subst(ideal.0.value.clone(), clause.pattern.0.value.clone())? {
                Some(mut subst) => match subst.is_injective() {
                    IsInjectiveResult::Injective => {

                        clauses.insert(0,clause.make_use());
                        Ok(clauses.to_vec())
                    }
                    IsInjectiveResult::NonInjective(binding) => {
                        let typ = ctx.get_pattern_type(binding)?;
                        let constructors = ctx.get_constructors(typ)?;
                        clauses.insert(0, clause);

                        Ok(constructors
                            .into_iter()
                            .flat_map(|con| {
                                self.covered_by_refined(binding, clauses, con, &ideal, span,ctx)
                            })
                            .flatten()
                            .collect())
                    }
                },
                None => {
                    Ok(self.covered_by(ideal, clauses, ctx)?)
                }
            }
        }
    }

    pub fn check_coverage(
        &mut self,
        ideal: Vec<IdealPattern>,
        user_patterns: &mut Vec<ClausePattern>,
        ctx: &mut CompileCtx,
    ) -> Result<(), CoverageError> {

        let mut checked_clauses = Vec::new();

        let mut clauses = user_patterns
            .into_iter()
            .map(|clause| Clause::new(clause.clone()))
            .collect::<Vec<Clause>>();


        for ideal in ideal {


            checked_clauses.push(self.covered_by(
                ideal,
                &mut clauses,
                ctx,
            )?);

        }
        // hashset to remove doubles
        let checked_clauses = checked_clauses.into_iter().flatten().collect::<HashSet<_>>();





        let unused_patterns = self.unused_patterns(checked_clauses.into_iter().collect::<Vec<_>>());



        if !unused_patterns.is_empty() {

            Err(CoverageError::RedundantClause(unused_patterns))
        } else {
            Ok(())
        }
    }

    pub fn unused_patterns(&mut self, clauses: Vec<Clause>) -> Vec<ClausePattern> {
        clauses
            .into_iter()
            .filter_map(|clause| {
                if clause.usages() < 1 {
                    Some(clause.pattern())
                } else {
                    None
                }
            })
            .collect::<Vec<ClausePattern>>()
    }
}

impl Clause {
    pub fn new(pattern: ClausePattern) -> Self {
        Self { pattern, usage: 0 }
    }

    pub fn pattern(self) -> ClausePattern {
        self.pattern
    }

    pub fn usages(&self) -> u32 {
        self.usage
    }

    pub fn make_use( self) -> Self {

        Self {
            usage:self.usage +1,
            pattern:self.pattern
        }

    }
}

impl Subst {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn insert(&mut self, key: PatternVar, val: Pattern) -> Option<Pattern> {
        self.0.insert(key, val)
    }

    pub fn concat(&mut self, other: Subst) {
        self.0.extend(other.0)
    }

    pub fn apply(&mut self, p: &IdealPattern) -> IdealPattern {

        match &p.0.value {
            Pattern::Bind(ref i) => {
                if let Some(ref pat) = self.0.get(i) {
                    IdealPattern(Spanned::new((**pat).clone(),p.0.span))
                } else {
                    IdealPattern(Spanned::new(Pattern::Bind(*i),p.0.span))
                }
            }
            Pattern::Const(ref nm, subpats) => {
                let mut nsubpats = Vec::new();

                for pat in subpats {
                    nsubpats.push(self.apply(&IdealPattern(pat.clone())).0)
                }

                IdealPattern(Spanned::new(Pattern::Const(nm.clone(), nsubpats),p.0.span))
            }
        }
    }

    pub fn is_injective(&mut self) -> IsInjectiveResult {
        for (b, p) in self.0.iter() {
            match p {
                Pattern::Bind(_) => continue,
                Pattern::Const(_, _) => return IsInjectiveResult::NonInjective(*b),
            }
        }

        IsInjectiveResult::Injective
    }
}
