use crate::{
    hir::{self, BinOp, ExprId, Function, Name, NameId, PatId, StmtId, UnaryOp},
    infer::{self, StackedMap, TypeVar},
    util, HirDatabase,
};
use errors::{FileId, Reporter};
use util::ReporterSpan;

use std::{collections::HashMap, rc::Rc, todo};
use std::{sync::Arc, usize};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeCon {
    Bool,
    Float,
    Int,
    Str,
    Void,
    Array { ty: Rc<Type>, size: Option<usize> },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Variant {
    pub tag: usize,
    pub ty: Option<Type>,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    /// (x |-> y) <br/>
    /// Last type in a app is the return type. <br/>
    /// i.e. i32 => App(Con(Int))
    /// and f(a:i32,b:i32) -> f32 <br/>
    /// App(vec&#33;[App(Con(Int)) ,App(Con(Int)),App(Con(Float)) ])
    App(Rc<Vec<Type>>),
    Tuple(Rc<Vec<Type>>),
    Var(TypeVar),
    Con(TypeCon),
    Enum(NameId, HashMap<NameId, Variant>),
    Class {
        name: NameId,
        fields: HashMap<NameId, Type>,
        methods: HashMap<NameId, Type>,
    },
    Unknown,
}

#[derive(Debug, Clone)]
pub(crate) struct Scheme {
    vars: HashMap<NameId, TypeVar>,
    ty: Type,
}
#[derive(Debug)]
pub struct Unify {
    pub(crate) errors: Vec<Reason>,
    pub(crate) tvar_count: u32,
}
#[derive(Debug)]
pub(crate) struct InferDataCollector<DB> {
    pub(crate) db: DB,
    pub(crate) reporter: Reporter,
    pub(crate) type_schemes: StackedMap<NameId, Scheme>,
    pub(crate) substitutions: HashMap<TypeVar, Type>,
    pub(crate) tvar_count: u32,
    pub(crate) file: FileId,
    pub(crate) errors: Vec<Reason>,
    pub(crate) returns: Option<Type>,
}

impl Into<TypeCon> for infer::TypeCon {
    fn into(self) -> TypeCon {
        match self {
            infer::TypeCon::Bool => TypeCon::Bool,
            infer::TypeCon::Float => TypeCon::Float,
            infer::TypeCon::Int => TypeCon::Int,
            infer::TypeCon::Str => TypeCon::Str,
            infer::TypeCon::Void => TypeCon::Void,
            infer::TypeCon::Array { ty, size } => TypeCon::Array {
                ty: Rc::new(Type::from(ty)),
                size,
            },
        }
    }
}

impl From<Box<infer::Type>> for Type {
    fn from(t: Box<infer::Type>) -> Self {
        let t = &*t;
        t.clone().into()
    }
}

impl From<Arc<infer::Type>> for Type {
    fn from(t: Arc<infer::Type>) -> Self {
        let t = &*t;
        t.clone().into()
    }
}
impl From<infer::Type> for Type {
    fn from(t: infer::Type) -> Self {
        match t {
            infer::Type::App(types) | infer::Type::Tuple(types) => Type::App(Rc::new(
                types.into_iter().map(|t| t.into()).collect::<Vec<_>>(),
            )),

            infer::Type::Poly(_, inner) => Type::from(*inner),
            infer::Type::Var(v) => Type::Var(v),
            infer::Type::Con(con) => Type::Con(con.into()),
            infer::Type::Enum(name, variants) => Type::Enum(
                name,
                variants
                    .into_iter()
                    .map(|(n, v)| {
                        (
                            n,
                            Variant {
                                tag: v.tag,
                                ty: v.ty.map(|t| t.into()),
                            },
                        )
                    })
                    .collect::<HashMap<_, _>>(),
            ),
            infer::Type::Class {
                name,
                fields,
                methods,
            } => Type::Class {
                name,
                fields: fields
                    .into_iter()
                    .map(|(n, t)| (n, t.into()))
                    .collect::<HashMap<_, _>>(),
                methods: methods
                    .into_iter()
                    .map(|(n, t)| (n, t.into()))
                    .collect::<HashMap<_, _>>(),
            },
            infer::Type::Unknown => Type::Unknown,
        }
    }
}
// impl Into<Type> for infer::Type {
//     fn into(self) -> Type {}
// }

// impl Into<Type> for Arc<infer::Type> {
//     fn into(self) -> Type {
//         let s: Type = *(&*self).into();
//         s
//     }
// }

impl<'a, DB> InferDataCollector<&'a DB>
where
    DB: HirDatabase,
{
    /// Apply the type substitution that we have already to the supplied type var
    fn shallow_normalize(&self, a: &Type) -> Type {
        match a {
            Type::Var(var) => self.get_var_type(var),
            ty => ty.clone(),
        }
    }

    fn get_var_type(&self, var: &TypeVar) -> Type {
        match self.substitutions.get(var) {
            Some(v) => self.shallow_normalize(v),
            None => Type::Var(*var),
        }
    }

    fn type_var(&mut self) -> TypeVar {
        let var = TypeVar(self.tvar_count);
        self.tvar_count += 1;
        var
    }

    pub(crate) fn insert_scheme_for_pat(
        &mut self,
        pat: hir::PatId,
        ty: Type,
        ast_map: &hir::FunctionAstMap,
    ) {
        let pat = ast_map.pat(&pat);

        match pat {
            hir::Pattern::Bind { name } => self.type_schemes.insert(
                name.item,
                Scheme {
                    vars: HashMap::new(),
                    ty,
                },
            ),
            hir::Pattern::Placeholder => {
                let name = self.db.intern_name(Name::new("_"));

                self.type_schemes.insert(
                    name,
                    Scheme {
                        vars: HashMap::new(),
                        ty,
                    },
                )
            }
            hir::Pattern::Tuple(pats) => match ty {
                Type::Tuple(types) => {
                    for (pat, ty) in pats.iter().zip(types.iter()) {
                        self.insert_scheme_for_pat(pat.item, ty.clone(), ast_map)
                    }
                }
                _ => {}
            },
            hir::Pattern::Literal(_) => {}
        }
    }

    pub(crate) fn infer_function(&mut self, function: &Function) {
        let expected = self.db.resolve_named_type(self.file, function.name.item);

        for param in &function.params {
            let param = function.ast_map.param(&param.item);

            let ty = self.db.resolve_hir_type(self.file, param.ty.item).into();

            self.insert_scheme_for_pat(param.pat.item, ty, &function.ast_map)
        }

        if let Some(body) = &function.body {
            body.iter().for_each(|stmt| {
                self.infer_statement(stmt, &function.ast_map);
            })
        }

        println!("{:#?}", self.type_schemes);
    }

    pub(crate) fn infer_statement(
        &mut self,
        id: &util::Span<StmtId>,
        map: &hir::FunctionAstMap,
    ) -> Type {
        let stmt = map.stmt(&id.item);

        match stmt {
            hir::Stmt::Let {
                pat,
                ascribed_type,
                initializer,
            } => {
                match (ascribed_type, initializer) {
                    (Some(expected), Some(init)) => {
                        let expr = self.infer_expr(map, init);
                        let expected = self.db.resolve_hir_type(self.file, expected.item).into();

                        self.unify(&expected, &expr, init.as_reporter_span());
                        self.insert_scheme_for_pat(pat.item, expr, map);
                    }
                    (Some(expected), None) => {
                        let ty = self.db.resolve_hir_type(self.file, expected.item).into();
                        self.insert_scheme_for_pat(pat.item, ty, map);
                    }
                    (None, Some(init)) => {
                        let expected = self.infer_expr(map, init);

                        self.insert_scheme_for_pat(pat.item, expected, map);
                    }
                    (None, None) => {
                        self.insert_scheme_for_pat(pat.item, Type::Con(TypeCon::Void), map);
                    }
                };

                Type::Con(TypeCon::Void)
            }

            hir::Stmt::Expr(expr) => self.infer_expr(map, expr),
        }
    }

    pub(crate) fn infer_expr(
        &mut self,
        map: &hir::FunctionAstMap,
        id: &util::Span<ExprId>,
    ) -> Type {
        let expr = map.expr(&id.item);
        match expr {
            hir::Expr::Array(exprs) => {
                if exprs.len() == 0 {
                    return Type::Con(TypeCon::Array {
                        ty: Rc::new(Type::Con(TypeCon::Void)),
                        size: None,
                    });
                }

                let first = self.infer_expr(map, &exprs[0]).into();

                exprs.iter().skip(1).for_each(|id| {
                    let inferred = self.infer_expr(map, id);
                    self.unify(&first, &inferred, id.as_reporter_span())
                });

                Type::Con(TypeCon::Array {
                    ty: Rc::new(first),
                    size: None,
                })
            }

            hir::Expr::Block(block, has_value) => self.infer_block(map, block, *has_value),
            hir::Expr::Break | hir::Expr::Continue => Type::Con(TypeCon::Void),
            hir::Expr::Binary { lhs, op, rhs } => {
                let inferred_lhs = self.infer_expr(map, lhs);
                let inferred_rhs = self.infer_expr(map, rhs);

                match op {
                    BinOp::Plus | BinOp::Minus | BinOp::Mult | BinOp::Div => {
                        self.unify(&inferred_lhs, &inferred_rhs, id.as_reporter_span());
                        inferred_lhs
                    }
                    BinOp::And | BinOp::Or => {
                        self.unify(
                            &inferred_lhs,
                            &Type::Con(TypeCon::Bool),
                            lhs.as_reporter_span(),
                        );
                        self.unify(
                            &inferred_rhs,
                            &Type::Con(TypeCon::Bool),
                            rhs.as_reporter_span(),
                        );

                        Type::Con(TypeCon::Bool)
                    }
                    BinOp::LessThan
                    | BinOp::GreaterThan
                    | BinOp::GreaterThanEqual
                    | BinOp::LessThanEqual => {
                        self.unify(&inferred_lhs, &inferred_rhs, id.as_reporter_span());
                        Type::Con(TypeCon::Bool)
                    }
                    BinOp::Equal => {
                        self.unify(&inferred_lhs, &inferred_rhs, id.as_reporter_span());

                        inferred_rhs
                    }
                    BinOp::EqualEqual | BinOp::NotEqual => Type::Con(TypeCon::Bool),
                    BinOp::PlusEqual | BinOp::MinusEqual | BinOp::MultEqual | BinOp::DivEqual => {
                        // TODO unify the rhs and lhs  with addable types
                        self.unify(&inferred_lhs, &inferred_rhs, id.as_reporter_span());

                        inferred_rhs
                    }
                }
            }
            hir::Expr::Literal(literal) => self.infer_literal(*literal),
            hir::Expr::Paren(inner) => self.infer_expr(map, inner),
            hir::Expr::Tuple(exprs) => {
                let types = exprs
                    .iter()
                    .map(|id| self.infer_expr(map, id))
                    .collect::<Vec<_>>();
                Type::Tuple(Rc::new(types))
            }
            hir::Expr::Unary { op, expr } => {
                let inferred = self.infer_expr(map, id);
                match op {
                    UnaryOp::Minus => match inferred {
                        Type::Con(TypeCon::Int) | Type::Con(TypeCon::Float) => inferred,

                        _ => {
                            let msg = format!("Cannot use `-` operator on type `{:?}`", inferred);

                            self.errors.push(Reason {
                                msg,
                                notes: Some("`-` only works on i32,f32,".into()),
                                span: expr.as_reporter_span(),
                            });

                            Type::Con(TypeCon::Int)
                        }
                    },
                    UnaryOp::Excl => {
                        self.unify(
                            &inferred,
                            &Type::Con(TypeCon::Bool),
                            expr.as_reporter_span(),
                        );
                        Type::Con(TypeCon::Bool)
                    }
                }
            }
            hir::Expr::Return(expr) => {
                let expected = self.returns.clone().unwrap();
                if let Some(id) = expr {
                    let inferred = self.infer_expr(map, id);

                    self.unify(&expected, &inferred, id.as_reporter_span());
                    inferred
                } else {
                    let inferred = Type::Con(TypeCon::Void);
                    self.unify(&expected, &inferred, id.as_reporter_span());
                    inferred
                }
            }
            hir::Expr::Match { expr, arms } => {
                let inferred_expr = self.infer_expr(map, expr);

                // let mut matrix = PatternMatrix::new();

                // for match_arm in arms {
                //     let mut patterns = vec![];

                //     for pattern in &match_arm.pats {
                //         let pat = map.pat(&pattern.item);

                //         patterns.push(self.to_matrix_pattern(pat, map))
                //     }

                //     matrix.add_row(Row::new(patterns, match_arm.expr.item))
                // }

                Type::Unknown
            }
            hir::Expr::Enum { def, variant, expr } => {
                let inferred_def = self.db.resolve_named_type(self.file, def.item).into();

                match inferred_def {
                    Type::Enum(_, ref variants) => {
                        if let Some(v) = variants.get(&variant.item) {
                            match (expr, &v.ty) {
                                (None, None) => {}
                                (None, Some(variant_ty)) => {
                                    let msg = format!("Missing enum variant constructor",);
                                    self.reporter.error(
                                        msg,
                                        format!("Expected an enum variant constructor of type {:?} but found none",variant_ty),
                                        variant.as_reporter_span(),
                                    );
                                }
                                (Some(_), None) => {
                                    let msg = format!("Unexpected enum variant constructor",);
                                    let name = self.db.lookup_intern_name(variant.item);
                                    self.reporter.error(
                                        msg,
                                        format!(
                                            "enum variant `{}` does not have a constructor",
                                            name
                                        ),
                                        variant.as_reporter_span(),
                                    );
                                }
                                (Some(expr), Some(variant_ty)) => {
                                    let inferred_expr =
                                        self.shallow_normalize(&self.infer_expr(map, expr));

                                    // for ty_var in vars {
                                    //     subst.insert(*ty_var, inferred_expr.clone());
                                    // }

                                    // let variant_ty = self.subst(&variant_ty, &mut subst);

                                    self.unify(&inferred_expr, &variant_ty, expr.as_reporter_span())
                                }
                            }
                        }

                        inferred_def
                    }
                    _ => {
                        // Error reported in resolver
                        Type::Unknown
                    }
                }
            },
            hir::Expr::RecordLiteral { def, fields } =>{
                let inferred_def = self.db.resolve_named_type(self.file, def.item).into();
            }

            e => todo!("{:?}", e),
        }
    }

    fn infer_literal(&mut self, lit_id: hir::LiteralId) -> Type {
        let lit = self.db.lookup_intern_literal(lit_id);

        match lit {
            hir::Literal::String(_) => Type::Con(TypeCon::Str),
            hir::Literal::Nil => Type::Con(TypeCon::Void),
            hir::Literal::True | hir::Literal::False => Type::Con(TypeCon::Bool),
            hir::Literal::Int(_) => Type::Con(TypeCon::Int),
            hir::Literal::Float(_) => Type::Con(TypeCon::Float),
        }
    }

    pub(crate) fn infer_block(
        &mut self,
        map: &hir::FunctionAstMap,
        block_id: &hir::BlockId,
        has_value: bool,
    ) -> Type {
        self.type_schemes.begin_scope();

        let block = map.block(block_id);

        let mut returns = Type::Con(TypeCon::Void);

        for (index, stmt) in block.0.iter().enumerate() {
            let ty = self.infer_statement(stmt, map);
            if has_value && index == block.0.len() - 1 {
                returns = ty;
            }
        }

        self.type_schemes.end_scope();

        returns
    }

    pub(crate) fn unify(&mut self, lhs: &Type, rhs: &Type, span: ReporterSpan) {
        let lhs = self.shallow_normalize(lhs);
        let rhs = self.shallow_normalize(rhs);

        match (&lhs, &rhs) {
            (Type::App(types1), Type::App(types2)) => {
                if types1.len() != types2.len() {
                    self.errors.push(Reason {
                        msg: format!("Expected {} params, found {}", types1.len(), types2.len()),
                        notes: None,
                        span,
                    });
                    return;
                }

                if types1[types1.len() - 1] != types2[types2.len() - 1] {
                    self.errors.push(Reason {
                        msg: format!(
                            "Expected {:?} found {:?}",
                            types1[types1.len() - 1],
                            types2[types2.len() - 1]
                        ),
                        notes: Some("The return types are different".into()),
                        span,
                    });

                    return;
                }
            }

            (Type::App(signature), ret) => {
                let last = signature.last().unwrap_or(&Type::Con(TypeCon::Void));
                self.unify(last, ret, span)
            }
            (Type::Tuple(types1), Type::Tuple(types2)) => {
                if types1.len() != types2.len() {
                    self.errors.push(Reason {
                        msg: format!(
                            "Expected {} tuple elements, found {}",
                            types1.len(),
                            types2.len()
                        ),
                        notes: None,
                        span,
                    });

                    return;
                }

                for (a, b) in types1.iter().zip(types2.iter()) {
                    let _ = self.unify(a, b, span);
                }
            }

            (Type::Enum(name1, variants1), Type::Enum(name2, variants2)) => {
                if name1 != name2 {
                    self.errors.push(Reason {
                        msg: format!(
                            "Expected enum `{:?}` but found enum `{:?}`",
                            name1,
                            name2 // self.db.lookup_intern_name(*name1),
                                  // self.db.lookup_intern_name(*name2)
                        ),
                        notes: None,
                        span,
                    })
                }

                if variants1 != variants2 {
                    self.errors.push(Reason {
                        msg: format!(
                            "Expected enum `{:?}` but found enum `{:?}`",
                            name1,
                            name2 // self.db.lookup_intern_name(*name1),
                                  // self.db.lookup_intern_name(*name2)
                        ),
                        notes: Some("The names are the same but the variants are different".into()),
                        span,
                    })
                }
            }
            (
                Type::Class {
                    name,
                    fields,
                    methods,
                },
                Type::Class {
                    name: name1,
                    fields: fields1,
                    methods: methods1,
                },
            ) => {
                if name != name1 {
                    self.errors.push(Reason {
                        msg: format!(
                            "Expected class `{:?}` but found enum `{:?}`",
                            name,
                            name1 // self.db.lookup_intern_name(*name1),
                                  // self.db.lookup_intern_name(*name2)
                        ),
                        notes: None,
                        span,
                    })
                }

                if fields != fields1 && methods != methods1 {
                    self.errors.push(Reason {
                        msg: format!(
                            "Expected enum `{:?}` but found enum `{:?}`",
                            name,
                            name1 // self.db.lookup_intern_name(*name1),
                                  // self.db.lookup_intern_name(*name2)
                        ),
                        notes: Some(
                            "The names are the same but the class fields/methods are different"
                                .into(),
                        ),
                        span,
                    })
                }
            }

            (Type::Var(a), ty) | (ty, Type::Var(a)) => self.unify_var_type(*a, ty, span),

            (Type::Con(TypeCon::Int), Type::Con(TypeCon::Float))
            | (Type::Con(TypeCon::Float), Type::Con(TypeCon::Int)) => return,
            (Type::Unknown, _) | (_, Type::Unknown) => return,
            (Type::Con(l), Type::Con(r)) if l == r => return,
            (_, _) => {
                self.errors.push(Reason {
                    msg: format!("Mismatching types, expected `{:?}` found `{:?}`", lhs, rhs),
                    notes: None,
                    span,
                });
                return;
            }
        }
    }

    fn unify_var_type(&mut self, var: TypeVar, ty: &Type, span: ReporterSpan) {
        match ty {
            Type::Var(tv) if &var == tv => return,
            _ => {}
        }

        if self.occurs(&var, &ty) {
            self.errors.push(Reason {
                msg: format!("Infinite type"),
                notes: None,
                span,
            });
            return;
        }
    }

    fn occurs(&self, tv: &TypeVar, ty: &Type) -> bool {
        match ty {
            Type::App(params) => {
                for param in params.iter() {
                    if self.occurs(tv, param) {
                        return true;
                    }
                }

                false
            }
            Type::Tuple(params) => {
                for param in params.iter() {
                    if self.occurs(tv, param) {
                        return true;
                    }
                }

                false
            }
            Type::Var(v) => v == tv,
            Type::Con(con) => match con {
                TypeCon::Bool | TypeCon::Float | TypeCon::Int | TypeCon::Str | TypeCon::Void => {
                    false
                }
                TypeCon::Array { ty, size } => self.occurs(tv, ty),
            },
            Type::Enum(_, variants) => {
                for (_, variant) in variants {
                    if let Some(ty) = variant.ty.as_ref() {
                        if self.occurs(tv, ty) {
                            return true;
                        }
                    }
                }

                false
            }
            Type::Class {
                name,
                fields,
                methods,
            } => {
                for (_, field) in fields {
                    if self.occurs(tv, field) {
                        return true;
                    }
                }

                false
            }
            Type::Unknown => false,
        }
    }
}
#[derive(Debug)]
pub struct Reason {
    msg: String,
    notes: Option<String>,
    span: ReporterSpan,
}

// impl Unify {
//     pub fn type_var(&mut self) -> TypeVar {
//         let var = TypeVar(self.tvar_count);
//         self.tvar_count += 1;
//         var
//     }

//     pub fn unify(&mut self, lhs: &Type, rhs: &Type) {
//         let lhs = self.shallow_normalize(lhs);
//         let rhs = self.shallow_normalize(rhs);

//     }

//     pub fn insert(&mut self, var: TypeVar, ty: Type) {
//         self.context.insert(var, ty);
//     }

//     fn get_var_type(&self, var: &TypeVar) -> Type {
//         match self.context.get(var) {
//             Some(v) => self.shallow_normalize(v),
//             None => Type::Var(*var),
//         }
//     }

//     /// Given a type scheme with fn <T>(v:T) -> T;
//     /// If we instantiate it with <T = i32> ;
//     /// we should obtain fn (v:i32) -> i32;
//     pub fn subst(&self, ty: &Type) -> Type {
//         match ty {
//             Type::App(types) => Type::App(types.iter().map(|ty| self.subst(ty)).collect()),

//             Type::Var(v) => {
//                 if let Some(t) = self.context.get(v) {
//                     t.clone()
//                 } else {
//                     Type::Var(*v)
//                 }
//             }
//             Type::Tuple(types) => Type::Tuple(types.iter().map(|ty| self.subst(ty)).collect()),
//             Type::Con(con) => Type::Con(match con {
//                 TypeCon::Bool => TypeCon::Bool,
//                 TypeCon::Float => TypeCon::Float,
//                 TypeCon::Int => TypeCon::Int,
//                 TypeCon::Str => TypeCon::Str,
//                 TypeCon::Void => TypeCon::Void,
//                 TypeCon::Array { ty, size } => TypeCon::Array {
//                     ty: Rc::new(self.subst(ty)),
//                     size: size.clone(),
//                 },
//             }),
//             Type::Enum(name, variants) => Type::Enum(
//                 *name,
//                 variants
//                     .iter()
//                     .map(|(name, v)| {
//                         (
//                             *name,
//                             Variant {
//                                 tag: v.tag,
//                                 ty: v.ty.clone().map(|ty| self.subst(&ty)),
//                             },
//                         )
//                     })
//                     .collect(),
//             ),
//             Type::Class {
//                 name,
//                 fields,
//                 methods,
//             } => Type::Class {
//                 name: *name,
//                 fields: fields
//                     .iter()
//                     .map(|(name, ty)| (*name, self.subst(ty)))
//                     .collect(),
//                 methods: methods
//                     .iter()
//                     .map(|(name, ty)| (*name, self.subst(ty)))
//                     .collect(),
//             },
//             Type::Unknown => Type::Unknown,
//         }
//     }
// }
