mod test;
use ast::expr::*;
use ast::statement::Statement;
use types::{BaseType, Type, TypeError};
use env::{Entry, Env};
use pos::{Postition, WithPos};
use symbol::Symbol;

type Exp = ();

#[derive(Debug, PartialEq)]
pub struct ExpressionType {
    pub ty: Type,
}
/// The struct that is in control of type checking
#[derive(Debug, PartialEq)]
pub struct TyChecker {
    pub this: Type,
}

impl TyChecker {
    pub fn new() -> Self {
        TyChecker {
            this: Type::Simple(BaseType::Nil),
        }
    }

    /// Checks if two types are eqvilant
    fn check_types(
        &self,
        expected: &Type,
        unknown: &Type,
        pos: Postition,
    ) -> Result<(), TypeError> {
        let expected = self.actual_type(expected);
        let unknown = self.actual_type(unknown);

        match (expected, unknown) {
            (
                &Type::Class {
                    name: ref n,
                    ref methods,
                    ..
                },
                &Type::Class {
                    name: ref sym,
                    methods: ref m,
                    ..
                },
            ) => {
                // Due to how class are inferred if a method on the class returns its self.
                // Its class type will have an empty methods; So we compare the name and the fields instead
                if n != sym && methods != m {
                    return Err(TypeError::Expected(expected.clone(), unknown.clone(), pos));
                }
            }

            (&Type::This(ref this, _, _), &Type::Class { ref name, .. })
            | (&Type::Class { ref name, .. }, &Type::This(ref this, _, _)) => {
                if this != name {
                    return Err(TypeError::Expected(expected.clone(), unknown.clone(), pos));
                }
            }

            (e, u) => {
                if expected != unknown {
                    return Err(TypeError::Expected(e.clone(), u.clone(), pos));
                }
            }
        }

        Ok(())
    }

    fn get_actual_ty(&self, entry: &Entry) -> Result<Type, TypeError> {
        match *entry {
            Entry::VarEntry(ref ty) => Ok(ty.clone()),
            Entry::FunEntry {
                ref params,
                ref returns,
            } => Ok(Type::Func(params.clone(), Box::new(returns.clone()))),
        }
    }

    fn actual_type<'a>(&self, ty: &'a Type) -> &'a Type {
        match *ty {
            Type::Name(_, ref name) => name,
            ref others => others,
        }
    }

    fn get_type(
        &self,
        ident: &ExpressionTy,
        pos: Postition,
        env: &mut Env,
    ) -> Result<Type, TypeError> {
        match *ident {
            ExpressionTy::Simple(s) => {
                if let Some(ty) = env.look_type(s) {
                    return Ok(ty.clone());
                }

                Err(TypeError::UndefindedType(env.name(s), pos))
            }
            ExpressionTy::Arr(ref s) => Ok(Type::Array(Box::new(self.get_type(s, pos, env)?))),
            ExpressionTy::Func(ref params, ref returns) => {
                let mut param_tys = Vec::with_capacity(params.len());

                for e_ty in params {
                    param_tys.push(self.get_type(e_ty, pos, env)?)
                }

                if let Some(ref ret) = *returns {
                    Ok(Type::Func(
                        param_tys,
                        Box::new(self.get_type(ret, pos, env)?),
                    ))
                } else {
                    Ok(Type::Func(param_tys, Box::new(Type::Simple(BaseType::Nil))))
                }
            }
        }
    }

    /// Given two expression types check if they are {int,int} or they are
    /// {float,float}

    fn check_int_float_str(
        &self,
        left: &ExpressionType,
        right: &ExpressionType,
        pos: Postition,
    ) -> Result<ExpressionType, TypeError> {
        if self.check_int(left, pos).is_err() || self.check_int(right, pos).is_err() {
            if self.check_float(left, pos).is_ok() {
                self.check_float(right, pos)?;
                Ok(ExpressionType {
                    ty: Type::Simple(BaseType::Float),
                })
            } else if self.check_str(left, pos).is_ok() {
                // self.check_str(left, pos)?;
                self.check_str(right, pos)?;
                Ok(ExpressionType {
                    ty: Type::Simple(BaseType::Str),
                })
            } else {
                Err(TypeError::ExpectedOneOf(
                    "Exepected on of 'Int', 'Float', or 'Str'".into(),
                ))
            }
        } else if self.check_int(left, pos).is_ok() && self.check_int(right, pos).is_ok() {
            Ok(ExpressionType {
                ty: Type::Simple(BaseType::Int),
            })
        } else {
            Err(TypeError::Expected(
                Type::Simple(BaseType::Int),
                right.ty.clone(),
                pos,
            ))
        }
    }

    /// Given an `ExpressionType` check if it an {int} or {float}
    fn s_check_int_float(
        &self,
        expr: &ExpressionType,
        pos: Postition,
    ) -> Result<ExpressionType, TypeError> {
        if self.check_int(expr, pos).is_err() {
            self.check_float(expr, pos)?;
            return Ok(ExpressionType {
                ty: Type::Simple(BaseType::Float),
            });
        }
        Ok(ExpressionType {
            ty: Type::Simple(BaseType::Int),
        })
    }

    /// Checks if `ExpressionType` is {bool}
    fn check_bool(&self, right: &ExpressionType, pos: Postition) -> Result<(), TypeError> {
        if right.ty != Type::Simple(BaseType::Bool) {
            return Err(TypeError::Expected(
                Type::Simple(BaseType::Bool),
                right.ty.clone(),
                pos,
            ));
        }
        Ok(())
    }

    /// Checks if `ExpressionType` is {int}
    fn check_int(&self, expr: &ExpressionType, pos: Postition) -> Result<(), TypeError> {
        if expr.ty != Type::Simple(BaseType::Int) {
            return Err(TypeError::Expected(
                Type::Simple(BaseType::Int),
                expr.ty.clone(),
                pos,
            ));
        }
        Ok(())
    }

    /// Checks if `ExpressionType` is {str}
    fn check_str(&self, expr: &ExpressionType, pos: Postition) -> Result<(), TypeError> {
        if expr.ty != Type::Simple(BaseType::Str) {
            return Err(TypeError::Expected(
                Type::Simple(BaseType::Str),
                expr.ty.clone(),
                pos,
            ));
        }
        Ok(())
    }
    /// Checks if `ExpressionType` is {float}
    fn check_float(&self, expr: &ExpressionType, pos: Postition) -> Result<(), TypeError> {
        if expr.ty != Type::Simple(BaseType::Float) {
            return Err(TypeError::Expected(
                Type::Simple(BaseType::Float),
                expr.ty.clone(),
                pos,
            ));
        }
        Ok(())
    }
}
