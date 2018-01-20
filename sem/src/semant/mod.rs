mod inferexp;
mod inferstatement;

use syntax::ast::expr::{Expression, ExpressionTy};
use syntax::ast::statement::Statement;
use util::symbol::Symbol;
use util::types::{Type, TypeError};
use util::env::{Entry, TypeEnv};
use util::pos::{Postition, WithPos};


/// The struct that is in control of type checking
#[derive(Debug, PartialEq, Default)]
pub struct TyChecker {
    pub this: Option<Type>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct InferedType {
    pub ty: Type,
}

impl TyChecker {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn analyse(
        &mut self,
        statements: &[WithPos<Statement>],
        env: &mut TypeEnv,
    ) -> Result<(), Vec<TypeError>> {
        let mut errors = vec![];

        for statement in statements {
            if let Err(e) = self.transform_statement(statement, env) {
                errors.push(e);
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn transform_var(
        &self,
        symbol: &Symbol,
        pos: Postition,
        env: &mut TypeEnv,
    ) -> Result<InferedType, TypeError> {
        
        match env.look_var(*symbol) {
            Some(ty) => Ok(InferedType {
                ty: self.actual_type(&self.get_actual_ty(ty)?).clone(),
            }),
            None => Err(TypeError::UndefinedVar(env.name(*symbol), pos)),
        }
    }

    /// Checks if two types are eqivilant. If the types are two classes it checks the name and says
    /// they are equivilant
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

    fn infer_params(
        &mut self,
        entry: &Entry,
        arguments: &[WithPos<Expression>],
        env: &mut TypeEnv,
        pos: Postition,
    ) -> Result<InferedType, TypeError> {
        match *entry {
            Entry::FunEntry {
                ref params,
                ref returns,
            } => {
                for (arg, param) in arguments.iter().zip(params) {
                    let exp_ty = self.transform_expression(arg, env)?;

                    self.check_types(&exp_ty.ty, param, pos)?;
                }
                Ok(InferedType {
                    ty: self.actual_type(returns).clone(),
                })
            }

            Entry::VarEntry(ref ty) => match ty {
                &Type::Class { .. } => Err(TypeError::NotCallable(pos)),
                rest => Ok(InferedType { ty: rest.clone() }),
            },
        }
    }

    /// Iterativiy walks the the `ExpressionTy` and returns a `Type`
    fn get_type(
        &self,
        ident: &ExpressionTy,
        pos: Postition,
        env: &mut TypeEnv,
    ) -> Result<Type, TypeError> {
        match *ident {
            ExpressionTy::Simple(s) => {
                if let Some(ty) = env.look_type(s) {
                    return Ok(ty.clone());
                }

                Err(TypeError::UndefinedType(env.name(s), pos))
            }
            ExpressionTy::Nil => Ok(Type::Nil),
            ExpressionTy::Arr(ref s) => {
                Ok(Type::Array(Box::new(self.get_type(s, pos, env)?)))
            }
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
                    Ok(Type::Func(param_tys, Box::new(Type::Nil)))
                }
            }
        }
    }
}

macro_rules! check_type {
    ($sel:ident,$ty:expr,$pos:expr) => {
        match $sel.ty {
            Type::Func(_,ref returns) => {
                if **returns != $ty {
                    return Err(TypeError::Expected($ty,$sel.ty.clone(), $pos));
                }

                Ok(())
            },

            _ =>  {
                if $sel.ty != $ty {
            return Err(TypeError::Expected($ty, $sel.ty.clone(), $pos));
            }

            Ok(())
            }

            
        }
        
    }
}

impl InferedType {
    /// Given an `InferedType` check if it an {int} or {float}
    fn check_int_float(&self, pos: Postition) -> Result<(), TypeError> {
        if self.check_int(pos).is_err() && self.check_float(pos).is_err() {
            Err(TypeError::ExpectedOneOf("Int or Float".into(), pos))
        } else {
            Ok(())
        }
    }

    /// Checks if `InferedType` is {bool}
    fn check_bool(&self, pos: Postition) -> Result<(), TypeError> {
        check_type!(self,Type::Bool,pos)
    }

    /// Checks if `InferedType` is {int}
    fn check_int(&self, pos: Postition) -> Result<(), TypeError> {
        check_type!(self,Type::Int,pos)
    }

    /// Checks if `InferedType` is {str}
    fn check_str(&self, pos: Postition) -> Result<(), TypeError> {
        check_type!(self,Type::Str,pos)
    }

       
    
    /// Checks if `InferedType` is {float}
    fn check_float(&self, pos: Postition) -> Result<(), TypeError> {
        if self.ty != Type::Float {
            return Err(TypeError::Expected(Type::Float, self.ty.clone(), pos));
        }
        Ok(())
    }

    /// Checks if they {Int,Float,Str}
    fn check_int_float_str(&self, pos: Postition) -> Result<InferedType, TypeError> {
        if self.check_int(pos).is_err() || self.check_int(pos).is_err() {
            if self.check_float(pos).is_ok() {
                self.check_float(pos)?;
                Ok(InferedType { ty: Type::Float })
            } else if self.check_str(pos).is_ok() {
                // self.check_str pos)?;
                self.check_str(pos)?;
                Ok(InferedType { ty: Type::Str })
            } else {
                Err(TypeError::ExpectedOneOf(
                    "Exepected on of 'Int', 'Float', or 'Str'".into(),
                    pos,
                ))
            }
        } else if self.check_int(pos).is_ok() && self.check_int(pos).is_ok() {
            Ok(InferedType { ty: Type::Int })
        } else {
            Err(TypeError::Expected(Type::Int, self.ty.clone(), pos))
        }
    }
}


