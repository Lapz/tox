mod inferexp;
mod inferstatement;

use syntax::ast::expr::{Expression, Ty};
use syntax::ast::statement::Statement;
use util::symbol::Symbol;
use util::types::Type;
use util::env::{Entry, TypeEnv};
use util::pos::{Span, Spanned};
use util::emmiter::Reporter;

/// The struct that is in control of type checking
#[derive(Debug, Default)]
pub struct TyChecker {
    pub this: Option<Type>,
    reporter: Reporter,
}

pub type InferResult<T> = Result<T, ()>;

#[derive(Debug, PartialEq, Clone)]
pub struct InferedType {
    pub ty: Type,
}

macro_rules! check_type {
    ($sel:ident,$ty:expr,$unknown_ty:expr,$span:expr) => {
        match $ty.ty {
            Type::Func(_,ref returns) => {
                if **returns != $unknown_ty {

                    $sel.expected(&$ty.ty,&$unknown_ty,$span);

                    return Err(());
                }

                Ok(())
            },

            _ =>  {
                if $ty.ty != $unknown_ty {
                 $sel.expected(&$ty.ty,&$unknown_ty,$span);

                    return Err(());
            }

            Ok(())
            }


        }

    }
}

impl TyChecker {
    pub fn new(reporter: Reporter) -> Self {
        TyChecker {
            this: None,
            reporter,
        }
    }

    pub fn analyse(
        &mut self,
        statements: &[Spanned<Statement>],
        env: &mut TypeEnv,
    ) -> Result<(), ()> {
        let mut had_error = false;

        for statement in statements {
            if let Err(_) = self.transform_statement(statement, env) {
                had_error = true;
            }
        }

        if had_error {
            Err(())
        } else {
            Ok(())
        }
    }

    fn error<T: Into<String>>(&mut self, msg: T, span: Span) {
        self.reporter.error(msg, span)
    }

    fn expected(&mut self, expected: &Type, unknown: &Type, span: Span) {
        let msg = format!(
            "Expected Type '{}' but instead got '{}' ",
            expected, unknown
        );

        self.error(msg, span);
    }

    fn transform_var(
        &self,
        symbol: &Spanned<Symbol>,
        env: &mut TypeEnv,
    ) -> InferResult<InferedType> {
        match env.look_var(symbol.value) {
            Some(ty) => Ok(InferedType {
                ty: self.actual_type(&self.get_actual_ty(ty)?).clone(),
            }),
            None => {
                let msg = format!("Undefined variable '{}' ", env.name(symbol.value));
                self.reporter.warn(&msg, symbol.span);
                Err(())
            }
        }
    }

    /// Checks if two types are eqivilant. If the types are two classes it checks the name and says
    /// they are equivilant
    fn check_types(&mut self, expected: &Type, unknown: &Type, span: Span) -> InferResult<()> {
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
                    self.expected(expected, unknown, span);
                    return Err(());
                }
            }

            (&Type::This(ref this, _, _), &Type::Class { ref name, .. })
            | (&Type::Class { ref name, .. }, &Type::This(ref this, _, _)) => {
                if this != name {
                    self.expected(expected, unknown, span);
                    return Err(());
                }
            }

            (e, u) => {
                if expected != unknown && unknown != &Type::Nil {
                    self.expected(e, u, span);
                    return Err(());
                }
            }
        }

        Ok(())
    }

    fn get_actual_ty(&self, entry: &Entry) -> InferResult<Type> {
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
        arguments: &[Spanned<Expression>],
        env: &mut TypeEnv,
        span: Span,
    ) -> InferResult<InferedType> {
        match *entry {
            Entry::FunEntry {
                ref params,
                ref returns,
            } => {
                for (arg, param) in arguments.iter().zip(params) {
                    let exp_ty = self.transform_expression(arg, env)?;

                    self.check_types(&exp_ty.ty, param, span)?;
                }
                Ok(InferedType {
                    ty: self.actual_type(returns).clone(),
                })
            }

            Entry::VarEntry(ref ty) => match ty {
                &Type::Class { .. } => {
                    self.error("Can only call functions and classes", span);
                    Err(())
                }
                rest => Ok(InferedType { ty: rest.clone() }),
            },
        }
    }

    /// Iterativiy walks the the `Ty` and returns a `Type`
    fn get_type(
        &mut self,
        ident: &Spanned<Ty>,
        span: Span,
        env: &mut TypeEnv,
    ) -> InferResult<Type> {
        match ident.value {
            Ty::Simple(ref s) => {
                if let Some(ty) = env.look_type(s.value) {
                    return Ok(ty.clone());
                }

                let msg = format!("Undefined Type '{}'", env.name(s.value));
                self.error(msg, ident.span);
                Err(())
            }
            Ty::Nil => Ok(Type::Nil),
            Ty::Arr(ref s) => Ok(Type::Array(Box::new(self.get_type(s, span, env)?))),
            Ty::Func(ref params, ref returns) => {
                let mut param_tys = Vec::with_capacity(params.len());

                for e_ty in params {
                    param_tys.push(self.get_type(e_ty, span, env)?)
                }

                if let Some(ref ret) = *returns {
                    Ok(Type::Func(
                        param_tys,
                        Box::new(self.get_type(ret, span, env)?),
                    ))
                } else {
                    Ok(Type::Func(param_tys, Box::new(Type::Nil)))
                }
            }
        }
    }

    fn check_int_float(&mut self, ty: &InferedType, span: Span) -> InferResult<()> {
        if self.check_int(ty, span).is_err() && self.check_float(ty, span).is_err() {
            let msg = "Expected one of 'int' or 'float' ";
            self.error(msg, span);
            Err(())
        } else {
            Ok(())
        }
    }

    /// Checks if `InferedType` is {bool}
    fn check_bool(&mut self, ty: &InferedType, span: Span) -> InferResult<()> {
        check_type!(self, ty, Type::Bool, span)
    }

    /// Checks if `InferedType` is {int}
    fn check_int(&mut self, ty: &InferedType, span: Span) -> InferResult<()> {
        check_type!(self, ty, Type::Int, span)
    }

    /// Checks if `InferedType` is {str}
    fn check_str(&mut self, ty: &InferedType, span: Span) -> InferResult<()> {
        check_type!(self, ty, Type::Str, span)
    }

    /// Checks if `InferedType` is {float}
    fn check_float(&mut self, ty: &InferedType, span: Span) -> InferResult<()> {
        if ty.ty != Type::Float {
            self.expected(&Type::Float, &ty.ty, span);
            Err(())
        } else {
            Ok(())
        }
    }

    /// Checks if they {Int,Float,Str}
    fn check_int_float_str(&mut self, ty: &InferedType, span: Span) -> InferResult<InferedType> {
        if self.check_int(ty, span).is_err() || self.check_int(ty, span).is_err() {
            if self.check_float(ty, span).is_ok() {
                self.check_float(ty, span)?;
                Ok(InferedType { ty: Type::Float })
            } else if self.check_str(ty, span).is_ok() {
                // self.check_str span)?;
                self.check_str(ty, span)?;
                Ok(InferedType { ty: Type::Str })
            } else {
                let msg = "Expected onE of 'Int', 'Float', or 'Str'";
                self.error(msg, span);
                Err(())
            }
        } else if self.check_int(ty, span).is_ok() && self.check_int(ty, span).is_ok() {
            Ok(InferedType { ty: Type::Int })
        } else {
            self.expected(&Type::Int, &ty.ty, span);
            Err(())
        }
    }
}
