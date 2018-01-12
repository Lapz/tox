mod test;
use ast::expr::*;
use ast::statement::Statement;
use types::{BaseType, Type, TypeError};
use env::{Entry, Env};
use pos::{Postition, WithPos};
use symbol::{Symbol, Table};

#[derive(Debug, PartialEq)]
pub struct ExpressionType {
    pub ty: Type,
}


impl ExpressionType {
     /// Given an `ExpressionType` check if it an {int} or {float}
    fn check_int_float(&self,pos:Postition) -> Result<(),TypeError> {
        if self.check_int(pos).is_err() {
            if self.check_float(pos).is_err() {
                return Err(TypeError::ExpectedOneOf("Int or Float".into()))
            } 
        }
        Ok(())
    }

    /// Checks if `ExpressionType` is {bool}
    fn check_bool(&self,pos: Postition) -> Result<(), TypeError> {
        if self.ty != Type::Simple(BaseType::Bool) {
            return Err(TypeError::Expected(
                Type::Simple(BaseType::Bool),
                self.ty.clone(),
                pos,
            ));
        }
        Ok(())
    }

    /// Checks if `ExpressionType` is {int}
    fn check_int(&self,pos: Postition) -> Result<(), TypeError> {
        if self.ty != Type::Simple(BaseType::Int) {
            return Err(TypeError::Expected(
                Type::Simple(BaseType::Int),
                self.ty.clone(),
                pos,
            ));
        }
        Ok(())
    }

    /// Checks if `ExpressionType` is {str}
    fn check_str(&self,pos: Postition) -> Result<(), TypeError> {
        if self.ty != Type::Simple(BaseType::Str) {
            return Err(TypeError::Expected(
                Type::Simple(BaseType::Str),
                self.ty.clone(),
                pos,
            ));
        }
        Ok(())
    }
    /// Checks if `ExpressionType` is {float}
    fn check_float(&self,pos: Postition) -> Result<(), TypeError> {
        if self.ty != Type::Simple(BaseType::Float) {
            return Err(TypeError::Expected(
                Type::Simple(BaseType::Float),
                self.ty.clone(),
                pos,
            ));
        }
        Ok(())
    }


    /// Given two expression types check if they are {int,int} or they are
    /// {float,float}
    fn check_int_float_str(
        &self,
        left: &ExpressionType,
        right: &ExpressionType,
        pos: Postition,
    ) -> Result<ExpressionType, TypeError> {
        if self.check_int(pos).is_err() || self.check_int( pos).is_err() {
            if self.check_float(pos).is_ok() {
                self.check_float( pos)?;
                Ok(ExpressionType {
                    ty: Type::Simple(BaseType::Float),
                })
            } else if self.check_str(pos).is_ok() {
                // self.check_str pos)?;
                self.check_str( pos)?;
                Ok(ExpressionType {
                    ty: Type::Simple(BaseType::Str),
                })
            } else {
                Err(TypeError::ExpectedOneOf(
                    "Exepected on of 'Int', 'Float', or 'Str'".into(),
                ))
            }
        } else if self.check_int(pos).is_ok() && self.check_int( pos).is_ok() {
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

}
/// The struct that is in control of type checking
#[derive(Debug, PartialEq)]
pub struct TyChecker {
    pub this: Type,
}

impl TyChecker {
    pub fn transform_statement(
        &mut self,
        statement: &WithPos<Statement>,
        env: &mut Env,
    ) -> Result<(), TypeError> {
        match statement.node {
            Statement::Block(ref expressions) => {
                if expressions.is_empty() {
                    return Ok(());
                }

                env.begin_scope();

                for expr in expressions.iter().rev().skip(1) {
                    self.transform_statement(expr, env)?;
                }

                let result = self.transform_statement(expressions.last().unwrap(), env);

                env.end_scope();

                result
            }


            Statement::Break | Statement::Continue => Ok(()),
            Statement::DoStmt {
                ref condition,
                ref body,
            }
            | Statement::WhileStmt {
                ref condition,
                ref body,
            } => {
                self.transform_expression(condition, env)?.check_bool(condition.pos)?;

            
                let body_ty = self.transform_statement(body, env)?;

                Ok(body_ty)
            }

            Statement::ForStmt {
                ref initializer,
                ref condition,
                ref increment,
                ref body,
            } => {
                if let Some(ref init) = *initializer {
                    self.transform_statement(init, env)?;
                }

                if let Some(ref incr) = *increment {
                    
                    self.transform_expression(incr, env)?.check_int_float(incr.pos)?;
                }

                if let Some(ref cond) = *condition {
                    self.transform_expression(cond, env)?.check_bool(cond.pos);
                }

                let body_ty = self.transform_statement(body, env)?;

                Ok(body_ty)
            }

            Statement::ExpressionStmt(ref expr) | Statement::Print(ref expr) => {
                self.transform_expression(expr, env)?;
                Ok(())
            }

            _ => unimplemented!(),
        }
    }
    fn transform_expression(
        &mut self,
        expr: &WithPos<Expression>,
        env: &mut Env,
    ) -> Result<ExpressionType, TypeError> {
        unimplemented!()
    }
}

impl TyChecker {
    pub fn new() -> Self {
        TyChecker {
            this: Type::Simple(BaseType::Nil),
        }
    }

    pub fn analyse(
        &mut self,
        statements: &[WithPos<Statement>],
        env: &mut Env,
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

    /// Iterativiy walks the the `ExpressionTy` and returns a `Type`
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
}
