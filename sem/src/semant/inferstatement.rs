use super::TyChecker;
use syntax::ast::statement::Statement;
use util::pos::Spanned;
use util::env::{Entry, TypeEnv};
use util::types::{Type, TypeError};
use std::collections::HashMap;
use util::symbol::Symbol;
use super::{InferResult, InferedType};

impl TyChecker {
    pub fn transform_statement(
        &mut self,
        statement: &Spanned<Statement>,
        env: &mut TypeEnv,
    ) -> InferResult<InferedType> {
        match statement.value {
            Statement::Block(ref expressions) => {
                if expressions.is_empty() {
                    return Ok(nil_type!());
                }

                for expr in expressions {
                    self.transform_statement(expr, env)?;
                }

                let result = self.transform_statement(expressions.last().unwrap(), env);

                result
            }

            Statement::Break | Statement::Continue => Ok(nil_type!()),

            // Statement::Class {
            //     ref name,
            //     ref methods,
            //     ref properties,
            //     ref superclass,
            // } => {
            //     let mut infered_fields = HashMap::new();
            //     let mut fields: HashMap<Symbol, Type> = HashMap::new();
            //     let mut class_methods = HashMap::new();

            //     if let Some(ref sclass) = *superclass {
            //         if let Some(mut entry) = env.look_var(*sclass) {
            //             match *entry {
            //                 Entry::VarEntry(ref sty) => match *sty {
            //                     Type::Class {
            //                         fields: ref sfields,
            //                         ref methods,
            //                         ..
            //                     } => {
            //                         infered_fields.extend(sfields.clone());
            //                         class_methods.extend(methods.clone());
            //                     }

            //                     _ => return Err(TypeError::SuperClass(*sclass, statement.span)),
            //                 },

            //                 _ => return Err(TypeError::SuperClass(*sclass, statement.span)),
            //             }
            //         }
            //     }

            //     for &(property, ref ty) in properties {
            //         let ty = self.get_type(ty, statement.span, env)?;
            //         fields.insert(property, ty.clone());
            //         infered_fields.insert(property, ty);
            //     }

            //     self.this = Some(Type::This(*name, fields, HashMap::new()));

            //     env.add_type(
            //         *name,
            //         Type::Class {
            //             name: *name,
            //             fields: infered_fields.clone(),
            //             methods: HashMap::new(),
            //         },
            //     ); // Add the class as a type so you can return the class
            //        //e.g fn new() -> Class

            //     env.add_var(
            //         *name,
            //         Entry::VarEntry(Type::Class {
            //             name: *name,
            //             fields: infered_fields.clone(),
            //             methods: HashMap::new(),
            //         }),
            //     ); // Add the class as a variable so you can return the class
            //        //e.g fn new() -> Class

            //     for method in methods {
            //         match method.value {
            //             Statement::Function { ref name, ref body } => {
            //                 use syntax::ast::expr::Expression;
            //                 match body.value {
            //                     Expression::Func {
            //                         ref returns,
            //                         ref parameters,
            //                         ref body,
            //                     } => {
            //                         let return_type = if let Some(ref return_ty) = *returns {
            //                             self.get_type(return_ty, statement.span, env)?
            //                         } else {
            //                             Type::Nil
            //                         };

            //                         match self.this {
            //                             Some(Type::This(_, ref mut methods, _)) => {
            //                                 methods.insert(*name, return_type.clone());
            //                             }
            //                             _ => unreachable!(),
            //                         }

            //                         let mut param_names = vec![];
            //                         let mut param_tys = vec![];

            //                         for &(param, ref p_ty) in parameters {
            //                             param_tys.push(self.get_type(p_ty, statement.span, env)?);
            //                             param_names.push(param);
            //                         }

            //                         env.begin_scope();

            //                         for (name, ty) in param_names.iter().zip(param_tys.clone()) {
            //                             env.add_var(*name, Entry::VarEntry(ty));
            //                         }

            //                         let body = &self.transform_statement(body, env)?;

            //                         self.check_types(&return_type, &body.ty, statement.span)?;

            //                         env.end_scope();

            //                         class_methods.insert(
            //                             *name,
            //                             Entry::FunEntry {
            //                                 params: param_tys,
            //                                 returns: return_type.clone(),
            //                             },
            //                         );
            //                     }
            //                     _ => unreachable!(),
            //                 };
            //             }

            //             _ => unreachable!(),
            //         }
            //     }

            //     let ty = Type::Class {
            //         name: *name,
            //         methods: class_methods,
            //         fields: infered_fields,
            //     };

            //     env.add_type(*name, ty.clone());

            //     self.this = None;

            //     env.add_var(*name, Entry::VarEntry(ty.clone()));

            //     Ok(InferedType { ty })
            // }
            
            Statement::While { ref cond, ref body } => {
                let ty = self.transform_expression(cond, env)?;

                self.check_bool(&ty, cond.span)?;

                let body_ty = self.transform_statement(body, env)?;

                Ok(body_ty)
            }

            Statement::Expr(ref expr) | Statement::Print(ref expr) => {
                self.transform_expression(expr, env)?;
                Ok(nil_type!())
            }

            Statement::Function { ref name, ref body } => {
                use syntax::ast::expr::Expression;
                match body.value {
                    Expression::Func {
                        ref returns,
                        ref params,
                        ..
                    } => {
                        let return_type = if let Some(ref return_ty) = *returns {
                            self.get_type(return_ty, statement.span, env)?
                        } else {
                            Type::Nil
                        };

                        let mut param_names = vec![];
                        let mut param_ty = vec![];

                        for function_param in &params.value {
                            param_ty.push(self.get_type(
                                &function_param.value.ty,
                                statement.span,
                                env,
                            )?);
                            param_names.push(function_param.value.name.clone());
                        }

                        env.add_var(
                            name.value,
                            Entry::FunEntry {
                                params: param_ty,
                                returns: return_type.clone(),
                            },
                        );
                    }
                    _ => unreachable!(),
                };

                let body_ty = self.transform_expression(body, env)?;

                Ok(body_ty)
            }

            Statement::ExternFunction {
                ref name,
                ref params,
                ref returns,
            } => {
                let return_type = if let Some(ref return_ty) = *returns {
                    self.get_type(return_ty, statement.span, env)?
                } else {
                    Type::Nil
                };

                let mut param_names = vec![];
                let mut param_ty = vec![];

                for function_param in &params.value {
                    param_ty.push(self.get_type(&function_param.value.ty, statement.span, env)?);
                    param_names.push(function_param.value.name.clone());
                }

                env.add_var(
                    name.value,
                    Entry::FunEntry {
                        params: param_ty,
                        returns: return_type.clone(),
                    },
                );

                Ok(InferedType { ty: return_type })
            }

            Statement::For {
                ref init,
                ref cond,
                ref incr,
                ref body,
            } => {
                if let Some(ref init) = *init {
                    self.transform_statement(init, env)?;
                }

                if let Some(ref incr) = *incr {
                    let ty = self.transform_expression(incr, env)?;
                    self.check_int_float(&ty, incr.span)?;
                }

                if let Some(ref cond) = *cond {
                    let ty = self.transform_expression(cond, env)?;
                    self.check_bool(&ty, cond.span)?;
                }

                let body_ty = self.transform_statement(body, env)?;

                Ok(body_ty)
            }

            Statement::If {
                ref cond,
                ref then,
                ref otherwise,
            } => {
                let ty = self.transform_expression(cond, env)?;
                self.check_bool(&ty, cond.span)?;

                let then_ty = self.transform_statement(then, env)?;

                if let Some(ref else_statement) = *otherwise {
                    let else_ty = self.transform_statement(else_statement, env)?;

                    self.check_types(&then_ty.ty, &else_ty.ty, statement.span)?;

                    Ok(then_ty)
                } else {
                    Ok(then_ty)
                }
            }

            Statement::Return(ref returns) => self.transform_expression(returns, env),

            Statement::TypeAlias { ref alias, ref ty } => {
                let alias_ty = self.get_type(ty, statement.span, env)?;
                env.add_type(
                    alias.value,
                    Type::Name(alias.value, Box::new(alias_ty.clone())),
                );

                Ok(InferedType { ty: alias_ty })
            }

            Statement::Var{ref ident,ref ty, ref expr} => {
                if let Some(ref expr) = *expr {
                    let expr_ty = self.transform_expression(expr, env)?;

                    if let Some(ref id) = *ty {
                        let ty = self.get_type(id, statement.span, env)?;

                        self.check_types(&ty, &expr_ty.ty, statement.span)?;

                        env.add_var(ident.value, Entry::VarEntry(ty));
                        return Ok(InferedType { ty: expr_ty.ty });
                    }

                    env.add_var(ident.value, Entry::VarEntry(expr_ty.ty.clone()));

                    Ok(expr_ty)
                } else {
                    if let Some(ref id) = *ty {
                        let ty = self.get_type(id, statement.span, env)?;

                        env.add_var(ident.value, Entry::VarEntry(ty.clone()));

                        return Ok(InferedType { ty });
                    }

                    Ok(nil_type!())
                }
            },
            _ => unimplemented!(),
        }
    }
}
