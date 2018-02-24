use super::TyChecker;
use syntax::ast::expr::{Expression, Literal, UnaryOp};
use util::pos::Spanned;
use util::env::{Entry, TypeEnv};
use util::types::{Type, TypeError};
use super::{InferResult, InferedType};

impl TyChecker {
    pub fn transform_expression(
        &mut self,
        expr: &Spanned<Expression>,
        env: &mut TypeEnv,
    ) -> InferResult<InferedType> {
        match expr.value {
            Expression::Array { ref items } => {
                if items.is_empty() {
                    return Ok(InferedType {
                        ty: Type::Array(Box::new(Type::Nil)),
                    });
                }

                let first_ty = self.transform_expression(&items[0], env)?;

                for item in items {
                    let exp_ty = &self.transform_expression(item, env)?.ty;
                    self.check_types(&first_ty.ty, exp_ty, item.span)?;
                }

                Ok(InferedType {
                    ty: Type::Array(Box::new(first_ty.ty)),
                })
            }

            Expression::Assign {
                ref value,
                ref kind,
                ref name,
                ..
            } => {
                let ty = self.transform_var(name, expr.span, env)?;
                use syntax::ast::expr::AssignOperator::*;
                match kind.value {
                    Equal => {
                        let value_ty = self.transform_expression(value, env)?;
                        self.check_types(&ty.ty, &value_ty.ty, expr.span)?;
                        Ok(ty)
                    }
                    MinusEqual | PlusEqual | StarEqual | SlashEqual => {
                        let value_ty = self.transform_expression(value, env)?;
                        self.check_int_float(&value_ty, value.span)?;
                        self.check_types(&ty.ty, &value_ty.ty, expr.span)?;
                        Ok(ty)
                    }
                }
            }

            Expression::Binary {
                ref lhs,
                ref rhs,
                ref op,
            } => {
                let left = self.transform_expression(lhs, env)?;
                let right = self.transform_expression(rhs, env)?;

                use syntax::ast::expr::Op;
                match op.value {
                    Op::BangEqual | Op::EqualEqual => Ok(bool_type!()),
                    Op::LessThan | Op::LessThanEqual | Op::GreaterThan | Op::GreaterThanEqual => {
                        self.check_int_float_str(&left, lhs.span)?;
                        Ok(bool_type!())
                    }

                    Op::Plus | Op::Slash | Op::Star | Op::Modulo | Op::Minus | Op::Exponential => {
                        let left_ty = self.check_int_float_str(&left, lhs.span)?;
                        let right_ty = self.check_int_float_str(&right, rhs.span)?;

                        self.check_types(&left_ty.ty, &right_ty.ty, expr.span)?;

                        Ok(left_ty)
                    }

                    _ => unimplemented!(),
                }
            }

            // Expression::Call {
            //     ref callee,
            //     ref args,
            // } => {
            //     use util::symbol::Symbol;
            //     let callee = match callee.value {
            //         Expression::Var(sym, _) => sym,
            //         Expression::Get {
            //             ref object,
            //             ref property,
            //             ..
            //         } => {
            //             let mut _symbol = Symbol(0); // For the undefined error message
            //             let ty = match object.value {
            //                 Expression::Var(ref sym, _) => {
            //                     _symbol = *sym;
            //                     self.transform_var(sym, expr.span, env)?
            //                 }
            //                 Expression::ClassInstance { ref name, .. } => {
            //                     if let Some(ty) = env.look_type(*name) {
            //                         return Ok(InferedType { ty: ty.clone() });
            //                     }

            //                     return Err(TypeError::UndefinedClass(env.name(*name), expr.span));
            //                 }

            //                 Expression::Call { .. } => {
            //                     return self.transform_expression(callee, env)
            //                 }
            //                 ref e => unimplemented!("{:?}", e),
            //             };

            //             match ty.ty {
            //                 Type::Class {
            //                     ref methods,
            //                     ref name,
            //                     ..
            //                 } => {
            //                     let mut _found = false;
            //                     for (key, value) in methods {
            //                         if key == property {
            //                             _found = true;

            //                             return self.infer_params(value, args, env, callee.span);
            //                         }
            //                     }

            //                     if !_found {
            //                         if let Some(class) = env.look_type(*name).cloned() {
            //                             // When the Class type is added to the environment the methods fields is empty
            //                             // After the first pass the class type with all the methods and fields is added to the  env
            //                             // so I check to see if the method being called occus their if not error

            //                             match class {
            //                                 Type::Class { ref methods, .. } => {
            //                                     _found = false;
            //                                     for (_, value) in methods {
            //                                         return self.infer_params(
            //                                             value,
            //                                             arguments,
            //                                             env,
            //                                             callee.span,
            //                                         );
            //                                     }
            //                                 }
            //                                 _ => unimplemented!(),
            //                             }
            //                         }

            //                         return Err(TypeError::NotMethodOrProperty(
            //                             env.name(*property),
            //                             expr.span,
            //                         ));
            //                     }
            //                 }

            //                 e => unimplemented!("TODO ADD AN ERROR, {:?} on {}", e, expr.span),
            //             }
            //             return Err(TypeError::UndefinedVar(env.name(_symbol), expr.span));
            //         }
            //         _ => return Err(TypeError::NotCallable(expr.span)),
            //     };

            //     if let Some(entry) = env.look_var(callee).cloned() {
            //         match entry {
            //             Entry::FunEntry {
            //                 ref params,
            //                 ref returns,
            //             } => {
            //                 for (arg, param) in arguments.iter().zip(params) {
            //                     let exp = self.transform_expression(arg, &mut env.clone())?;

            //                     self.check_types(&param, &exp.ty, expr.span)?;
            //                 }
            //                 return Ok(InferedType {
            //                     ty: self.actual_type(returns).clone(),
            //                 });
            //             }

            //             Entry::VarEntry(ref ty) => match ty {
            //                 &Type::Class { .. } => return Err(TypeError::NotCallable(expr.span)),
            //                 e => return Ok(InferedType { ty: e.clone() }),
            //             },
            //         }
            //     }

            //     Err(TypeError::UndefinedVar(env.name(callee), expr.span))
            // }
            Expression::ClassInstance {
                ref props,
                ref symbol,
            } => {
                let class = self.transform_var(symbol, expr.span, env)?;
                match class.ty {
                    Type::Class { ref fields, .. } => {
                        let mut found = false;
                        for (key, value) in fields {
                            for instance_field in props.iter() {
                                if &instance_field.value.symbol.value == key {
                                    found = true;
                                    let instance_val_ty =
                                        self.transform_expression(&instance_field.value.expr, env)?;

                                    self.check_types(value, &instance_val_ty.ty, expr.span)?;
                                }
                            }

                            if !found {
                                let msg = format!("Undefined method/property '{}'", env.name(*key));
                                return Err(());
                            }
                        }

                        if fields.len() < props.len() {
                            self.error("Expected more fields", expr.span);
                            return Err(());
                        } else if fields.len() > props.len() {
                            self.error("Expected less fields", expr.span);
                            return Err(());
                        }
                    }
                    ref e => unimplemented!("{:?}", e), //TODO CHANGE INTO HARD ERROR
                };

                Ok(class)
            }

            Expression::Dict { ref items } => {
                if items.is_empty() {
                    return Ok(InferedType {
                        ty: Type::Dict(Box::new(Type::Nil), Box::new(Type::Nil)),
                    });
                }

                let first_key_ty = self.transform_expression(&items[0].0, env)?;
                let first_value_ty = self.transform_expression(&items[0].0, env)?;

                for item in items {
                    let key_ty = &self.transform_expression(&item.0, env)?.ty;
                    let value_ty = &self.transform_expression(&item.1, env)?.ty;
                    self.check_types(&first_key_ty.ty, key_ty, expr.span)?;
                    self.check_types(&first_value_ty.ty, value_ty, expr.span)?;
                }

                Ok(InferedType {
                    ty: Type::Dict(Box::new(first_key_ty.ty), Box::new(first_value_ty.ty)),
                })
            }

            Expression::Func { .. } => self.infer_func(expr, env),

            // Expression::Get {
            //     ref object,
            //     ref property,
            //     ..
            // } => {
            //     let instance = self.transform_expression(object, env)?;

            //     let mut ty = Type::Nil;

            //     match instance.ty {
            //         Type::Class { ref name, .. } => {
            //             if let Some(class) = env.look_type(*name).cloned() {
            //                 match class {
            //                     Type::Class {
            //                         ref fields,
            //                         ref methods,
            //                         ..
            //                     } => {
            //                         for (field, field_ty) in fields {
            //                             if field == property {
            //                                 ty = field_ty.clone();
            //                             }
            //                         }

            //                         for (method, methods_ty) in methods {
            //                             if method == property {
            //                                 ty = match *methods_ty {
            //                                     Entry::VarEntry(ref t) => t.clone(),
            //                                     Entry::FunEntry { ref returns, .. } => {
            //                                         returns.clone()
            //                                     }
            //                                 }
            //                             }
            //                         }
            //                     }
            //                     _ => unimplemented!(),
            //                 }
            //             } else {
            //                 return Err(TypeError::NotMethodOrProperty(
            //                     env.name(*property),
            //                     expr.span,
            //                 ));
            //             }
            //         }

            //         Type::This(_, ref fields, ref methods) => {
            //             let mut found = false;

            //             for (field, field_ty) in fields {
            //                 if field == property {
            //                     found = true;
            //                     ty = field_ty.clone();
            //                 }
            //             }

            //             for (method, methods_ty) in methods {
            //                 if method == property {
            //                     found = true;
            //                     ty = methods_ty.clone();
            //                 }
            //             }

            //             if !found {
            //                 return Err(TypeError::NotMethodOrProperty(
            //                     env.name(*property),
            //                     expr.span,
            //                 ));
            //             }
            //         }

            //         ref e => {
            //             return Err(TypeError::NotInstanceOrClass(
            //                 e.clone(),
            //                 *property,
            //                 expr.span,
            //             ))
            //         }
            //     }

            //     Ok(InferedType { ty })
            // }
            Expression::Grouping { ref expr } => self.transform_expression(expr, env),

            Expression::Index {
                ref target,
                ref index,
            } => match target.value {
                Expression::Var(ref symbol, _) => {
                    let target_ty = self.transform_var(symbol, expr.span, env)?;
                    let index_ty = self.transform_expression(index, env)?;

                    self.check_int(&index_ty, index.span)?;

                    match target_ty.ty {
                        Type::Array(ref exp_ty) => Ok(InferedType {
                            ty: *exp_ty.clone(),
                        }),
                        Type::Str => Ok(str_type!()),
                        _ => {
                            let msg = format!("'{}' is not an indexable", env.name(symbol.value));
                            self.error(msg, index.span);
                            Err(())
                        }
                    }
                }
                _ => {
                    self.error("Invalid Index Expression", index.span);
                    Err(())
                }
            },

            Expression::Literal(ref literal) => match *literal {
                Literal::Float(_) => Ok(float_type!()),
                Literal::Int(_) => Ok(int_type!()),
                Literal::Str(_) => Ok(str_type!()),
                Literal::True(_) | Literal::False(_) => Ok(bool_type!()),
                Literal::Nil => Ok(nil_type!()),
            },

            // Expression::Set {
            //     ref object,
            //     ref name,
            //     ref value,
            //     ..
            // } => {
            //     let instance = self.transform_expression(object, env)?;
            //     let mut ty = Type::Nil;

            //     match instance.ty {
            //         Type::Class {
            //             ref fields,
            //             ref methods,
            //             ..
            //         } => {
            //             let mut found = false;

            //             for (field, field_ty) in fields {
            //                 if field == name {
            //                     found = true;
            //                     let value_ty = self.transform_expression(value, env)?;
            //                     self.check_types(field_ty, &value_ty.ty, expr.span)?;
            //                     ty = field_ty.clone();
            //                 }
            //             }

            //             for (method, methods_ty) in methods {
            //                 if method == name {
            //                     found = true;
            //                     let value_ty = self.transform_expression(value, env)?;

            //                     match *methods_ty {
            //                         Entry::VarEntry(ref t) => {
            //                             self.check_types(t, &value_ty.ty, expr.span)?
            //                         }
            //                         Entry::FunEntry {
            //                             ref returns,
            //                             ref params,
            //                         } => self.check_types(
            //                             &Type::Func(params.clone(), Box::new(returns.clone())),
            //                             &value_ty.ty,
            //                             expr.span,
            //                         )?,
            //                     };

            //                     ty = match *methods_ty {
            //                         Entry::VarEntry(ref t) => t.clone(),
            //                         Entry::FunEntry { ref returns, .. } => returns.clone(),
            //                     };
            //                 }
            //             }

            //             if !found {
            //                 return Err(TypeError::NotMethodOrProperty(env.name(*name), expr.span));
            //             }
            //         }

            //         Type::This(_, ref fields, ref methods) => {
            //             let mut found = false;

            //             for (field, field_ty) in fields {
            //                 if field == name {
            //                     found = true;
            //                     let value_ty = self.transform_expression(value, env)?;
            //                     self.check_types(field_ty, &value_ty.ty, expr.span)?;
            //                     ty = field_ty.clone();
            //                 }
            //             }

            //             for (method, methods_ty) in methods {
            //                 if method == name {
            //                     found = true;
            //                     let value_ty = self.transform_expression(value, env)?;

            //                     self.check_types(methods_ty, &value_ty.ty, expr.span)?;

            //                     ty = methods_ty.clone();
            //                 }
            //             }

            //             if !found {
            //                 let msg =
            //                     format!("Undefined method/property '{}' ", env.name(name.value));
            //                 self.error(msg, expr.span);
            //                 return Err(());
            //             }
            //         }

            //         ref e => {
            //             let msg = format!(
            //                 "Type {} dosen't have the method/field {}",
            //                 e,
            //                 env.name(name.value)
            //             );
            //             self.error(msg, expr.span);
            //             return Err(());
            //         }
            //     }

            //     Ok(InferedType { ty })
            // }
            Expression::This(_) => {
                if let Some(ref this) = self.this {
                    Ok(InferedType { ty: this.clone() })
                } else {
                    Ok(InferedType { ty: Type::Nil })
                }
            }

            Expression::Unary { ref expr, ref op } => {
                let expr_ty = self.transform_expression(expr, env)?;

                match op.value {
                    UnaryOp::Bang => self.check_bool(&expr_ty, expr.span)?,
                    UnaryOp::Minus => self.check_int_float(&expr_ty, expr.span)?,
                };

                Ok(expr_ty)
            }

            Expression::Ternary {
                ref condition,
                ref then_branch,
                ref else_branch,
            } => {
                let condition_ty = self.transform_expression(condition, env)?;
                self.check_bool(&condition_ty, expr.span)?;

                let then_ty = self.transform_expression(then_branch, env)?;
                let else_ty = self.transform_expression(else_branch, env)?;
                self.check_types(&then_ty.ty, &else_ty.ty, expr.span)?;

                Ok(InferedType { ty: then_ty.ty })
            }

            Expression::Var(ref symbol, _) => self.transform_var(symbol, expr.span, env),

            ref e => {
                self.error("Unimplenmented", expr.span);
                Err(())
            }
        }
    }

    pub fn infer_func(
        &mut self,
        expr: &Spanned<Expression>,
        env: &mut TypeEnv,
    ) -> InferResult<InferedType> {
        match expr.value {
            Expression::Func {
                ref body,
                ref returns,
                ref params,
            } => {
                let return_type = if let Some(ref return_ty) = *returns {
                    self.get_type(return_ty, expr.span, env)?
                } else {
                    Type::Nil
                };

                let mut params_ty = Vec::with_capacity(params.value.len());
                let mut param_names = Vec::with_capacity(params.value.len());

                for function_param in &params.value {
                    params_ty.push(self.get_type(&function_param.value.ty, expr.span, env)?);
                    param_names.push(function_param.value.name.clone());
                }

                env.begin_scope();

                for (name, ty) in param_names.iter().zip(params_ty.clone()) {
                    env.add_var(name.value, Entry::VarEntry(ty));
                }

                let body_ty = self.transform_statement(body, env)?;

                self.check_types(&return_type, &body_ty.ty, expr.span)?;

                env.end_scope();

                Ok(InferedType {
                    ty: Type::Func(params_ty, Box::new(return_type)),
                })
            }
            _ => unreachable!(),
        }
    }
}
