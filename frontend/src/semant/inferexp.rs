use super::TyChecker;
use super::{InferResult, InferedType};
use syntax::ast::expr::{Expression, Literal, UnaryOp};
use env::{Entry, TypeEnv};
use util::pos::Spanned;
use types::Type;

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
                let ty = self.transform_var(name, env)?;
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

            Expression::Call {
                ref callee,
                ref args,
            } => {
                let symbol = match callee.value {
                    Expression::Var(ref sym, _) => sym.value,
                    Expression::Get {
                        ref property,
                        ref object,
                        ..
                    } => {
                        let inferred_ty = match object.value {
                            Expression::Var(ref sym, _) => self.transform_var(sym, env)?,
                            Expression::ClassInstance { ref symbol, .. } => {
                                if let Some(ty) = env.look_type(symbol.value) {
                                    return Ok(InferedType { ty: ty.clone() });
                                }

                                let msg = format!("Undefined Class '{}'", env.name(symbol.value));

                                self.error(msg, object.span);
                                return Err(());
                            }

                            Expression::Call { .. } => {
                                return self.transform_expression(callee, env)
                            }
                            _ => unimplemented!(),
                        };

                        match inferred_ty.ty {
                            Type::Class {
                                ref name,
                                ref methods,
                                ..
                            } => {
                                for (key, value) in methods {
                                    if key == &property.value {
                                        return self.infer_params(value, args, env, callee.span);
                                    }
                                }

                                if let Some(class) = env.look_type(*name).cloned() {
                                    // When the Class type is added to the environment the methods fields is empty
                                    // After the first pass the class type with all the methods and fields is added to the  env
                                    // so I check to see if the method being called occus their if not error

                                    match class {
                                        Type::Class { ref methods, .. } => {
                                            for value in methods.values() {
                                                self.infer_params(value, args, env, callee.span)?;
                                            }
                                        }
                                        _ => unimplemented!(),
                                    }
                                }

                                let msg = format!(
                                    "Undefined method/property '{}' ",
                                    env.name(property.value)
                                );

                                self.error(msg, callee.span);
                                return Err(());
                            }

                            ref e => {
                                let msg = format!("Type '{}' is not callable", e);

                                self.error(msg, callee.span);
                                return Err(());
                            }
                        }
                    }
                    _ => {
                        self.error("Can only call functions and classes", callee.span);
                        return Err(());
                    }
                };

                if let Some(entry) = env.look_var(symbol).cloned() {
                    match entry {
                        Entry::FunEntry {
                            ref params,
                            ref returns,
                        } => {
                            for (arg, param) in args.iter().zip(params) {
                                let exp = self.transform_expression(arg, &mut env.clone())?;

                                self.check_types(param, &exp.ty, expr.span)?;
                            }
                            return Ok(InferedType {
                                ty: self.actual_type(returns).clone(),
                            });
                        }
                        Entry::VarEntry(ref ty) => match ty {
                            func @ &Type::Func { .. } => {
                                return Ok(InferedType { ty: func.clone() })
                            }

                            ty => {
                                let msg = format!(
                                    "Type '{}' is not callable. \n Can only call functions",
                                    ty
                                );
                                self.error(msg, callee.span);
                                return Err(());
                            }
                        },
                    }
                }

                let msg = format!("Undefined variable '{}' ", env.name(symbol));
                self.reporter.error(msg, callee.span);
                Err(())
            }

            Expression::ClassInstance {
                ref props,
                ref symbol,
            } => {
                let class = self.transform_var(symbol, env)?;
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
                                self.error(msg, expr.span);
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

            Expression::Get {
                ref object,
                ref property,
                ..
            } => {
                let instance = self.transform_expression(object, env)?;

                let mut ty = Type::Nil;

                match instance.ty {
                    Type::Class { ref name, .. } => {
                        if let Some(class) = env.look_type(*name).cloned() {
                            match class {
                                Type::Class {
                                    ref fields,
                                    ref methods,
                                    ..
                                } => {
                                    for (field, field_ty) in fields {
                                        if field == &property.value {
                                            ty = field_ty.clone();
                                        }
                                    }

                                    for (method, methods_ty) in methods {
                                        if method == &property.value {
                                            ty = match *methods_ty {
                                                Entry::VarEntry(ref t) => t.clone(),
                                                Entry::FunEntry { ref returns, .. } => {
                                                    returns.clone()
                                                }
                                            }
                                        }
                                    }
                                }
                                _ => unimplemented!(),
                            }
                        } else {
                            let msg =
                                format!("Undefined method/property '{}'", env.name(property.value));
                            self.error(msg, object.span);
                            return Err(());
                        }
                    }

                    Type::This(_, ref fields, ref methods) => {
                        let mut found = false;

                        for (field, field_ty) in fields {
                            if field == &property.value {
                                found = true;
                                ty = field_ty.clone();
                            }
                        }

                        for (method, methods_ty) in methods {
                            if method == &property.value {
                                found = true;
                                ty = methods_ty.clone();
                            }
                        }

                        if !found {
                            let msg =
                                format!("Undefined method/property '{}'", env.name(property.value));
                            self.error(msg, property.span);
                            return Err(());
                        }
                    }

                    ref e => {
                        let msg = format!(
                            "Type {} dosen't have the method/field {}",
                            e,
                            env.name(property.value)
                        );

                        self.error(msg, property.span);
                        return Err(());
                    }
                }

                Ok(InferedType { ty })
            }
            Expression::Grouping { ref expr } => self.transform_expression(expr, env),

            Expression::Index {
                ref target,
                ref index,
            } => match target.value {
                Expression::Var(ref symbol, _) => {
                    let target_ty = self.transform_var(symbol, env)?;
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

            Expression::Set {
                ref object,
                ref name,
                ref value,
                ..
            } => {
                let instance = self.transform_expression(object, env)?;
                let mut ty = Type::Nil;

                match instance.ty {
                    Type::Class {
                        ref fields,
                        ref methods,
                        ..
                    } => {
                        let mut found = false;

                        for (field, field_ty) in fields {
                            if field == &name.value {
                                found = true;
                                let value_ty = self.transform_expression(value, env)?;
                                self.check_types(field_ty, &value_ty.ty, expr.span)?;
                                ty = field_ty.clone();
                            }
                        }

                        for (method, methods_ty) in methods {
                            if method == &name.value {
                                found = true;
                                let value_ty = self.transform_expression(value, env)?;

                                match *methods_ty {
                                    Entry::VarEntry(ref t) => {
                                        self.check_types(t, &value_ty.ty, expr.span)?
                                    }
                                    Entry::FunEntry {
                                        ref returns,
                                        ref params,
                                    } => self.check_types(
                                        &Type::Func(params.clone(), Box::new(returns.clone())),
                                        &value_ty.ty,
                                        expr.span,
                                    )?,
                                };

                                ty = match *methods_ty {
                                    Entry::VarEntry(ref t) => t.clone(),
                                    Entry::FunEntry { ref returns, .. } => returns.clone(),
                                };
                            }
                        }

                        if !found {
                            let msg =
                                format!("Undefined method/property '{}'", env.name(name.value));
                            self.error(msg, name.span);
                            return Err(());
                        }
                    }

                    Type::This(_, ref fields, ref methods) => {
                        let mut found = false;

                        for (field, field_ty) in fields {
                            if field == &name.value {
                                found = true;
                                let value_ty = self.transform_expression(value, env)?;
                                self.check_types(field_ty, &value_ty.ty, expr.span)?;
                                ty = field_ty.clone();
                            }
                        }

                        for (method, methods_ty) in methods {
                            if method == &name.value {
                                found = true;
                                let value_ty = self.transform_expression(value, env)?;

                                self.check_types(methods_ty, &value_ty.ty, expr.span)?;

                                ty = methods_ty.clone();
                            }
                        }

                        if !found {
                            let msg =
                                format!("Undefined method/property '{}' ", env.name(name.value));
                            self.error(msg, expr.span);
                            return Err(());
                        }
                    }

                    ref e => {
                        let msg = format!(
                            "Type {} dosen't have the method/field {}",
                            e,
                            env.name(name.value)
                        );
                        self.error(msg, expr.span);
                        return Err(());
                    }
                }

                Ok(InferedType { ty })
            }
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

            Expression::Var(ref symbol, _) => self.transform_var(symbol, env),
        }
    }
}
