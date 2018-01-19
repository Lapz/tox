use super::TyChecker;
use syntax::ast::expr::{Expression, Literal, UnaryOperator};
use util::pos::WithPos;
use util::env::{Entry, TypeEnv};
use util::types::{Type, TypeError};
use super::InferedType;

impl TyChecker {
    pub fn transform_expression(
        &mut self,
        expr: &WithPos<Expression>,
        env: &mut TypeEnv,
    ) -> Result<InferedType, TypeError> {
        match expr.node {
            Expression::Array { ref items, ref len } => {
                if items.is_empty() {
                    return Ok(InferedType {
                        ty: Type::Array(Box::new(Type::Nil), 0),
                    });
                }

                let first_ty = self.transform_expression(&items[0], env)?;

                for item in items {
                    let exp_ty = &self.transform_expression(item, env)?.ty;
                    self.check_types(&first_ty.ty, exp_ty, item.pos)?;
                }

                Ok(InferedType {
                    ty: Type::Array(Box::new(first_ty.ty), *len),
                })
            }

            Expression::Assign {
                ref value,
                ref kind,
                ref name,
                ..
            } => {
                let ty = self.transform_var(name, expr.pos, env)?;
                use syntax::ast::expr::AssignOperator::*;
                match *kind {
                    Equal => {
                        let value_ty = self.transform_expression(value, env)?;
                        self.check_types(&ty.ty, &value_ty.ty, expr.pos)?;
                        Ok(ty)
                    }
                    MinusEqual | PlusEqual | StarEqual | SlashEqual => {
                        let value_ty = self.transform_expression(value, env)?;
                        value_ty.check_int_float(value.pos)?;

                        self.check_types(&ty.ty, &value_ty.ty, expr.pos)?;
                        Ok(ty)
                    }
                }
            }

            Expression::Binary {
                ref left_expr,
                ref right_expr,
                ref operator,
            } => {
                let left = self.transform_expression(left_expr, env)?;
                let right = self.transform_expression(right_expr, env)?;

                use syntax::ast::expr::Operator;
                match *operator {
                    Operator::BangEqual | Operator::EqualEqual => Ok(bool_type!()),
                    Operator::LessThan
                    | Operator::LessThanEqual
                    | Operator::GreaterThan
                    | Operator::GreaterThanEqual => {
                        left.check_int_float_str(left_expr.pos)?;

                        Ok(bool_type!())
                    }

                    Operator::Plus
                    | Operator::Slash
                    | Operator::Star
                    | Operator::Modulo
                    | Operator::Minus
                    | Operator::Exponential => {
                        let left_ty = left.check_int_float_str(left_expr.pos)?;
                        let right_ty = right.check_int_float_str(right_expr.pos)?;

                        self.check_types(&left_ty.ty, &right_ty.ty, expr.pos)?;

                        Ok(left_ty)
                    }
                }
            }

            Expression::Call {
                ref callee,
                ref arguments,
            } => match callee.node {
                Expression::Var(sym, _) => {
                    if let Some(entry) = env.look_var(sym).cloned() {
                        self.infer_params(&entry, arguments, env, callee.pos)
                    } else {
                        Err(TypeError::UndefindedVar(env.name(sym), callee.pos))
                    }
                }

                Expression::Get {
                    ref object,
                    ref property,
                    ..
                } => {
                    let ty = match object.node {
                        Expression::Var(sym, _) => self.transform_var(&sym, object.pos, env)?,
                        Expression::ClassInstance { ref name, .. } => {
                            if let Some(ty) = env.look_type(*name) {
                                return Ok(InferedType { ty: ty.clone() });
                            }

                            return Err(TypeError::UndefindedClass(env.name(*name), expr.pos));
                        }

                        Expression::Call { .. } => self.transform_expression(object, env)?,
                        _ => unimplemented!(),
                    };

                    match ty.ty {
                        Type::Class {
                            ref name,
                            ref methods,
                            ..
                        } => {
                            let mut found = false;

                            let mut ty = Type::Nil;
                         

                            for (name, method) in methods {
                                if name == property {
                                    found = true;
                                    self.infer_params(method, arguments, env, object.pos)?;

                                    ty = self.get_actual_ty(method)?;
                                }
                            }

                            if !found {
                            return Err(TypeError::NotMethodOrProperty(
                                env.name(*property),
                                expr.pos,
                            ))
                    }

                    Ok(InferedType{ty})

                        },

                        _ => unimplemented!()
                    }
                
                }
                Expression::Call { .. } => self.transform_expression(callee, env),
                _ => Err(TypeError::NotCallable(callee.pos)),
            },

            Expression::ClassInstance {
                ref properties,
                ref name,
            } => {
                let class = self.transform_var(name, expr.pos, env)?;
                match class.ty {
                    Type::Class { ref fields, .. } => {
                        let mut found = false;
                        for (key, value) in fields {
                            for &(ref instance_name, ref instance_val) in properties {
                                if instance_name == key {
                                    found = true;
                                    let instance_val_ty =
                                        self.transform_expression(instance_val, env)?;

                                    self.check_types(value, &instance_val_ty.ty, expr.pos)?;
                                }
                            }

                            if !found {
                                return Err(TypeError::NotMethodOrProperty(
                                    env.name(*key),
                                    expr.pos,
                                ));
                            }
                        }

                        if fields.len() < properties.len() {
                            return Err(TypeError::TooManyProperty(expr.pos));
                        } else if fields.len() > properties.len() {
                            return Err(TypeError::TooLittleProperty(expr.pos));
                        }
                    }
                    _ => unimplemented!(), //TODO CHANGE INTO HARD ERROR
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
                    self.check_types(&first_key_ty.ty, key_ty, expr.pos)?;
                    self.check_types(&first_value_ty.ty, value_ty, expr.pos)?;
                }

                Ok(InferedType {
                    ty: Type::Dict(Box::new(first_key_ty.ty), Box::new(first_value_ty.ty)),
                })
            }

            Expression::Func { .. } => self.infer_func(expr, env),

            Expression::Get {
                ref object,
                ref property,
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
                            if field == property {
                                found = true;
                                ty = field_ty.clone();
                            }
                        }

                        for (method, methods_ty) in methods {
                            if method == property {
                                found = true;
                                ty = match *methods_ty {
                                    Entry::VarEntry(ref t) => t.clone(),
                                    Entry::FunEntry { ref returns, .. } => returns.clone(),
                                }
                            }
                        }

                        if !found {
                            return Err(TypeError::NotMethodOrProperty(
                                env.name(*property),
                                expr.pos,
                            ));
                        }
                    }

                    Type::This(_, ref fields, ref methods) => {
                        let mut found = false;

                        for (field, field_ty) in fields {
                            if field == property {
                                found = true;
                                ty = field_ty.clone();
                            }
                        }

                        for (method, methods_ty) in methods {
                            if method == property {
                                found = true;
                                ty = methods_ty.clone();
                            }
                        }

                        if !found {
                            return Err(TypeError::NotMethodOrProperty(
                                env.name(*property),
                                expr.pos,
                            ));
                        }
                    }

                    ref e => {
                        return Err(TypeError::NotInstanceOrClass(
                            e.clone(),
                            *property,
                            expr.pos,
                        ))
                    }
                }

                Ok(InferedType { ty })
            }

            Expression::Grouping { ref expr } => self.transform_expression(expr, env),

            Expression::IndexExpr {
                ref target,
                ref index,
            } => match target.node {
                Expression::Var(ref symbol, _) => {
                    let target_ty = self.transform_var(symbol, expr.pos, env)?;
                    let index_ty = self.transform_expression(index, env)?;

                    index_ty.check_int(index.pos)?;

                    match target_ty.ty {
                        Type::Array(ref exp_ty, _) => Ok(InferedType {
                            ty: *exp_ty.clone(),
                        }),
                        Type::Str => Ok(str_type!()),
                        _ => Err(TypeError::IndexAble(env.name(*symbol), index.pos)),
                    }
                }
                _ => Err(TypeError::InvalidIndex(index.pos)),
            },

            Expression::Literal(ref literal) => match *literal {
                Literal::Float(_) => Ok(float_type!()),
                Literal::Int(_) => Ok(int_type!()),
                Literal::Str(_) => Ok(str_type!()),
                Literal::True(_) | Literal::False(_) => Ok(bool_type!()),
                Literal::Nil => Ok(nil_type!()),
            },

            Expression::Logical {
                ref left,
                ref right,
                ..
            } => {
                self.transform_expression(left, env)?.check_bool(expr.pos)?;
                self.transform_expression(right, env)?.check_bool(expr.pos)?;
                Ok(bool_type!())
            }

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
                            if field == name {
                                found = true;
                                let value_ty = self.transform_expression(value, env)?;
                                self.check_types(field_ty, &value_ty.ty, expr.pos)?;
                                ty = field_ty.clone();
                            }
                        }

                        for (method, methods_ty) in methods {
                            if method == name {
                                found = true;
                                let value_ty = self.transform_expression(value, env)?;

                                match *methods_ty {
                                    Entry::VarEntry(ref t) => {
                                        self.check_types(t, &value_ty.ty, expr.pos)?
                                    }
                                    Entry::FunEntry {
                                        ref returns,
                                        ref params,
                                    } => self.check_types(
                                        &Type::Func(params.clone(), Box::new(returns.clone())),
                                        &value_ty.ty,
                                        expr.pos,
                                    )?,
                                };

                                ty = match *methods_ty {
                                    Entry::VarEntry(ref t) => t.clone(),
                                    Entry::FunEntry { ref returns, .. } => returns.clone(),
                                };
                            }
                        }

                        if !found {
                            return Err(TypeError::NotMethodOrProperty(env.name(*name), expr.pos));
                        }
                    }

                    Type::This(_, ref fields, ref methods) => {
                        let mut found = false;

                        for (field, field_ty) in fields {
                            if field == name {
                                found = true;
                                let value_ty = self.transform_expression(value, env)?;
                                self.check_types(field_ty, &value_ty.ty, expr.pos)?;
                                ty = field_ty.clone();
                            }
                        }

                        for (method, methods_ty) in methods {
                            if method == name {
                                found = true;
                                let value_ty = self.transform_expression(value, env)?;

                                self.check_types(methods_ty, &value_ty.ty, expr.pos)?;

                                ty = methods_ty.clone();
                            }
                        }

                        if !found {
                            return Err(TypeError::NotMethodOrProperty(env.name(*name), expr.pos));
                        }
                    }

                    ref e => return Err(TypeError::NotInstanceOrClass(e.clone(), *name, expr.pos)),
                }

                Ok(InferedType { ty })
            }

            Expression::Super { .. } => unimplemented!(),

            Expression::This(_) => {
                if let Some(ref this) = self.this {
                    Ok(InferedType { ty: this.clone() })
                } else {
                    Ok(InferedType { ty: Type::Nil })
                }
            }

            Expression::Unary {
                ref expr,
                ref operator,
            } => {
                let expr_ty = self.transform_expression(expr, env)?;

                match *operator {
                    UnaryOperator::Bang => expr_ty.check_bool(expr.pos)?,
                    UnaryOperator::Minus => expr_ty.check_int_float(expr.pos)?,
                };

                Ok(expr_ty)
            }

            Expression::Ternary {
                ref condition,
                ref then_branch,
                ref else_branch,
            } => {
                let condition_ty = self.transform_expression(condition, env)?;
                condition_ty.check_bool(expr.pos)?;

                let then_ty = self.transform_expression(then_branch, env)?;
                let else_ty = self.transform_expression(else_branch, env)?;
                self.check_types(&then_ty.ty, &else_ty.ty, expr.pos)?;

                Ok(InferedType { ty: then_ty.ty })
            }

            Expression::Var(ref symbol, _) => self.transform_var(symbol, expr.pos, env),
        }
    }

    pub fn infer_func(
        &mut self,
        expr: &WithPos<Expression>,
        env: &mut TypeEnv,
    ) -> Result<InferedType, TypeError> {
        match expr.node {
            Expression::Func {
                ref body,
                ref returns,
                ref parameters,
            } => {
                let return_type = if let Some(ref return_ty) = *returns {
                    self.get_type(return_ty, expr.pos, env)?
                } else {
                    Type::Nil
                };

                let mut params_ty = Vec::with_capacity(parameters.len());
                let mut param_names = Vec::with_capacity(parameters.len());

                for &(symbol, ref p_ty) in parameters {
                    params_ty.push(self.get_type(p_ty, expr.pos, env)?);
                    param_names.push(symbol);
                }

                env.begin_scope();

                for (name, ty) in param_names.iter().zip(params_ty.clone()) {
                    env.add_var(*name, Entry::VarEntry(ty));
                }

                let body_ty = self.transform_statement(body, env)?;

                self.check_types(&return_type, &body_ty.ty, expr.pos)?;

                env.end_scope();

                Ok(InferedType {
                    ty: Type::Func(params_ty, Box::new(return_type)),
                })
            }
            _ => unreachable!(),
        }
    }
}
