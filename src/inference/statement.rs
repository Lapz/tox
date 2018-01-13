use inference::TyChecker;
use ast::statement::Statement;
use pos::WithPos;
use env::{Entry, Env};
use types::{BaseType, InferedType, Type, TypeError};
use std::collections::HashMap;
use symbol::Symbol;

impl TyChecker {
    pub fn transform_statement(
        &mut self,
        statement: &WithPos<Statement>,
        env: &mut Env,
    ) -> Result<InferedType, TypeError> {
        match statement.node {
            Statement::Block(ref expressions) => {
                if expressions.is_empty() {
                    return Ok(nil_type!());
                }

                env.begin_scope();

                for expr in expressions.iter().rev().skip(1) {
                    self.transform_statement(expr, env)?;
                }

                let result = self.transform_statement(expressions.last().unwrap(), env);

                env.end_scope();

                result
            }

            Statement::Break | Statement::Continue => Ok(nil_type!()),

            Statement::Class {
                ref name,
                ref methods,
                ref properties,
                ref superclass,
            } => {
                let mut infered_fields = HashMap::new();
                let mut fields: HashMap<Symbol, Type> = HashMap::new();
                let mut class_methods = HashMap::new();

                if let Some(sclass) = *superclass {
                    if let Some(mut entry) = env.look_var(sclass) {
                        match *entry {
                            Entry::VarEntry(ref sty) => match *sty {
                                Type::Class {
                                    fields: ref sfields,
                                    ref methods,
                                    ..
                                } => {
                                    infered_fields.extend(sfields.clone());
                                    class_methods.extend(methods.clone());
                                }

                                _ => return Err(TypeError::SuperClass(sclass, statement.pos)),
                            },

                            _ => return Err(TypeError::SuperClass(sclass, statement.pos)),
                        }
                    }
                }

                env.add_type(
                    *name,
                    Type::Class {
                        name: *name,
                        fields: HashMap::new(),
                        methods: HashMap::new(),
                    },
                );

                for &(property, ref ty) in properties {
                    let ty = self.get_type(ty, statement.pos, env)?;
                    fields.insert(property, ty.clone());
                    infered_fields.insert(property, ty);
                }

                self.this = Some(Type::This(*name, fields, HashMap::new()));

                env.add_type(
                    *name,
                    Type::Class {
                        name: *name,
                        fields: infered_fields.clone(),
                        methods: HashMap::new(),
                    },
                ); // Add the class as a type so you can return the class
                   //e.g fn new() -> Class

                env.add_var(
                    *name,
                    Entry::VarEntry(Type::Class {
                        name: *name,
                        fields: infered_fields.clone(),
                        methods: HashMap::new(),
                    }),
                ); // Add the class as a variable so you can return the class
                   //e.g fn new() -> Class

                for method in methods {
                    match method.node {
                        Statement::Function { ref name, ref body } => {
                            let method_ty = self.transform_expression(body, env)?;
                            match method_ty.ty {
                                Type::Func(params, returns) => class_methods.insert(
                                    *name,
                                    Entry::FunEntry {
                                        params,
                                        returns: *returns,
                                    },
                                ),
                                _ => unreachable!(),
                            };
                        }

                        _ => unreachable!(),
                    }
                }

                let ty = Type::Class {
                    name: *name,
                    methods: class_methods,
                    fields: infered_fields,
                };

                env.add_type(*name, ty.clone());

                self.this = None;

                env.add_var(*name, Entry::VarEntry(ty.clone()));

                Ok(InferedType { ty })
            }

            Statement::DoStmt {
                ref condition,
                ref body,
            }
            | Statement::WhileStmt {
                ref condition,
                ref body,
            } => {
                self.transform_expression(condition, env)?
                    .check_bool(condition.pos)?;

                let body_ty = self.transform_statement(body, env)?;

                Ok(body_ty)
            }

            Statement::ExpressionStmt(ref expr) | Statement::Print(ref expr) => {
                self.transform_expression(expr, env)?;
                Ok(nil_type!())
            }

            Statement::Function { ref name, ref body } => {
                use ast::expr::Expression;
                match body.node {
                    Expression::Func {
                        ref returns,
                        ref parameters,
                        ..
                    } => {
                        let return_type = if let Some(ref return_ty) = *returns {
                            self.get_type(return_ty, statement.pos, env)?
                        } else {
                            Type::Simple(BaseType::Nil)
                        };

                        let mut param_names = vec![];
                        let mut param_ty = vec![];

                        for &(param, ref p_ty) in parameters {
                            param_ty.push(self.get_type(p_ty, statement.pos, env)?);
                            param_names.push(param);
                        }

                        env.add_var(
                            *name,
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
                    self.transform_expression(incr, env)?
                        .check_int_float(incr.pos)?;
                }

                if let Some(ref cond) = *condition {
                    self.transform_expression(cond, env)?.check_bool(cond.pos)?;
                }

                let body_ty = self.transform_statement(body, env)?;

                Ok(body_ty)
            }

            Statement::IfStmt {
                ref condition,
                ref then_branch,
                ref else_branch,
            } => {
                self.transform_expression(condition, env)?
                    .check_bool(condition.pos)?;

                let then_ty = self.transform_statement(then_branch, env)?;

                if let Some(ref else_statement) = *else_branch {
                    let else_ty = self.transform_statement(else_statement, env)?;

                    self.check_types(&then_ty.ty, &else_ty.ty, statement.pos)?;

                    Ok(then_ty)
                } else {
                    Ok(then_ty)
                }
            }

            Statement::Return(ref returns) => {
                if let Some(ref expr) = *returns {
                    self.transform_expression(expr, env)
                } else {
                    Ok(nil_type!())
                }
            }

            Statement::TypeAlias { ref alias, ref ty } => {
                let alias_ty = self.get_type(ty, statement.pos, env)?;
                env.add_type(*alias, Type::Name(*alias, Box::new(alias_ty.clone())));

                Ok(InferedType { ty: alias_ty })
            }

            Statement::Var(ref symbol, ref expression, ref ty) => {
                if let Some(ref expr) = *expression {
                    let expr_ty = self.transform_expression(expr, env)?;

                    if let Some(ref id) = *ty {
                        let ty = self.get_type(id, statement.pos, env)?;

                        self.check_types(&ty, &expr_ty.ty, statement.pos)?;

                        env.add_var(*symbol, Entry::VarEntry(ty));
                        return Ok(InferedType { ty: expr_ty.ty });
                    }

                    env.add_var(*symbol, Entry::VarEntry(expr_ty.ty.clone()));

                    Ok(expr_ty)
                } else {
                    if let Some(ref id) = *ty {
                        let ty = self.get_type(id, statement.pos, env)?;

                        env.add_var(*symbol, Entry::VarEntry(ty.clone()));

                        return Ok(InferedType { ty });
                    }

                    Ok(nil_type!())
                }
            }
        }
    }
}
