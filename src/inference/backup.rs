 // pub fn analyse(
    //     &mut self,
    //     statements: &[WithPos<Statement>],
    //     env: &mut Env,
    // ) -> Result<Vec<ExpressionType>, Vec<TypeError>> {
    //     let mut tys = vec![];
    //     let mut errors = vec![];

    //     for statement in statements {
    //         match self.transform_statement(statement, env) {
    //             Ok(ty) => tys.push(ty),
    //             Err(e) => errors.push(e),
    //         }
    //     }

    //     if errors.is_empty() {
    //         Ok(tys)
    //     } else {
    //         Err(errors)
    //     }
    // }

    // fn transform_var(
    //     &self,
    //     symbol: &Symbol,
    //     pos: Postition,
    //     env: &mut Env,
    // ) -> Result<ExpressionType, TypeError> {
    //     match env.look_var(*symbol) {
    //         Some(ty) => Ok(ExpressionType {
    //             exp: (),
    //             ty: actual_type(&get_actual_ty(ty)?).clone(),
    //         }),
    //         None => Err(TypeError::UndefindedVar(env.name(*symbol), pos)),
    //     }
    // }

    // fn transform_statement(
    //     &mut self,
    //     statement: &WithPos<Statement>,
    //     env: &mut Env,
    // ) -> Result<ExpressionType, TypeError> {
    //     match statement.node {
    //         Statement::ExpressionStmt(ref expr) | Statement::Print(ref expr) => {
    //             self.transform_expression(expr, env)?;
    //             Ok(ExpressionType {
    //                 exp: (),
    //                 ty: Type::Nil,
    //             })
    //         }
    //         Statement::Class {
    //             ref name,
    //             ref methods,
    //             ref properties,
    //             ref superclass,
    //         } => {
    //             let mut properties_ty = vec![];
    //             let mut fields = vec![];
    //             let mut class_methods: Vec<(Symbol, Entry)> = vec![];

    //             if let Some(sclass) = *superclass {
    //                 if let Some(mut entry) = env.look_var(sclass) {
    //                     match *entry {
    //                         Entry::VarEntry(ref sty) => match *sty {
    //                             Type::Class {
    //                                 fields: ref sfields,
    //                                 ref methods,
    //                                 ..
    //                             } => {
    //                                 properties_ty.extend(sfields.iter().cloned());
    //                                 class_methods.extend(methods.iter().cloned())
    //                             }

    //                             _ => return Err(TypeError::SuperClass(sclass, statement.pos)),
    //                         },

    //                         _ => return Err(TypeError::SuperClass(sclass, statement.pos)),
    //                     }
    //                 }
    //             }

    //             env.add_type(
    //                 *name,
    //                 Type::Class {
    //                     name: *name,
    //                     fields: vec![],
    //                     methods: vec![],
    //                 },
    //             );

    //             for &(property, ref ty) in properties {
    //                 let ty = get_type(ty, statement.pos, env)?;
    //                 fields.push((property, ty.clone()));
    //                 properties_ty.push((property, ty))
    //             }

    //             self.this = Type::This(*name, fields, vec![]);

    //             env.add_type(
    //                 *name,
    //                 Type::Class {
    //                     name: *name,
    //                     fields: properties_ty.clone(),
    //                     methods: vec![],
    //                 },
    //             );

    //             env.add_var(
    //                 *name,
    //                 Entry::VarEntry(Type::Class {
    //                     name: *name,
    //                     fields: properties_ty.clone(),
    //                     methods: vec![],
    //                 }),
    //             );

    //             for &WithPos { ref node, .. } in methods {
    //                 match *node {
    //                     Statement::Function { ref name, ref body } => {
    //                         match body.node {
    //                             Expression::Func {
    //                                 ref returns,
    //                                 ref parameters,
    //                                 ref body,
    //                             } => {
    //                                 let return_type = if let Some(ref return_ty) = *returns {
    //                                     get_type(return_ty, statement.pos, env)?
    //                                 } else {
    //                                     Type::Nil
    //                                 };

    //                                 match self.this {
    //                                     Type::This(_, _, ref mut methods) => {
    //                                         methods.push((*name, return_type.clone()))
    //                                     }
    //                                     _ => unreachable!(),
    //                                 }

    //                                 let mut param_names = Vec::with_capacity(parameters.len());
    //                                 let mut param_tys = Vec::with_capacity(parameters.len());

    //                                 for &(param, ref p_ty) in parameters {
    //                                     param_tys.push(get_type(p_ty, statement.pos, env)?);
    //                                     param_names.push(param);
    //                                 }

    //                                 env.begin_scope();

    //                                 for (name, ty) in param_names.iter().zip(param_tys.clone()) {
    //                                     env.add_var(*name, Entry::VarEntry(ty));
    //                                 }

    //                                 check_types(
    //                                     &return_type,
    //                                     &self.transform_statement(body, env)?.ty,
    //                                     statement.pos,
    //                                 )?;

    //                                 env.end_scope();

    //                                 class_methods.push((
    //                                     *name,
    //                                     Entry::FunEntry {
    //                                         params: param_tys,
    //                                         returns: return_type.clone(),
    //                                     },
    //                                 ));
    //                             }
    //                             _ => unreachable!(),
    //                         };
    //                     }
    //                     _ => unreachable!(),
    //                 }
    //             }

    //             let ty = Type::Class {
    //                 name: *name,
    //                 methods: class_methods,
    //                 fields: properties_ty,
    //             };

    //             env.add_type(*name, ty.clone());

    //             self.this = Type::Nil;

    //             env.add_var(*name, Entry::VarEntry(ty.clone()));

    //             Ok(ExpressionType { exp: (), ty })
    //         }

    //         Statement::Var(ref symbol, ref expression, ref ty) => {
    //             if let Some(ref expr) = *expression {
    //                 let expr_ty = self.transform_expression(expr, env)?;

    //                 if let Some(ref id) = *ty {
    //                     let ty = get_type(id, statement.pos, env)?;

    //                     check_types(&ty, &expr_ty.ty, statement.pos)?;

    //                     env.add_var(*symbol, Entry::VarEntry(ty.clone()));

    //                     return Ok(ExpressionType { exp: (), ty });
    //                 }

    //                 env.add_var(*symbol, Entry::VarEntry(expr_ty.ty.clone()));

    //                 Ok(expr_ty)
    //             } else {
    //                 if let Some(ref id) = *ty {
    //                     let ty = get_type(id, statement.pos, env)?;

    //                     env.add_var(*symbol, Entry::VarEntry(ty.clone()));

    //                     return Ok(ExpressionType { exp: (), ty });
    //                 }

    //                 Ok(ExpressionType {
    //                     exp: (),
    //                     ty: Type::Nil,
    //                 })
    //             }
    //         }

    //         Statement::Break | Statement::Continue => Ok(ExpressionType {
    //             exp: (),
    //             ty: Type::Nil,
    //         }),

    //         Statement::TypeAlias { ref alias, ref ty } => {
    //             let alias_ty = get_type(ty, statement.pos, env)?;
    //             env.add_type(*alias, Type::Name(*alias, Box::new(alias_ty.clone())));

    //             Ok(ExpressionType {
    //                 exp: (),
    //                 ty: alias_ty,
    //             })
    //         }

    //         Statement::Block(ref expressions) => {
    //             if expressions.is_empty() {
    //                 return Ok(ExpressionType {
    //                     exp: (),
    //                     ty: Type::Nil,
    //                 });
    //             }

    //             env.begin_scope();
    //             for expr in expressions.iter().rev().skip(1) {
    //                 self.transform_statement(expr, env)?;
    //             }

    //             let result = self.transform_statement(expressions.last().unwrap(), env);

    //             env.end_scope();

    //             result
    //         }

    //         Statement::IfStmt {
    //             ref condition,
    //             ref then_branch,
    //             ref else_branch,
    //         } => {
    //             let condition_ty = self.transform_expression(condition, env)?;

    //             check_bool(&condition_ty, statement.pos)?;

    //             let then_ty = self.transform_statement(then_branch, env)?;

    //             if let Some(ref else_statement) = *else_branch {
    //                 let else_ty = self.transform_statement(else_statement, env)?;

    //                 check_types(&then_ty.ty, &else_ty.ty, statement.pos)?;

    //                 return Ok(ExpressionType {
    //                     exp: (),
    //                     ty: then_ty.ty,
    //                 });
    //             }

    //             Ok(then_ty)
    //         }

    //         Statement::WhileStmt {
    //             ref condition,
    //             ref body,
    //         }
    //         | Statement::DoStmt {
    //             ref condition,
    //             ref body,
    //         } => {
    //             let condition_ty = self.transform_expression(condition, env)?;

    //             check_bool(&condition_ty, statement.pos)?;

    //             let body_ty = self.transform_statement(body, env)?;

    //             Ok(body_ty)
    //         }

    //         Statement::ForStmt {
    //             ref initializer,
    //             ref condition,
    //             ref increment,
    //             ref body,
    //         } => {
    //             if let Some(ref init) = *initializer {
    //                 self.transform_statement(init, env)?;
    //             }

    //             if let Some(ref incr) = *increment {
    //                 s_check_int_float(&self.transform_expression(incr, env)?, statement.pos)?;
    //             }

    //             if let Some(ref cond) = *condition {
    //                 check_bool(&self.transform_expression(cond, env)?, statement.pos)?;
    //             }

    //             let body_ty = self.transform_statement(body, env)?;

    //             Ok(body_ty)
    //         }

    //         Statement::Return(ref returns) => {
    //             if let Some(ref expr) = *returns {
    //                 let exp_ty = self.transform_expression(expr, env)?;
    //                 return Ok(exp_ty);
    //             }
    //             Ok(ExpressionType {
    //                 exp: (),
    //                 ty: Type::Nil,
    //             })
    //         }

    //         Statement::Function { ref name, ref body } => {
    //             match body.node {
    //                 Expression::Func {
    //                     ref returns,
    //                     ref parameters,
    //                     ..
    //                 } => {
    //                     let return_type = if let Some(ref return_ty) = *returns {
    //                         get_type(return_ty, statement.pos, env)?
    //                     } else {
    //                         Type::Nil
    //                     };

    //                     let mut param_names = Vec::with_capacity(parameters.len());
    //                     let mut param_ty = Vec::with_capacity(parameters.len());

    //                     for &(param, ref p_ty) in parameters {
    //                         param_ty.push(get_type(p_ty, statement.pos, env)?);
    //                         param_names.push(param);
    //                     }

    //                     env.add_var(
    //                         *name,
    //                         Entry::FunEntry {
    //                             params: param_ty,
    //                             returns: return_type.clone(),
    //                         },
    //                     );
    //                 }
    //                 _ => unreachable!(),
    //             };

    //             let body_ty = self.transform_expression(body, env)?;

    //             Ok(body_ty)
    //         }
    //     }
    // }

    // fn transform_expression(
    //     &mut self,
    //     expr: &WithPos<Expression>,
    //     env: &mut Env,
    // ) -> Result<ExpressionType, TypeError> {
    //     match expr.node {
    //         Expression::Array { ref items } => {
    //             if items.is_empty() {
    //                 return Ok(ExpressionType {
    //                     exp: (),
    //                     ty: Type::Array(Box::new(Type::Nil)),
    //                 });
    //             }

    //             let first_ty = self.transform_expression(&items[0], env)?;

    //             for item in items {
    //                 match check_types(
    //                     &first_ty.ty,
    //                     &self.transform_expression(item, env)?.ty,
    //                     expr.pos,
    //                 ) {
    //                     Ok(_) => (),
    //                     Err(e) => return Err(e),
    //                 }
    //             }

    //             Ok(ExpressionType {
    //                 exp: (),
    //                 ty: Type::Array(Box::new(first_ty.ty)),
    //             })
    //         }

    //         Expression::Assign {
    //             ref value,
    //             ref kind,
    //             ref name,
    //             ..
    //         } => {
    //             let ty = self.transform_var(name, expr.pos, env)?;
    //             use ast::expr::AssignOperator::*;
    //             match *kind {
    //                 Equal => {
    //                     let value_ty = self.transform_expression(value, env)?;
    //                     check_types(&ty.ty, &value_ty.ty, expr.pos)?;
    //                     Ok(ty)
    //                 }
    //                 MinusEqual | PlusEqual | StarEqual | SlashEqual => {
    //                     let value_ty =
    //                         s_check_int_float(&self.transform_expression(value, env)?, value.pos)?;
    //                     check_types(&ty.ty, &value_ty.ty, expr.pos)?;
    //                     Ok(ty)
    //                 }
    //             }
    //         }

    //         Expression::Binary {
    //             ref left_expr,
    //             ref right_expr,
    //             ref operator,
    //         } => {
    //             let left = self.transform_expression(left_expr, env)?;
    //             let right = self.transform_expression(right_expr, env)?;

    //             use ast::expr::Operator;
    //             match *operator {
    //                 Operator::BangEqual | Operator::EqualEqual => Ok(ExpressionType {
    //                     exp: (),
    //                     ty: Type::Bool,
    //                 }),
    //                 Operator::LessThan
    //                 | Operator::LessThanEqual
    //                 | Operator::GreaterThan
    //                 | Operator::GreaterThanEqual => {
    //                     check_int_float_str(&left, &right, left_expr.pos)?;
    //                     Ok(ExpressionType {
    //                         exp: (),
    //                         ty: Type::Bool,
    //                     })
    //                 }

    //                 Operator::Plus
    //                 | Operator::Slash
    //                 | Operator::Star
    //                 | Operator::Modulo
    //                 | Operator::Minus
    //                 | Operator::Exponential => check_int_float_str(&left, &right, left_expr.pos),
    //             }
    //         }

    //         Expression::Call {
    //             ref callee,
    //             ref arguments,
    //         } => {
    //             let callee = match callee.node {
    //                 Expression::Var(sym, _) => sym,
    //                 Expression::Get {
    //                     ref object,
    //                     ref property,
    //                     ..
    //                 } => {
    //                     let mut _symbol = Symbol(0); // For the undefined error message
    //                     let ty = match object.node {
    //                         Expression::Var(ref sym, _) => {
    //                             _symbol = *sym;
    //                             self.transform_var(sym, expr.pos, env)?
    //                         }
    //                         Expression::ClassInstance { ref name, .. } => {
    //                             if let Some(ty) = env.look_type(*name) {
    //                                 return Ok(ExpressionType {
    //                                     exp: (),
    //                                     ty: ty.clone(),
    //                                 });
    //                             }

    //                             return Err(TypeError::UndefindedClass(env.name(*name), expr.pos));
    //                         }

    //                         Expression::Call { .. } => self.transform_expression(object, env)?,
    //                         ref e => unimplemented!("{:#?}", e),
    //                     };

    //                     match ty.ty {
    //                         Type::Class {
    //                             ref methods,
    //                             ref name,
    //                             ..
    //                         } => {
    //                             let mut _found = false;
    //                             for &(ref key, ref value) in methods {
    //                                 if key == property {
    //                                     _found = true;

    //                                     match *value {
    //                                         Entry::VarEntry(ref ty) => {
    //                                             return Ok(ExpressionType {
    //                                                 exp: (),
    //                                                 ty: ty.clone(),
    //                                             })
    //                                         }

    //                                         Entry::FunEntry {
    //                                             ref params,
    //                                             ref returns,
    //                                         } => {
    //                                             for (arg, param) in arguments.iter().zip(params) {
    //                                                 let exp = self.transform_expression(
    //                                                     arg,
    //                                                     &mut env.clone(),
    //                                                 )?;

    //                                                 check_types(param, &exp.ty, expr.pos)?;
    //                                             }
    //                                             return Ok(ExpressionType {
    //                                                 exp: (),
    //                                                 ty: actual_type(returns).clone(),
    //                                             });
    //                                         }
    //                                     }
    //                                 }
    //                             }

    //                             if !_found {
    //                                 if let Some(class) = env.look_type(*name).cloned() {
    //                                     // When the Class type is added to the environment the methods fields is empty
    //                                     // After the first pass the class type with all the methods and fields is added to the  env
    //                                     // so I check to see if the method being called occus their if not error

    //                                     match class {
    //                                         Type::Class { ref methods, .. } => {
    //                                             _found = false;

    //                                             for &(ref key, ref value) in methods {
    //                                                 if key == property {
    //                                                     _found = true;

    //                                                     match value {
    //                                                         &Entry::VarEntry(ref ty) => {
    //                                                             return Ok(ExpressionType {
    //                                                                 exp: (),
    //                                                                 ty: ty.clone(),
    //                                                             })
    //                                                         }

    //                                                         &Entry::FunEntry {
    //                                                             ref params,
    //                                                             ref returns,
    //                                                         } => {
    //                                                             for (arg, param) in
    //                                                                 arguments.iter().zip(params)
    //                                                             {
    //                                                                 let exp = self.transform_expression(
    //                                                     arg,
    //                                                     &mut env.clone(),
    //                                                 )?;

    //                                                                 check_types(
    //                                                                     param,
    //                                                                     &exp.ty,
    //                                                                     expr.pos,
    //                                                                 )?;
    //                                                             }
    //                                                             return Ok(ExpressionType {
    //                                                                 exp: (),
    //                                                                 ty: actual_type(returns)
    //                                                                     .clone(),
    //                                                             });
    //                                                         }
    //                                                     }
    //                                                 }
    //                                             }
    //                                         }

    //                                         _ => unimplemented!(),
    //                                     }
    //                                 }

    //                                 return Err(TypeError::NotMethodOrProperty(
    //                                     env.name(*property),
    //                                     expr.pos,
    //                                 ));
    //                             }
    //                         }

    //                         e => unimplemented!("TODO ADD AN ERROR, {:?} on {}", e, expr.pos),
    //                     }

    //                     return Err(TypeError::UndefindedVar(env.name(_symbol), expr.pos));
    //                 }
    //                 _ => return Err(TypeError::NotCallable(expr.pos)),
    //             };

    //             if let Some(entry) = env.look_var(callee).cloned() {
    //                 match entry {
    //                     Entry::FunEntry {
    //                         ref params,
    //                         ref returns,
    //                     } => {
    //                         for (arg, param) in arguments.iter().zip(params) {
    //                             let exp = self.transform_expression(arg, &mut env.clone())?;

    //                             check_types(param, &exp.ty, expr.pos)?;
    //                         }
    //                         return Ok(ExpressionType {
    //                             exp: (),
    //                             ty: actual_type(returns).clone(),
    //                         });
    //                     }

    //                     Entry::VarEntry(ref ty) => match ty {
    //                         &Type::Class { .. } => return Err(TypeError::NotCallable(expr.pos)),
    //                         e => {
    //                             return Ok(ExpressionType {
    //                                 exp: (),
    //                                 ty: e.clone(),
    //                             })
    //                         }
    //                     },
    //                 }
    //             }

    //             Err(TypeError::UndefindedVar(env.name(callee), expr.pos))
    //         }

    //         Expression::Dict { ref items } => {
    //             if items.is_empty() {
    //                 return Ok(ExpressionType {
    //                     exp: (),
    //                     ty: Type::Dict(Box::new(Type::Nil), Box::new(Type::Nil)),
    //                 });
    //             }

    //             let first_key_ty = self.transform_expression(&items[0].0, env)?;
    //             let first_value_ty = self.transform_expression(&items[0].0, env)?;
    //             for item in items {
    //                 match check_types(
    //                     &first_key_ty.ty,
    //                     &self.transform_expression(&item.0, env)?.ty,
    //                     expr.pos,
    //                 ) {
    //                     Ok(_) => (),
    //                     Err(e) => return Err(e),
    //                 };

    //                 match check_types(
    //                     &first_value_ty.ty,
    //                     &self.transform_expression(&item.1, env)?.ty,
    //                     expr.pos,
    //                 ) {
    //                     Ok(_) => (),
    //                     Err(e) => return Err(e),
    //                 }
    //             }

    //             Ok(ExpressionType {
    //                 exp: (),
    //                 ty: Type::Dict(Box::new(first_key_ty.ty), Box::new(first_value_ty.ty)),
    //             })
    //         }

    //         Expression::Func {
    //             ref body,
    //             ref returns,
    //             ref parameters,
    //         } => {
    //             let return_type = if let Some(ref return_ty) = *returns {
    //                 get_type(return_ty, expr.pos, env)?
    //             } else {
    //                 Type::Nil
    //             };

    //             let mut params_ty = Vec::with_capacity(parameters.len());
    //             let mut param_names = Vec::with_capacity(parameters.len());

    //             for &(symbol, ref p_ty) in parameters {
    //                 params_ty.push(get_type(p_ty, expr.pos, env)?);
    //                 param_names.push(symbol);
    //             }

    //             env.begin_scope();

    //             for (name, ty) in param_names.iter().zip(params_ty.clone()) {
    //                 env.add_var(*name, Entry::VarEntry(ty));
    //             }

    //             let body_ty = self.transform_statement(body, env)?;

    //             check_types(&return_type, &body_ty.ty, expr.pos)?;

    //             env.end_scope();
    //             Ok(ExpressionType {
    //                 exp: (),
    //                 ty: Type::Func(params_ty, Box::new(return_type)),
    //             })
    //         }

    //         Expression::Get {
    //             ref object,
    //             ref property,
    //             ..
    //         } => {
    //             let instance = self.transform_expression(object, env)?;

    //             let mut ty = Type::Nil;

    //             match instance.ty {
    //                 Type::Class {
    //                     ref fields,
    //                     ref methods,
    //                     ..
    //                 } => {
    //                     let mut found = false;

    //                     for prop in fields {
    //                         if prop.0 == *property {
    //                             found = true;
    //                             ty = prop.1.clone();
    //                         }
    //                     }

    //                     for prop in methods {
    //                         if prop.0 == *property {
    //                             found = true;
    //                             ty = match prop.1 {
    //                                 Entry::VarEntry(ref t) => t.clone(),
    //                                 Entry::FunEntry { ref returns, .. } => returns.clone(),
    //                             }
    //                         }
    //                     }

    //                     if !found {
    //                         return Err(TypeError::NotMethodOrProperty(
    //                             env.name(*property),
    //                             expr.pos,
    //                         ));
    //                     }
    //                 }

    //                 Type::This(_, ref fields, ref methods) => {
    //                     let mut found = false;

    //                     for prop in fields {
    //                         if prop.0 == *property {
    //                             found = true;
    //                             ty = prop.1.clone();
    //                         }
    //                     }

    //                     for prop in methods {
    //                         if prop.0 == *property {
    //                             found = true;
    //                             ty = prop.1.clone();
    //                         }
    //                     }

    //                     if !found {
    //                         return Err(TypeError::NotMethodOrProperty(
    //                             env.name(*property),
    //                             expr.pos,
    //                         ));
    //                     }
    //                 }

    //                 ref e => {
    //                     return Err(TypeError::NotInstanceOrClass(
    //                         e.clone(),
    //                         *property,
    //                         expr.pos,
    //                     ))
    //                 }
    //             }

    //             Ok(ExpressionType { exp: (), ty })
    //         }

    //         Expression::Grouping { ref expr } => self.transform_expression(expr, env),

    //         Expression::IndexExpr {
    //             ref target,
    //             ref index,
    //         } => match target.node {
    //             Expression::Var(ref symbol, _) => {
    //                 let target_ty = self.transform_var(symbol, expr.pos, env)?;
    //                 let index_ty = self.transform_expression(index, env)?;

    //                 check_int(&index_ty, index.pos)?;

    //                 match target_ty.ty {
    //                     Type::Array(ref exp_ty) => Ok(ExpressionType {
    //                         exp: (),
    //                         ty: *exp_ty.clone(),
    //                     }),

    //                     Type::Str => Ok(ExpressionType {
    //                         exp: (),
    //                         ty: Type::Str,
    //                     }),
    //                     _ => Err(TypeError::IndexAble(env.name(*symbol), index.pos)),
    //                 }
    //             }

    //             _ => Err(TypeError::InvalidIndex(expr.pos)),
    //         },

    //         Expression::ClassInstance {
    //             ref properties,
    //             ref name,
    //         } => {
    //             let class = self.transform_var(name, expr.pos, env)?;
    //             match class.ty {
    //                 Type::Class { ref fields, .. } => {
    //                     let mut found = true;
    //                     for &(ref key, ref value) in fields {
    //                         for &(ref instance_name, ref instance_val) in properties {
    //                             if instance_name == key {
    //                                 found = true;

    //                                 let instance_val_ty =
    //                                     self.transform_expression(instance_val, env)?;

    //                                 check_types(value, &instance_val_ty.ty, expr.pos)?;
    //                             }
    //                         }

    //                         if !found {
    //                             return Err(TypeError::NotMethodOrProperty(
    //                                 env.name(*key),
    //                                 expr.pos,
    //                             ));
    //                         }
    //                     }

    //                     if fields.len() < properties.len() {
    //                         return Err(TypeError::TooManyProperty(expr.pos));
    //                     } else if fields.len() > properties.len() {
    //                         return Err(TypeError::TooLittleProperty(expr.pos));
    //                     }
    //                 }
    //                 _ => unimplemented!(), //TODO CHANGE INTO HARD ERROR
    //             };

    //             Ok(class)
    //         }

    //         Expression::Literal(ref literal) => match *literal {
    //             Literal::Float(_) => Ok(ExpressionType {
    //                 exp: (),
    //                 ty: Type::Float,
    //             }),
    //             Literal::Int(_) => Ok(ExpressionType {
    //                 exp: (),
    //                 ty: Type::Int,
    //             }),
    //             Literal::Str(_) => Ok(ExpressionType {
    //                 exp: (),
    //                 ty: Type::Str,
    //             }),
    //             Literal::True(_) | Literal::False(_) => Ok(ExpressionType {
    //                 exp: (),
    //                 ty: Type::Bool,
    //             }),
    //             Literal::Nil => Ok(ExpressionType {
    //                 exp: (),
    //                 ty: Type::Nil,
    //             }),
    //         },

    //         Expression::Unary {
    //             ref expr,
    //             ref operator,
    //         } => {
    //             let expr_ty = self.transform_expression(expr, env)?;

    //             match *operator {
    //                 UnaryOperator::Bang => check_bool(&expr_ty, expr.pos)?,
    //                 UnaryOperator::Minus => {
    //                     s_check_int_float(&expr_ty, expr.pos)?;
    //                 }
    //             };

    //             Ok(expr_ty)
    //         }

    //         Expression::This(_) => Ok(ExpressionType {
    //             exp: (),
    //             ty: self.this.clone(),
    //         }),

    //         Expression::Super { .. } => unimplemented!(),

    //         Expression::Logical {
    //             ref left,
    //             ref right,
    //             ..
    //         } => {
    //             let left_ty = self.transform_expression(left, env)?;
    //             check_bool(&left_ty, expr.pos)?;
    //             let right_ty = self.transform_expression(right, env)?;
    //             check_bool(&right_ty, expr.pos)?;

    //             Ok(ExpressionType {
    //                 exp: (),
    //                 ty: Type::Bool,
    //             })
    //         }

    //         Expression::Set {
    //             ref object,
    //             ref name,
    //             ref value,
    //             ..
    //         } => {
    //             let instance = self.transform_expression(object, env)?;
    //             let mut ty = Type::Nil;

    //             match instance.ty {
    //                 Type::Class {
    //                     ref fields,
    //                     ref methods,
    //                     ..
    //                 } => {
    //                     let mut found = false;

    //                     for prop in fields {
    //                         if prop.0 == *name {
    //                             found = true;
    //                             let value_ty = self.transform_expression(value, env)?;
    //                             check_types(&prop.1, &value_ty.ty, expr.pos)?;
    //                             ty = prop.1.clone();
    //                         }
    //                     }

    //                     for prop in methods {
    //                         if prop.0 == *name {
    //                             found = true;
    //                             let value_ty = self.transform_expression(value, env)?;

    //                             match prop.1 {
    //                                 Entry::VarEntry(ref t) => {
    //                                     check_types(t, &value_ty.ty, expr.pos)?
    //                                 }
    //                                 Entry::FunEntry {
    //                                     ref returns,
    //                                     ref params,
    //                                 } => check_types(
    //                                     &Type::Func(params.clone(), Box::new(returns.clone())),
    //                                     &value_ty.ty,
    //                                     expr.pos,
    //                                 )?,
    //                             };

    //                             ty = match prop.1 {
    //                                 Entry::VarEntry(ref t) => t.clone(),
    //                                 Entry::FunEntry { ref returns, .. } => returns.clone(),
    //                             };
    //                         }
    //                     }

    //                     if !found {
    //                         return Err(TypeError::NotMethodOrProperty(env.name(*name), expr.pos));
    //                     }
    //                 }

    //                 Type::This(_, ref fields, ref methods) => {}

    //                 _ => unreachable!(),
    //             }

    //             Ok(ExpressionType { exp: (), ty })
    //         }

    //         Expression::Ternary {
    //             ref condition,
    //             ref then_branch,
    //             ref else_branch,
    //         } => {
    //             let condition_ty = self.transform_expression(condition, env)?;
    //             check_bool(&condition_ty, expr.pos)?;

    //             let then_ty = self.transform_expression(then_branch, env)?;
    //             let else_ty = self.transform_expression(else_branch, env)?;
    //             check_types(&then_ty.ty, &else_ty.ty, expr.pos)?;

    //             Ok(ExpressionType {
    //                 exp: (),
    //                 ty: then_ty.ty,
    //             })
    //         }

    //         Expression::Var(ref symbol, _) => self.transform_var(symbol, expr.pos, env),
    //     }
    // }