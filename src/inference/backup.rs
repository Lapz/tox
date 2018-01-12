          

    // fn transform_expression(
    //     &mut self,
    //     expr: &WithPos<Expression>,
    //     env: &mut Env,
    // ) -> Result<InferedType, TypeError> {
    //     match expr.node {
    //         Expression::Array { ref items } => {
    //             if items.is_empty() {
    //                 return Ok(InferedType {
    //                     
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

    //             Ok(InferedType {
    //                 
    //                 ty: Type::Array(Box::new(first_ty.ty)),
    //             })
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
    //                                 return Ok(InferedType {
    //                                     
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
    //                                             return Ok(InferedType {
    //                                                 
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
    //                                             return Ok(InferedType {
    //                                                 
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
    //                                                             return Ok(InferedType {
    //                                                                 
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
    //                                                             return Ok(InferedType {
    //                                                                 
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
    //                         return Ok(InferedType {
    //                             
    //                             ty: actual_type(returns).clone(),
    //                         });
    //                     }

    //                     Entry::VarEntry(ref ty) => match ty {
    //                         &Type::Class { .. } => return Err(TypeError::NotCallable(expr.pos)),
    //                         e => {
    //                             return Ok(InferedType {
    //                                 
    //                                 ty: e.clone(),
    //                             })
    //                         }
    //                     },
    //                 }
    //             }

    //             Err(TypeError::UndefindedVar(env.name(callee), expr.pos))
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

    //             Ok(InferedType {  ty })
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
    //                     Type::Array(ref exp_ty) => Ok(InferedType {
    //                         
    //                         ty: *exp_ty.clone(),
    //                     }),

    //                     Type::Str => Ok(InferedType {
    //                         
    //                         ty: Type::Str,
    //                     }),
    //                     _ => Err(TypeError::IndexAble(env.name(*symbol), index.pos)),
    //                 }
    //             }

    //             _ => Err(TypeError::InvalidIndex(expr.pos)),
    //         },

        //     Expression::ClassInstance {
        //         ref properties,
        //         ref name,
        //     } => {
        //         let class = self.transform_var(name, expr.pos, env)?;
        //         match class.ty {
        //             Type::Class { ref fields, .. } => {
        //                 let mut found = true;
        //                 for &(ref key, ref value) in fields {
        //                     for &(ref instance_name, ref instance_val) in properties {
        //                         if instance_name == key {
        //                             found = true;

        //                             let instance_val_ty =
        //                                 self.transform_expression(instance_val, env)?;

        //                             check_types(value, &instance_val_ty.ty, expr.pos)?;
        //                         }
        //                     }

        //                     if !found {
        //                         return Err(TypeError::NotMethodOrProperty(
        //                             env.name(*key),
        //                             expr.pos,
        //                         ));
        //                     }
        //                 }

        //                 if fields.len() < properties.len() {
        //                     return Err(TypeError::TooManyProperty(expr.pos));
        //                 } else if fields.len() > properties.len() {
        //                     return Err(TypeError::TooLittleProperty(expr.pos));
        //                 }
        //             }
        //             _ => unimplemented!(), //TODO CHANGE INTO HARD ERROR
        //         };

        //         Ok(class)
        //     }

    

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

    //             Ok(InferedType {  ty })
    //         }

        //     Expression::Ternary {
        //         ref condition,
        //         ref then_branch,
        //         ref else_branch,
        //     } => {
        //         let condition_ty = self.transform_expression(condition, env)?;
        //         check_bool(&condition_ty, expr.pos)?;

        //         let then_ty = self.transform_expression(then_branch, env)?;
        //         let else_ty = self.transform_expression(else_branch, env)?;
        //         check_types(&then_ty.ty, &else_ty.ty, expr.pos)?;

        //         Ok(InferedType {
                    
        //             ty: then_ty.ty,
        //         })
        //     }

        //     Expression::Var(ref symbol, _) => self.transform_var(symbol, expr.pos, env),
    //     }
    // }