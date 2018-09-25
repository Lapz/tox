use ast as t;
use ctx::CompileCtx;
use infer::types::{Type, Unique};
use infer::{Infer, InferResult};
use std::collections::HashMap;
use syntax::ast::Class;
use util::pos::Spanned;

impl Infer {
    pub fn infer_class(
        &mut self,
        class: Spanned<Class>,
        ctx: &mut CompileCtx,
    ) -> InferResult<t::Class> {
        use std::collections::HashMap;

        let mut field_types = HashMap::new();
        let mut methods_types = HashMap::new();
        let mut methods = Vec::new();
        let mut fields = Vec::new();

        if let Some(sclass) = class.value.superclass {
            if let Some(mut entry) = ctx.look_type(sclass.value).cloned() {
                match entry {
                    Type::Class(ref name, ref fields, ref methods, ref u) => {
                        field_types.extend(fields.clone().into_iter());
                        methods_types.extend(methods.clone().into_iter());
                    }

                    _ => {
                        let msg = format!("The type `{}` is not inheritable.", entry.print(ctx));

                        ctx.error(msg, sclass.span);
                        return Err(());
                    }
                }
            } else {
                let msg = format!("`{}` is not a inheritable class", ctx.name(sclass.value));

                ctx.error(msg, sclass.span);
                return Err(());
            }
        }

        for field in class.value.fields.iter() {
            let ty = self.trans_type(&field.value.ty, ctx)?;
            fields.push(t::Field {
                name: field.value.name.value,
                ty: ty.clone(),
            });
            field_types.insert(field.value.name.value, ty);
        }

        self.this = Type::This {
            name: class.value.name.value,
            fields: field_types.clone(),
            methods: HashMap::new(),
        };

        ctx.add_type(
            class.value.name.value,
            Type::Class(
                class.value.name.value,
                field_types.clone(),
                methods_types.clone(),
                Unique::new(),
            ),
        );

        for method in class.value.methods {
            methods.push(self.infer_function(method, ctx)?);
            // if let Statement::Function {
            //     name,
            //     body,
            //     params,
            //     returns,
            // } = method.value
            // {
            //     let returns = if let Some(ty) = returns {
            //         self.trans_type(&ty, ctx)?
            //     } else {
            //         Type::Nil
            //     };

            //     match self.this {
            //         Type::This {
            //             ref mut methods, ..
            //         } => {
            //             methods.insert(name.value, Entry::Ty(returns.clone())); // Allows for use of methods on this
            //         }
            //         _ => unreachable!(),
            //     }

            //     let mut param_types = Vec::with_capacity(params.value.len());
            //     let mut env_types = Vec::with_capacity(params.value.len());

            //     for param in &params.value {
            //         let ty = self.trans_type(&param.value.ty, ctx)?;

            //         env_types.push(ty.clone());
            //         param_types.push(t::FunctionParam {
            //             name: param.value.name.value,
            //             ty,
            //         })
            //     }

            //     ctx.begin_scope();

            //     for param in param_types.iter() {
            //         ctx.add_var(param.name, VarEntry::Var(param.ty.clone()))
            //     }

            //     let span = body.span;
            //     let body = self.infer_statement(*body, ctx)?;

            //     ctx.end_scope();

            //     self.unify(&returns, &self.body, span, ctx)?;

            //     methods_types.insert(
            //         name.value,
            //         Entry::Fun(Type::Fun(
            //             param_types
            //                 .clone()
            //                 .into_iter()
            //                 .map(|param| param.ty)
            //                 .collect(),
            //             Box::new(returns.clone()),
            //         )),
            //     );

            //     methods.push(t::Statement::Function {
            //         name: name.value,
            //         params: param_types,
            //         body: Box::new(body),
            //         returns: returns,
            //     })
            // }
        }

        let ty = Type::Class(
            class.value.name.value,
            field_types,
            methods_types,
            Unique::new(),
        );

        self.this = ty.clone();

        ctx.add_type(class.value.name.value, ty);

        Ok(t::Class {
            name: class.value.name.value,
            methods,
            fields,
        })
        // Ok(())
    }
}
