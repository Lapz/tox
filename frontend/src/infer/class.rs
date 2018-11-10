use ast as t;
use ctx::CompileCtx;
use infer::env::{Entry, VarEntry};
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
        let mut field_types = HashMap::new();
        let mut methods_types = HashMap::new();
        let mut methods = Vec::new();
        let mut fields = Vec::new();

        if let Some(sclass) = class.value.superclass {
            if let Some(mut entry) = ctx.look_type(sclass.value).cloned() {
                match entry {
                    Type::Class(_, ref fields, ref methods, _) => {
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
        ); // Allows for new methods by making the class name available

        for method in class.value.methods {
            let fun = self.infer_function(method, ctx)?;

            methods_types.insert(
                fun.name,
                Entry::Fun(Type::Fun(
                    fun.params
                        .clone()
                        .into_iter()
                        .map(|param| param.ty)
                        .collect(),
                    Box::new(fun.returns.clone()),
                    false
                )),
            );
            methods.push(fun);
        }

        let ty = Type::Class(
            class.value.name.value,
            field_types,
            methods_types,
            Unique::new(),
        );

        self.this = ty.clone();

        ctx.add_type(class.value.name.value, ty.clone());
        ctx.add_var(class.value.name.value, VarEntry::Var(ty));

        Ok(t::Class {
            name: class.value.name.value,
            methods,
            fields,
        })
        // Ok(())
    }
}
