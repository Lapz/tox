use ast as t;
use ctx::CompileCtx;

use infer::types::{Method, Property, Type, TypeVar, Unique};
use infer::{Infer, InferResult};
use syntax::ast::Class;
use util::pos::Spanned;

impl Infer {
    pub fn infer_class(
        &mut self,
        class: Spanned<Class>,
        ctx: &mut CompileCtx,
    ) -> InferResult<t::Class> {
        let mut generic_type_vars = Vec::with_capacity(class.value.name.value.type_params.len());
        let mut type_params = Vec::with_capacity(class.value.name.value.type_params.len());

        ctx.begin_scope();

        for symbol in class.value.name.value.type_params.iter() {
            let type_var = TypeVar::new();
            ctx.add_type(symbol.value, Type::Var(type_var));
            generic_type_vars.push(type_var);
            type_params.push(symbol.value)
        }

        let unique = Unique::new();

        let mut property_types = Vec::with_capacity(class.value.fields.len());

        let mut methods_types = Vec::with_capacity(class.value.methods.len());

        if let Some(ref super_class) = class.value.superclass {
            if let Some(ref ty) = ctx.look_type(super_class.value).cloned() {
                match ty {
                    Type::Generic(_, ref ty) => match **ty {
                        Type::Class(_, ref properties, ref methods, _) => {
                            property_types.extend(properties.clone().into_iter());
                            methods_types.extend(methods.clone().into_iter());
                        }
                        _ => {
                            let msg = format!(
                                "The type `{}` is not inheritable.",
                                ty.print(ctx.symbols())
                            );

                            ctx.error(msg, super_class.span);
                            return Err(());
                        }
                    },

                    _ => {
                        let msg =
                            format!("The type `{}` is not inheritable.", ty.print(ctx.symbols()));

                        ctx.error(msg, super_class.span);
                        return Err(());
                    }
                }
            } else {
                let msg = format!(
                    "`{}` is not a inheritable class",
                    ctx.name(super_class.value)
                );

                ctx.error(msg, super_class.span);
                return Err(());
            }
        }

        ctx.add_type(
            class.value.name.value.name.value,
            Type::Generic(
                generic_type_vars.clone(),
                Box::new(Type::Class(
                    class.value.name.value.name.value,
                    property_types.clone(),
                    methods_types.clone(),
                    unique,
                )),
            ),
        ); // For recursive types we need to add the empty struct

        let mut methods = Vec::with_capacity(class.value.methods.len());

        for property in class.value.fields.iter() {
            property_types.push(Property {
                name: property.value.name.value,
                ty: self.trans_type(&property.value.ty, ctx)?,
            })
        }

        ctx.add_type(
            class.value.name.value.name.value,
            Type::Generic(
                generic_type_vars.clone(),
                Box::new(Type::Class(
                    class.value.name.value.name.value,
                    property_types.clone(),
                    methods_types.clone(),
                    unique,
                )),
            ),
        ); // Ensures that if the class returned from a function has the right number of properties

        for method in class.value.methods {
            let fun = self.infer_function(method, ctx)?;
            let mut types: Vec<Type> = fun
                .params
                .clone()
                .into_iter()
                .map(|param| param.ty)
                .collect();

            types.push(fun.returns.clone());

            methods_types.push(Method {
                name: fun.name,
                ty: ctx.look_var(fun.name).unwrap().clone().get_ty(),
            });

            methods.push(fun);
        }

        ctx.end_scope();
        ctx.add_type(
            class.value.name.value.name.value,
            Type::Generic(
                generic_type_vars.clone(),
                Box::new(Type::Class(
                    class.value.name.value.name.value,
                    property_types.clone(),
                    methods_types.clone(),
                    unique,
                )),
            ),
        ); // Adds the type to the global scope

        Ok(t::Class {
            name: class.value.name.value.name.value,
            superclass: class.value.superclass,
            methods,
            properties: property_types,
        })
    }
}
