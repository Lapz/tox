use ast as t;
use ctx::CompileCtx;

use infer::types::{Variant, Type, TypeVar, Unique};
use infer::{Infer, InferResult};
use syntax::ast::Enum;
use util::pos::Spanned;


impl Infer {
    pub fn infer_enum(
        &mut self,
        _enum:Spanned<Enum>,
        ctx:&mut CompileCtx
    ) -> InferResult<()> {

        let mut generic_type_vars = Vec::with_capacity(_enum.value.name.value.type_params.len());
        let mut type_params = Vec::with_capacity(_enum.value.name.value.type_params.len());

        ctx.begin_scope();

        for symbol in _enum.value.name.value.type_params.iter() {
            let type_var = TypeVar::new();
            ctx.add_type(symbol.value, Type::Var(type_var));
            generic_type_vars.push(type_var);
            type_params.push(symbol.value)
        }

        println!("{}",Type::Generic(
                generic_type_vars.clone(),
                Box::new(Type::Enum{
                    name:_enum.value.name.value.name.value,
                    variants:Vec::new()
                }),
            ).print(ctx.symbols()));

        ctx.add_type(
            _enum.value.name.value.name.value,
            Type::Generic(
                generic_type_vars.clone(),
                Box::new(Type::Enum{
                    name:_enum.value.name.value.name.value,
                    variants:Vec::new()
                }),
            ),
        ); // For recursive types we need to add the empty enum




        unimplemented!()
    }
}