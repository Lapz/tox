// mod inferexp;
// mod inferstatement;
pub(crate) mod env;

// mod resolver;
mod alias;
mod class;
mod function;
pub(crate) mod types;
mod unify;
mod user_types;

pub(crate) type InferResult<T> = Result<T, ()>;
// pub use self::resolver::Resolver;
use fnv::FnvHashMap;

use std::rc::Rc;

#[derive(Debug)]
pub struct Infer {
    this: types::Type, // for this
    body: types::Type,
    // resolver: self::resolver::Resolver,
}

impl Infer {
    pub fn new() -> Self {
        Self {
            this: self::types::Type::Nil,
            body: self::types::Type::Nil,
            // resolver: self::resolver::Resolver::new(),
        }
    }

    /// Runs type inference returns a version of the ast which has the type of each operation
    pub fn infer(
        &mut self,
        program: ::syntax::ast::Program,
        strings: &Rc<::util::symbol::SymbolFactory>,
        reporter: &mut ::util::emmiter::Reporter,
    ) -> InferResult<::ast::Program> {
        let mut ctx = ::ctx::CompileCtx::new(strings, reporter);

        let mut new_program = super::ast::Program {
            functions: Vec::new(),
            classes:Vec::new()
        };

        for alias in program.aliases.iter() {
            self.infer_alias(alias, &mut ctx)?;
        }

        for class in program.classes {
            new_program.classes.push(self.infer_class(class, &mut ctx)?);
        }

        for function in program.functions{
            new_program.functions.push(self.infer_function(function,&mut ctx)?);
        }

        Ok(new_program)
    }
    //
    // Gets the locals from  the resolver
    // pub fn locals(&self) -> &FnvHashMap<VariableUseHandle, usize> {
    //     &self.resolver.locals
    // }
}
