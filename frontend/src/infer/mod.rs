// mod inferexp;
// mod inferstatement;
pub(crate) mod env;

// mod resolver;
mod alias;
mod class;
mod function;
pub(crate) mod subst;
mod sum;
pub(crate) mod types;
mod unify;
mod user_types;

pub(crate) type InferResult<T> = Result<T, ()>;
// pub use self::resolver::Resolver;
use crate::infer::types::Type;
use std::rc::Rc;
use util::symbol::Symbol;

#[derive(Debug)]
pub struct Infer {
    body: Type,
    main: Option<Symbol>,
}

impl Default for Infer {
    fn default() -> Self {
        Self {
            body: Type::Nil,
            main: None,
        }
    }
}

impl Infer {
    pub fn new() -> Self {
        Self::default()
    }

    /// Runs type inference returns a version of the ast which has the type of each operation
    pub fn infer(
        &mut self,
        program: ::syntax::ast::Program,
        strings: &Rc<::util::symbol::SymbolFactory>,
        reporter: &mut ::util::emmiter::Reporter,
    ) -> InferResult<super::ast::Program> {
        let mut ctx = crate::ctx::CompileCtx::new(strings, reporter);

        let mut new_program = super::ast::Program {
            functions: Vec::new(),
            classes: Vec::new(),
        };

        for alias in program.aliases.iter() {
            self.infer_alias(alias, &mut ctx)?;
        }

        for sum in program.enums {
            self.infer_enum(sum, &mut ctx)?;
        }

        for class in program.classes {
            new_program.classes.push(self.infer_class(class, &mut ctx)?);
        }

        for function in program.functions {
            new_program
                .functions
                .push(self.infer_function(function, &mut ctx)?);
            self.body = Type::Nil; // resets the body for the next run
        }

        if self.main.is_none() {
            ctx.global_error("Main method is missing");
            return Err(());
        }

        Ok(new_program)
    }

    pub fn set_main(&mut self, symbol: Symbol) {
        self.main = Some(symbol)
    }

    pub fn get_main(&mut self) -> Symbol {
        self.main.take().unwrap()
    }
}
