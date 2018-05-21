#[cfg(test)]
extern crate pretty_assertions;

extern crate fnv;
extern crate rand;
extern crate syntax;
#[macro_use]
extern crate util;

#[macro_use]
// mod semant;
mod env;
// mod resolver;
// mod test;
mod ast;
mod ctx;
mod types;
mod unify;
pub(crate) type InferResult<T> = Result<T, ()>;

#[derive(Debug)]
pub struct Infer {
    this: self::types::Type, // for this
}

use std::rc::Rc;

impl Infer {
    pub fn new() -> Self {
        Self {
            this: self::types::Type::Nil,
        }
    }

    pub fn infer(
        &mut self,
        program: &[syntax::ast::statement::Statement],
        strings: &Rc<util::symbol::SymbolFactory>,
        reporter: &mut util::emmiter::Reporter,
    ) -> InferResult<self::ast::Program> {
        let mut ctx = self::ctx::CompileCtx::new(strings, reporter);

        for statement in program {
            // self.infer_program()
        }

        unimplemented!()
    }
}
