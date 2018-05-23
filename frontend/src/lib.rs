#![feature(nll)]
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
mod resolver;
// mod test;
mod ast;
mod ctx;
mod infer;
mod types;
mod unify;
pub(crate) type InferResult<T> = Result<T, ()>;

#[derive(Debug)]
pub struct Infer {
    this: self::types::Type,
    body:self::types::Type, // for this
}

use std::rc::Rc;

impl Infer {
    pub fn new() -> Self {
        Self {
            this: self::types::Type::Nil,
            body:self::types::Type::Nil,
        }
    }

    pub fn infer(
        &mut self,
        program: Vec<util::pos::Spanned<syntax::ast::statement::Statement>>,
        strings: &Rc<util::symbol::SymbolFactory>,
        reporter: &mut util::emmiter::Reporter,
    ) -> InferResult<self::ast::Program> {
        let mut ctx = self::ctx::CompileCtx::new(strings, reporter);

        // println!("{:#?}",ctx);
        let mut resolver = self::resolver::Resolver::new();

        let mut new_program = self::ast::Program {
            classes: Vec::new(),
            statements: Vec::new(),
        };

        resolver.resolve(&program, &mut ctx)?;

        for statement in program {
            new_program
                .statements
                .push(self.infer_statement(statement, &mut ctx)?)
            // self.infer_program()
        }

        Ok(new_program)
    }
}
