// mod inferexp;
// mod inferstatement;
pub(crate) mod env;
mod expression;
mod resolver;
mod statements;
pub(crate) mod types;
mod unify;
mod user_types;

pub(crate) type InferResult<T> = Result<T, ()>;
pub use self::resolver::Resolver;
use fnv::FnvHashMap;
use syntax::ast::expr::VariableUseHandle;

use std::rc::Rc;

#[derive(Debug)]
pub struct Infer {
    this: types::Type, // for this
    body: types::Type,
    resolver: self::resolver::Resolver,
}

impl Infer {
    pub fn new() -> Self {
        Self {
            this: self::types::Type::Nil,
            body: self::types::Type::Nil,
            resolver: self::resolver::Resolver::new(),
        }
    }

    /// Runs type inference returns a version of the ast which has the type of each operation
    pub fn infer(
        &mut self,
        program: Vec<::util::pos::Spanned<::syntax::ast::statement::Statement>>,
        strings: &Rc<::util::symbol::SymbolFactory>,
        reporter: &mut ::util::emmiter::Reporter,
    ) -> InferResult<::ast::Program> {
        let mut ctx = ::ctx::CompileCtx::new(strings, reporter);

        let mut new_program = ::ast::Program {
            statements: Vec::new(),
        };

        self.resolver.resolve(&program, &mut ctx)?;

        for statement in program {
            new_program
                .statements
                .push(self.infer_statement(statement, &mut ctx)?)
        }

        Ok(new_program)
    }
    /// Gets the locals from  the resolver
    pub fn locals(&self) -> &FnvHashMap<VariableUseHandle, usize> {
        &self.resolver.locals
    }
}
