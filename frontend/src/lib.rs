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
mod ctx;
mod types;
pub(crate) type InferResult<T> = Result<T, ()>;
use util::symbol::Symbols;

// pub use semant::TyChecker;
// pub use env::TypeEnv;
// pub use resolver::Resolver;
