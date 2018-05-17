#[cfg(test)]
extern crate pretty_assertions;

extern crate fnv;
extern crate rand;
extern crate syntax;
#[macro_use]
extern crate util;

#[macro_use]
 mod semant;
mod resolver;
mod env;
mod types;
mod test;

pub use semant::TyChecker;
pub use resolver::Resolver;
pub use env::TypeEnv;