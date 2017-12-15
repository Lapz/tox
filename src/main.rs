#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;
mod token;
mod lexer;
mod pos;
mod ast;
mod parser;
// mod object;
//  mod interpreter;
mod inference;
mod types;
mod resolver;
mod symbol;
mod env;

use lexer::Lexer;
use parser::Parser;
use resolver::Resolver;
use symbol::{SymbolFactory, Symbols};
use env::Env;

// use interpreter::Interpreter;
use inference::analyse;
use std::rc::Rc;

fn main() {
    let input = "var add = fun(a,b) { a+b;};";

    println!("{}", input);

    let tokens = Lexer::new(input).lex();

    println!("{:#?}", tokens);
    let strings = Rc::new(SymbolFactory::new());
    let mut symbols = Symbols::new(Rc::clone(&strings));

    let ast = Parser::new(tokens.unwrap(), &mut symbols).parse().unwrap();

    println!("{:#?}", ast);

    Resolver::new().resolve(&ast).unwrap();

    let mut env = Env::new(&strings);

    println!("{:#?}", analyse(&ast[0], &mut env));
}
