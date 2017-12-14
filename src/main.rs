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
use symbol::{Symbols,SymbolFactory};
use env::Env;

use std::rc::Rc;
// use interpreter::Interpreter;
use inference::analyse;

fn main() {
    let input = "var a =10;";

    println!("{}", input);

    let tokens = Lexer::new(input).lex();

    println!("{:#?}", tokens);
    let mut symbol_factory = Rc::new(SymbolFactory::new());
    let mut symbols = Symbols::new(symbol_factory);

    let ast = Parser::new(tokens.unwrap(), &mut symbols).parse().unwrap();

    println!("{:#?}", ast);

    Resolver::new().resolve(&ast).unwrap();

    let mut env =  Env {
        types: Symbols::new(symbol_factory),
        vars : Symbols::new(symbol_factory),
    };

    println!("{:#?}", analyse(&ast[0],&mut env));

    // let result = Interpreter::new().interpret(&ast).unwrap();

    // println!("{:#?}", result);
}
