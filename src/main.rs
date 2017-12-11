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
use symbol::Symbols;

// use interpreter::Interpreter;
use inference::analyse;

fn main() {
    let input = "12.0+10;";

    println!("{}", input);

    let tokens = Lexer::new(input).lex();

    println!("{:#?}", tokens);
    let mut symbols = Symbols::new();

    let ast = Parser::new(tokens.unwrap(), &mut symbols).parse().unwrap();

    Resolver::new().resolve(&ast).unwrap();

    println!("{:#?}",analyse(&ast[0]));

    // let result = Interpreter::new().interpret(&ast).unwrap();

    // println!("{:#?}", result);
}
