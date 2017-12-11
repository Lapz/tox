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
// pub mod inference;
mod types;
mod resolver;
mod symbol;

use lexer::Lexer;
use parser::Parser;
use resolver::Resolver;
use symbol::Symbols;

// use interpreter::Interpreter;
// use inference::analyse;

fn main() {
    let input = "fun add(a:int,b:int) -> int { return a+b;} fun add(a:int,b:int) -> int { return a+b;} {fun add(a:int,b:int) -> int { return a+b;}}";

    println!("{}", input);

    let tokens = Lexer::new(input).lex();

    println!("{:#?}", tokens);
    let mut symbols = Symbols::new();

    let ast = Parser::new(tokens.unwrap(), &mut symbols).parse().unwrap();

    Resolver::new().resolve(ast).unwrap();

    // println!("{:#?}",analyse(&ast));

    // let result = Interpreter::new().interpret(&ast).unwrap();

    // println!("{:#?}", result);
}
