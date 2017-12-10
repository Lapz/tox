#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;
pub mod token;
pub mod lexer;
pub mod pos;
pub mod ast;
pub mod parser;
pub mod object;
pub mod interpreter;
// pub mod inference;
// pub mod types;
pub mod resolver;
pub mod symbol;
// pub mod pprint;

use lexer::Lexer;
use parser::Parser;
// use resolver::Resolver;
use symbol::Symbols;

// use interpreter::Interpreter;
// use inference::analyse;

fn main() {
    let input = "do {print(10);} while (true)
        ";

    println!("{}", input);

    let tokens = Lexer::new(input).lex();


    println!("{:#?}", tokens);
    let mut symbols = Symbols::new();

    let ast = Parser::new(tokens.unwrap(),&mut symbols).parse().unwrap();

    println!("{:#?}", ast);


    // let mut resolver = Resolver::new();

    // println!("{:#?}", resolver.resolve(ast).unwrap());

    // println!("{:#?}", resolver);

    // println!("{:#?}",analyse(&ast));


    // let result = Interpreter::new().interpret(&ast).unwrap();

    // println!("{:#?}", result);
}
