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
pub mod inference;
pub mod types;
pub mod resolver;
// pub mod pprint;

use lexer::Lexer;
use parser::Parser;
use interpreter::Interpreter;
use inference::analyse;

fn main() {
    let input = "
var a = 0;
var b = 1;

while (a < 10000) {
    print(a);
    var temp = a;
    a = b;
    b = temp + b;
}
";

    println!("{}", input);

    let tokens = Lexer::new(input).lex();


    println!("{:#?}", tokens);

    let ast = Parser::new(tokens.unwrap()).parse().unwrap();

    println!("{:#?}", ast);

    // println!("{:#?}",analyse(&ast));


    // let result = Interpreter::new().interpret(&ast).unwrap();

    // println!("{:#?}", result);
}
