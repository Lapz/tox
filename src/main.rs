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
// pub mod pprint;

use lexer::Lexer;
use parser::Parser;
use interpreter::Interpreter;

fn main() {
    let input = "10 *\"h\"";

    println!("{}",input);

    let tokens = Lexer::new(input).lex();


    println!("{:?}", tokens);

    let ast = Parser::new(tokens.unwrap()).parse_single();

    println!("{:#?}", ast);

    let result = Interpreter::new().interpret(&ast.unwrap()).unwrap();

    println!("{:#?}",result);
}
