#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;
pub mod token;
pub mod lexer;
pub mod pos;
pub mod ast;
pub mod parser;
pub mod pprint;

use lexer::Lexer;
use parser::Parser;

fn main() {
    let input = "12+1.1";

    let tokens = Lexer::new(input).lex();

    // match tokens {
    //     Ok(tokens) => for token in tokens {
    //         println!("{:?}", token);
    //     },
    //     Err(errors) => for e in errors {
    //         println!("{}", e);
    //     },
    // };

    println!("{:?}", tokens);

    let ast = Parser::new(tokens.unwrap()).parse_single();

    println!("{:?}", ast);
}
