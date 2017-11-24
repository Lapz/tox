#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;
pub mod token;
pub mod lexer;
pub mod pos;
pub mod ast;

use lexer::Lexer;


fn main() {
    let input = "{}(),;:+= -= - <= >= 
\"hello\"
 {}
//
10.53
10.0
var hello
";

    let tokens = Lexer::new(input).lex();

    match tokens {
        Ok(tokens) => for token in tokens {
            println!("{:?}", token);
        },
        Err(errors) => for e in errors {
            println!("{}", e);
        },
    };
}
