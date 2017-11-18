#![feature(type_ascription)]
#[macro_use]
extern crate nom;
pub mod token;
pub mod lexer;
// pub mod syntex;
use lexer::Lexer;



fn main() {
    let lexer = Lexer::new(
        "fun(x, y) {
            x + y;
        };±",
    );
    let input = lexer.input;


    println!("{:#?}", lexer.parse(input.as_bytes()));
}

