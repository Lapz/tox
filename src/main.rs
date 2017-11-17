#[macro_use]
extern crate nom;
extern crate nom_locate;

pub mod token;
pub mod lexer;
use lexer::Lexer;

fn main() {
    let mut lexer = Lexer::new("fun(x, y) {
            x + y;
        };");
    let input = lexer.input;

    

    println!("{:?}",lexer.lex_tokens(input.as_bytes()));
}
