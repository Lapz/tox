
pub mod token;
// pub mod lexer;
pub mod syntex;

use syntex::{Lexer, LexerError};
use token::{Token, TokenType};

fn main() {
    let input = 
"{}(),;:+= -= - <= >= 
\"hello\"
 {}
//
10.53
10
var hello
±";

    // let tokens = tokenizer(input);

    // println!("{:#?}",tokens);

    for token in tokenizer(input) {
        println!("{:?}", token)
    }
}

fn tokenizer<'input>(
    input: &'input str,
) -> Box<Iterator<Item = Result<Token<'input>, LexerError>> + 'input> where {
    Box::new(Lexer::new(input).take_while(|token| match *token {
        Ok(Token {
            token: TokenType::EOF,
            ..
        }) => false,
        _ => true,
    }))
}
