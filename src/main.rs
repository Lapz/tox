
pub mod token;
// pub mod lexer;
pub mod syntex;

use syntex::Lexer;


fn main() {
    let input = 
"{}(),;:+= -= - <= >= 
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
            println!("{:?}",token);
        },
        Err(errors) => for e in errors {
            println!("{}", e);
        },
    };
}
