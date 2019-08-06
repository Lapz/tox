mod lexer;
#[macro_use]
mod macros;
mod parser;
mod pos;
mod token;
use crate::lexer::Lexer;
use codespan::{CodeMap, FileMap, FileName, Span};
use parser::Parser;
use rowan::SmolStr;

pub type ParseResult<T> = Result<T, ()>;

fn main() {
    let mut input = "fn main";
    let mut lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer.lex().into_iter(), input);
    parser.parse_function();
    // println!("{:?}",);
    // println!("{:?}", parser.past_tokens);
    // println!("{:?}", parser.bump());
    // println!("{:?}", parser.past_tokens);
    // println!("{:?}", parser.bump());
    // println!("{:?}", parser.past_tokens);
    // println!("{:?}", parser.bump());

    println!("{:?}", parser.builder.finish());

    for token in lexer.lex() {
        println!(
            "{:?},{:?}",
            SmolStr::new(
                &input[token.start.absolute as usize
                    ..(token.value.len + token.start.absolute) as usize]
            ),
            token.value.kind
        )
    }

    match teraron::generate(
        std::path::Path::new("/Users/lenardpratt/Projects/Rust/syntax/syntax/src/ast.rs.tera"),
        std::path::Path::new("/Users/lenardpratt/Projects/Rust/syntax/syntax/src/grammer.ron"),
        teraron::Mode::Overwrite,
    ) {
        Ok(_) => println!("ok"),
        Err(e) => println!("{:?}", e),
    };
}
