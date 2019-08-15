use codespan::{CodeMap, FileMap, FileName, Span};
use parser::Parser;
use rowan::SmolStr;
use syntax::{
    ArgListOwner, AstNode, ClassDefOwner, FnDefOwner, Lexer, TypeAscriptionOwner, VisibilityOwner,
};

pub type ParseResult<T> = Result<T, ()>;

fn main() {
    let input = "fn main(a:i32,b:i32) {}";
    let mut lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer.lex().into_iter(), input);

    let file = parser.parse_program();
    // println!("{:?}",);
    // println!("{:?}", parser.past_tokens);
    // println!("{:?}", parser.bump());
    // println!("{:?}", parser.past_tokens);
    // println!("{:?}", parser.bump());
    // println!("{:?}", parser.past_tokens);
    // println!("{:?}", parser.bump());
    //
    println!("{:#?}", file);

    let func = file.functions().nth(0).unwrap();

    if let Some(params) = func.param_list() {
        for param in params.params() {
            println!("{:?}", param.pat());
            println!("{:?}", param.ascribed_type());
        }
    }

    println!("{:?}", func.visibility());

    // for token in lexer.lex() {
    //     println!(
    //         "{:?},{:?}",
    //         SmolStr::new(
    //             &input[token.start.absolute as usize
    //                 ..(token.value.len + token.start.absolute) as usize]
    //         ),
    //         token.value.kind
    //     )
    // }

    // match teraron::generate(
    //     std::path::Path::new("/Users/lenardpratt/Projects/Rust/syntax/syntax/src/ast.rs.tera"),
    //     std::path::Path::new("/Users/lenardpratt/Projects/Rust/syntax/syntax/src/grammer.ron"),
    //     teraron::Mode::Overwrite,
    // ) {
    //     Ok(_) => println!("ok"),
    //     Err(e) => println!("{:?}", e),
    // };
}
