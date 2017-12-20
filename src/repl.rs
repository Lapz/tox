use std::env;
use lexer::Lexer;
use parser::Parser;
use resolver::Resolver;
use inference::analyse;
use std::io;
use symbol::{SymbolFactory, Symbols};
use env::Env;
use std::rc::Rc;

fn repl(ptokens: bool, pprint: bool) {
    loop {
        let mut input = String::new();
        io::stdin()
            .read_line(&mut input)
            .expect("Couldn't read input");

        let tokens = match Lexer::new(&input).lex() {
            Ok(tokens) => {
                if ptokens {
                    for token in &tokens {
                        println!("{}", token);
                    }
                }
                tokens
            }
            Err(errors) => {
                for err in errors {
                    println!("{}", err);
                }
                continue;
            }
        };

        let strings = Rc::new(SymbolFactory::new());
        let mut symbols = Symbols::new(Rc::clone(&strings));

        let ast = match Parser::new(tokens, &mut symbols).parse() {
            Ok(statements) => {
                if pprint {
                    for statement in &statements {
                        println!("{}", statement.pprint());
                    }
                }
                statements
            }
            Err(errors) => {
                for err in errors {
                    println!("{}", err);
                }
                continue;
            }
        };

        Resolver::new().resolve(&ast).unwrap();

        let mut env = Env::new(&strings);

        println!("{:#?}", analyse(&ast, &mut env));
    }
}
