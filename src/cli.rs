use lexer::Lexer;
use parser::Parser;
use resolver::Resolver;
use inference::analyse;
use interpreter::interpret;
use std::io;
use symbol::{SymbolFactory, Symbols};
use env::Env;
use std::rc::Rc;
use std::io::Write;

pub fn repl(ptokens: bool, pprint: bool) {
    println!("Welcome to the lexer programming language");
    loop {
        let _ = io::stdout().write(b"lexer>> ");
        let _ = io::stdout().flush();
        let mut input = String::new();
        io::stdin()
            .read_line(&mut input)
            .expect("Couldn't read input");

        let tokens = match Lexer::new(&input).lex() {
            Ok(tokens) => {
                if ptokens {
                    for token in &tokens {
                        println!("{:#?}", token);
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
                        println!("{}", statement.node.pprint(&mut symbols));
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

        match analyse(&ast, &mut env) {
            Ok(_) => (),
            Err(errors) => {
                for err in errors {
                    println!("{:?}", err);
                }
                continue;
            }
        };

        match interpret(&ast, &mut env) {
            Ok(_) => (),
            Err(err) => {
                println!("{:?}", err);

                continue;
            }
        };
    }
}

pub fn run(path: String, ptokens: bool, pprint: bool, penv: bool, past: bool) {
    use std::fs::File;
    use std::io::Read;

    let mut file = File::open(path).expect("File not found");

    let mut contents = String::new();

    file.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    let input = contents.trim();

    if contents.is_empty() {
        ::std::process::exit(0)
    }

    let tokens = match Lexer::new(&input).lex() {
        Ok(tokens) => {
            if ptokens {
                for token in &tokens {
                    println!("{:#?}", token);
                }
            }
            tokens
        }
        Err(errors) => {
            for err in errors {
                println!("{}", err);
            }
            ::std::process::exit(65)
        }
    };

    let strings = Rc::new(SymbolFactory::new());
    let mut symbols = Symbols::new(Rc::clone(&strings));

    let ast = match Parser::new(tokens, &mut symbols).parse() {
        Ok(statements) => {
            if pprint {
                for statement in &statements {
                    println!("{}", statement.node.pprint(&mut symbols));
                }
            }
            statements
        }
        Err(errors) => {
            for err in errors {
                println!("{}", err);
            }
            ::std::process::exit(65)
        }
    };

    if past {
        println!("{:#?}", ast);
    }

    Resolver::new().resolve(&ast).unwrap();

    let mut env = Env::new(&strings);

    // match analyse(&ast, &mut env) {
    //     Ok(_) => (),
    //     Err(errors) => {
    //         for err in errors {
    //             println!("{:?}", err);
    //         }
    //         ::std::process::exit(65)
    //     }
    // };

    if penv {
        println!("{:#?}", env);
    }

    match interpret(&ast, &mut env) {
        Ok(_) => (),
        Err(err) => {
            println!("{:?}", err);

            ::std::process::exit(65)
        }
    };

    if penv {
        println!("{:#?}", env);
    }
}

#[derive(StructOpt, Debug)]
#[structopt(name = "lexer")]
pub struct Cli {
    /// The source code file
    pub source: Option<String>,
    /// Pretty Print Source Code
    #[structopt(long = "pretty_print", short = "p")]
    pub pprint: bool,
    /// Print out the mappings in the environment
    #[structopt(long = "env", short = "e")]
    pub env: bool,
    /// Print out tokens
    #[structopt(long = "tokens", short = "t")]
    pub ptokens: bool,
    /// Print out ast debug mode
    #[structopt(long = "rawast", short = "ra")]
    pub past: bool,
}
