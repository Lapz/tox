extern crate fnv;
extern crate interpreter;
extern crate sem;
extern crate structopt;
#[macro_use]
extern crate structopt_derive;
extern crate syntax;
extern crate util;
extern crate vm;

use syntax::lexer::Lexer;
use syntax::parser::Parser;
use sem::resolver::Resolver;
use sem::semant::TyChecker;
use interpreter::interpret;
use interpreter::interpreter::env::Environment;
use std::io;
use util::env::TypeEnv;
use util::symbol::{SymbolFactory, Table};
use util::emmiter::Reporter;
use std::rc::Rc;
use std::io::Write;
use structopt::StructOpt;
use vm::{Chunk, OpCode};
use util::pos::{Position, Span};

fn main() {
    let opts = Cli::from_args();

    if let Some(file) = opts.source {
        run(file, opts.ptokens, opts.pprint, opts.env, opts.past);
    } else {
        repl(opts.ptokens, opts.pprint)
    }
}

// use compiler::compile;
pub fn repl(ptokens: bool, pprint: bool) {
    println!("Welcome to the lexer programming language");

    loop {
        let _ = io::stdout().write(b"lexer>> ");
        let _ = io::stdout().flush();
        let mut input = String::new();

        io::stdin()
            .read_line(&mut input)
            .expect("Couldn't read input");

        let reporter = Reporter::new();

        let tokens = match Lexer::new(&input, reporter.clone()).lex() {
            Ok(tokens) => {
                if ptokens {
                    for token in &tokens {
                        println!("{:#?}", token);
                    }
                }
                tokens
            }
            Err(_) => {
                reporter.emit(&input);
                continue;
            }
        };

        let strings = Rc::new(SymbolFactory::new());
        let mut symbols = Table::new(Rc::clone(&strings));

        let ast = match Parser::new(tokens, reporter.clone(), &mut symbols).parse() {
            Ok(statements) => {
                if pprint {
                    // for statement in &statements {
                    //     println!("{}", statement.node.pprint(&mut symbols));
                    // }
                }
                statements
            }
            Err(_) => {
                reporter.emit(&input);
                continue;
            }
        };

        let mut tyenv = TypeEnv::new(&strings);
        let mut resolver = Resolver::new(reporter.clone());

        resolver.resolve(&ast, &tyenv).unwrap();

        match TyChecker::new(reporter.clone()).analyse(&ast, &mut tyenv) {
            Ok(_) => (),
            Err(_) => {
                reporter.emit(&input);
                continue;
            }
        };

        let mut env = Environment::new();
        env.fill_env(&mut tyenv);
        match interpret(&ast, &resolver.locals, &mut env) {
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

    let reporter = Reporter::new();

    let tokens = match Lexer::new(input, reporter.clone()).lex() {
        Ok(tokens) => {
            if ptokens {
                for token in &tokens {
                    println!("{:#?}", token);
                }
            }
            tokens
        }
        Err(_) => {
            reporter.emit(input);
            ::std::process::exit(65)
        }
    };

    let strings = Rc::new(SymbolFactory::new());
    let mut symbols = Table::new(Rc::clone(&strings));

    let ast = match Parser::new(tokens, reporter.clone(), &mut symbols).parse() {
        Ok(statements) => {
            if pprint {
                for statement in &statements {
                    println!("{}", statement.value.pprint(&mut symbols));
                }
            }
            statements
        }
        Err(_) => {
            reporter.emit(input);
            // vec![];
            ::std::process::exit(65)
        }
    };

    if past {
        println!("{:#?}", ast);
    }

    let mut tyenv = TypeEnv::new(&strings);

    let mut resolver = Resolver::new(reporter.clone());

    match resolver.resolve(&ast, &tyenv) {
        Ok(_) => (),
        Err(_) => {
            reporter.emit(input);
            ::std::process::exit(65)
        }
    }

    match TyChecker::new(reporter.clone()).analyse(&ast, &mut tyenv) {
        Ok(_) => (),
        Err(_) => {
            reporter.emit(input);
            ::std::process::exit(65)
        }
    };

    if penv {
        println!("{:#?}", tyenv);
    }

    let mut chunk = Chunk::new();

    let constant = chunk.add_constant(1.2);

    chunk.write(
        OpCode::Constant(constant),
        Span {
            start: Position {
                line: 1,
                column: 0,
                absolute: 1,
            },
            end: Position {
                line: 1,
                column: 0,
                absolute: 1,
            },
        },
    );

    chunk.write(
        OpCode::Return,
        Span {
            start: Position {
                line: 1,
                column: 0,
                absolute: 1,
            },
            end: Position {
                line: 1,
                column: 0,
                absolute: 1,
            },
        },
    );

    chunk.dissassemble("test chunk");

    // let mut env = Environment::new();
    // env.fill_env(&mut tyenv);
    // match interpret(&ast, &resolver.locals, &mut env) {
    //     Ok(_) => (),
    //     Err(_) => {
    //         // println!("{:?}", err);
    //         ::std::process::exit(65)
    //     }
    // };

    // if penv {
    //     println!("{:#?}", tyenv);
    //     println!("{:#?}", env);
    // }
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
