extern crate fnv;
extern crate frontend;
extern crate structopt;
#[macro_use]
extern crate structopt_derive;
extern crate interpreter;
extern crate syntax;
extern crate util;
extern crate vm;

mod repl;

// use codegen::Compiler;
use frontend::{compile, Infer};
use interpreter::{interpret, Environment};
use std::fs::File;
use std::io::Read;
use std::rc::Rc;
use structopt::StructOpt;
use syntax::lexer::Lexer;
use syntax::parser::Parser;
use util::emmiter::Reporter;
use util::symbol::{SymbolFactory, Symbols};
use vm::VM;

fn main() {
    let opts = Cli::from_args();

    if let Some(file) = opts.source {
        if opts.vm {
            run_vm(file);
        } else if opts.interpreter {
            run_interpreter(file, opts.ptokens, opts.pprint, opts.past);
        } else {
            run(file, opts.ptokens, opts.pprint, opts.past);
        }
    } else {
        repl()
    }
}

pub fn repl() {
    use repl::Repl;

    // Repl::new().run();
}

pub fn run_interpreter(path: String, ptokens: bool, pprint: bool, past: bool) {
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

    let mut reporter = Reporter::new();

    let mut lexer = Lexer::new(input, reporter.clone());

    let tokens = match lexer.lex() {
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

    reporter.set_end(lexer.end_span());

    let strings = Rc::new(SymbolFactory::new());
    let mut symbols = Symbols::new(Rc::clone(&strings));

    let ast = match Parser::new(tokens, reporter.clone(), &mut symbols).parse() {
        Ok(statements) => {
            if pprint {
                // for statement in &statements {
                //     println!("{}", statement.value.pprint(&mut symbols));
                // }
            }
            statements
        }
        Err(_) => {
            reporter.emit(input);
            ::std::process::exit(65)
        }
    };

    if past {
        println!("{:#?}", ast);
    }

    let mut infer = Infer::new();

    match infer.infer(ast.clone(), &strings, &mut reporter) {
        Ok(_) => (),
        Err(_) => {
            reporter.emit(input);
            ::std::process::exit(65)
        }
    };

    let mut env = Environment::new();
    env.fill_env(&mut symbols);

    match interpret(&ast, &mut env, infer.get_main()) {
        Ok(_) => (),
        Err(err) => {
            if let Some(span) = err.span {
                let msg = err.code.reason(&symbols);

                reporter.run_time_error(msg, span);

                reporter.emit(input);
            } else {
                reporter.global_run_time_error(&err.code.reason(&symbols));
                reporter.emit(input);
            }

            ::std::process::exit(65)
        }
    };
}

pub fn run_vm(path: String) {
    let mut file = File::open(path).expect("File not found");

    let mut contents = String::new();

    file.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    if contents.is_empty() {
        ::std::process::exit(0)
    }

    // let mut assembler = Assembler::new();

    // let bytecode = match assembler.assemble(&contents) {
    //     Ok(bytecode) => bytecode,
    //     Err(e) => {
    //         println!("{:?}", e);
    //         ::std::process::exit(0)
    //     }
    // };

    // let mut vm = VM::new();

    // vm.code(bytecode);

    // vm.disassemble("test");

    // vm.run();

    // println!("{:?}", vm);
}

pub fn run(path: String, ptokens: bool, pprint: bool, past: bool) {
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

    let mut reporter = Reporter::new();

    let mut lexer = Lexer::new(input, reporter.clone());

    let tokens = match lexer.lex() {
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

    reporter.set_end(lexer.end_span());

    let strings = Rc::new(SymbolFactory::new());
    let mut symbols = Symbols::new(Rc::clone(&strings));

    let ast = match Parser::new(tokens, reporter.clone(), &mut symbols).parse() {
        Ok(statements) => statements,
        Err(_) => {
            reporter.emit(input);
            ::std::process::exit(65)
        }
    };

    if past {
        println!("{:#?}", ast);
    }

    let mut infer = Infer::new();

    let typed_ast = match infer.infer(ast, &strings, &mut reporter) {
        Ok(ast) => ast,
        Err(_) => {
            reporter.emit(input);
            ::std::process::exit(65)
        }
    };

    let functions = match compile(&typed_ast, &mut reporter) {
        Ok(functions) => functions,
        Err(_) => {
            reporter.emit(input);
            ::std::process::exit(65)
        }
    };

    let mut vm = VM::new(symbols.symbol("main"), &functions).unwrap();
    vm.run();

    #[cfg(feature = "debug")]
    println!("{:?}",vm);
}

#[derive(StructOpt, Debug)]
#[structopt(name = "tox")]
pub struct Cli {
    /// The source code file
    pub source: Option<String>,
    /// Pretty Print Source Code
    #[structopt(long = "pretty_print", short = "p")]
    pub pprint: bool,
    /// Print out tokens
    #[structopt(long = "tokens", short = "t")]
    pub ptokens: bool,
    /// Print out ast debug mode
    #[structopt(long = "rawast", short = "ra")]
    pub past: bool,
    /// Run in vm mode
    #[structopt(long = "vm", short = "v")]
    pub vm: bool,
    /// Run in interpreter mode
    #[structopt(long = "interpter", short = "-i")]
    pub interpreter: bool,
}
