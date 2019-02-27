extern crate fnv;
extern crate frontend;
extern crate structopt;
#[macro_use]
extern crate structopt_derive;
// extern crate interpreter;
extern crate syntax;
extern crate util;
extern crate vm;

mod repl;

use frontend::compile;
use frontend::Infer;
// use interpreter::{interpret, Environment};
use std::fs::File;
use std::io::Read;
use std::rc::Rc;
use structopt::StructOpt;
use syntax::parser::Parser;
use util::emmiter::Reporter;
use util::symbol::{SymbolFactory, Symbols};
use vm::VM;

fn main() {
    let opts = Cli::from_args();

    if let Some(file) = opts.source {
        // if opts.interpreter {
        //     run_interpreter(file);
        // } else {
        run(file);
    // }
    } else {
        repl()
    }
}

pub fn repl() {
    use repl::Repl;

    Repl::new().run();
}

// pub fn run_interpreter(path: String) {
//     let mut file = File::open(path).expect("File not found");

//     let mut contents = String::new();

//     file.read_to_string(&mut contents)
//         .expect("something went wrong reading the file");

//     let input = contents.trim();

//     if contents.is_empty() {
//         ::std::process::exit(0)
//     }

//     let mut reporter = Reporter::new();

//     let strings = Rc::new(SymbolFactory::new());
//     let mut symbols = Symbols::new(Rc::clone(&strings));

//     let ast = match Parser::new(input, reporter.clone(), &mut symbols).parse() {
//         Ok(statements) => statements,
//         Err(_) => {
//             reporter.emit(input);
//             ::std::process::exit(65)
//         }
//     };

//     let mut infer = Infer::new();

//     match infer.infer(ast.clone(), &strings, &mut reporter) {
//         Ok(_) => (),
//         Err(_) => {
//             reporter.emit(input);
//             ::std::process::exit(65)
//         }
//     };

//     let mut env = Environment::new();
//     env.fill_env(&mut symbols);

//     match interpret(&ast, &mut env, infer.get_main()) {
//         Ok(_) => (),
//         Err(err) => {
//             if let Some(span) = err.span {
//                 let msg = err.code.reason(&symbols);

//                 reporter.run_time_error(msg, span);

//                 reporter.emit(input);
//             } else {
//                 reporter.global_run_time_error(&err.code.reason(&symbols));
//                 reporter.emit(input);
//             }

//             ::std::process::exit(65)
//         }
//     };
// }

pub fn run(path: String) {
    let mut file = File::open(path).expect("File not found");

    let mut contents = String::new();

    file.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    let input = contents.trim();

    if contents.is_empty() {
        ::std::process::exit(0)
    }

    let mut reporter = Reporter::new();

    let strings = Rc::new(SymbolFactory::new());
    let mut symbols = Symbols::new(Rc::clone(&strings));

    let ast = match Parser::new(input, reporter.clone(), &mut symbols).parse() {
        Ok(statements) => statements,
        Err(_) => {
            reporter.emit(input);
            ::std::process::exit(65)
        }
    };

    let mut infer = Infer::new();

    let typed_ast = match infer.infer(ast, &strings, &mut reporter) {
        Ok(ast) => {
            reporter.emit(input); //emit warnings

            ast
        }
        Err(_) => {
            reporter.emit(input);

            ::std::process::exit(65)
        }
    };

    // if compile_vm {
    let (program, objects) = match compile(&typed_ast, &symbols, &mut reporter) {
        Ok(functions) => functions,
        Err(_) => {
            reporter.emit(input);
            ::std::process::exit(65)
        }
    };

    let mut vm = VM::new(symbols.symbol("main"), &program, objects).unwrap();
    vm.run();
    // }
}

#[derive(StructOpt, Debug)]
#[structopt(name = "tox")]
pub struct Cli {
    /// The source code file
    pub source: Option<String>,
    /// Run in interpreter mode
    #[structopt(long = "interpter", short = "-i")]
    pub interpreter: bool,

    /// Dump the ir to the given file
    #[structopt(long = "file", short = "-f")]
    pub ir_file: Option<String>,
}
