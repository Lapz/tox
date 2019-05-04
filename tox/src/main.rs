extern crate fnv;
extern crate frontend;
extern crate structopt;
#[macro_use]
extern crate structopt_derive;
// extern crate interpreter;
extern crate ir;
extern crate syntax;
extern crate util;
extern crate vm;

mod repl;

#[cfg(feature = "graphviz")]
use frontend::build_program;
use frontend::compile;
use frontend::Infer;
#[cfg(feature = "graphviz")]
use ir::printer::Printer;

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
        run(file, opts.ir_file);
    } else {
        repl()
    }
}

pub fn repl() {
    use repl::Repl;

    Repl::new().run();
}

pub fn run(path: String, _print_ir: Option<String>) {
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

    #[cfg(feature = "graphviz")]
    {
        let program = build_program(&mut symbols, typed_ast.clone());
        program.graphviz(&symbols).unwrap();
        if let Some(f) = _print_ir {
            let printer = Printer::new(&symbols);

            printer
                .print_program(&program, &mut File::create(f).unwrap())
                .unwrap();
        }
    }

    let (program, objects) = match compile(&typed_ast, &symbols, &mut reporter) {
        Ok(functions) => functions,
        Err(_) => {
            reporter.emit(input);
            ::std::process::exit(65)
        }
    };

    let mut vm = VM::new(symbols.symbol("main"), &program, objects).unwrap();
    vm.run();
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
