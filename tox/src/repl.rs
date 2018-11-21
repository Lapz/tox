use frontend::{compile, Infer};
use std::io::{self, Write};
use std::rc::Rc;
use syntax::ast::{Function, Program};

use syntax::parser::Parser;
use util::emmiter::Reporter;
use util::pos::Spanned;
use util::symbol::{SymbolFactory, Symbols};
use vm::VM;

pub struct Repl {}

impl Repl {
    pub fn new() -> Self {
        Repl {}
    }

    pub fn run(&mut self) {
        println!("Welcome to the tox programming language");

        let strings = Rc::new(SymbolFactory::new());
        let mut symbols = Symbols::new(Rc::clone(&strings));

        loop {
            let mut input = String::with_capacity(1024);

            let _ = io::stdout().write(b"lexer>> ");
            let _ = io::stdout().flush();

            input.clear();

            io::stdin()
                .read_line(&mut input)
                .expect("Couldn't read input");

            if input.is_empty() {
                continue;
            }

            match input.as_ref() {
                ".quit\n" => break,
                ".next\n" => continue,
                ".history\n" => unimplemented!(),
                _ => (),
            }

            let mut reporter = Reporter::new();

            let expr = match Parser::new(&input, reporter.clone(), &mut symbols).parse_statement() {
                Ok(statements) => statements,
                Err(_) => {
                    reporter.emit(&input);
                    ::std::process::exit(65)
                }
            };

            let pusedo_main_symbol = symbols.symbol("main");

            let pusedo_main_span = expr.get_span();

            let pusedo_main = Spanned::new(
                Function {
                    name: Spanned::new(pusedo_main_symbol, pusedo_main_span),
                    params: Spanned::new(vec![], pusedo_main_span),
                    returns: None,
                    body: expr,
                },
                pusedo_main_span,
            );

            let ast = Program {
                functions: vec![pusedo_main],
                classes: vec![],
                aliases: vec![],
            };

            let mut infer = Infer::new();

            let typed_ast = match infer.infer(ast, &strings, &mut reporter) {
                Ok(ast) => ast,
                Err(_) => {
                    reporter.emit(&input);

                    continue;
                }
            };

            let (functions, classes, objects) = match compile(&typed_ast, &mut reporter) {
                Ok(functions) => functions,
                Err(_) => {
                    reporter.emit(&input);
                    continue;
                }
            };

            let mut vm = VM::new(symbols.symbol("main"), &functions, &classes, objects).unwrap();

            vm.run()
        }
    }
}
