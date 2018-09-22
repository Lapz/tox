use codegen::Compiler;
use sem::resolver::Resolver;
use sem::semant::TyChecker;
use std::fs::File;
use std::io::{self, Read, Write};
use std::rc::Rc;
use syntax::lexer::Lexer;
use syntax::parser::Parser;
use util::emmiter::Reporter;
use util::env::TypeEnv;
use util::symbol::{SymbolFactory, Table};
use vm::{Assembler, VM};

pub struct Repl {
    commands: Vec<String>,
    vm: VM,
    assembler: Assembler,
}

impl Repl {
    pub fn new() -> Self {
        Repl {
            vm: VM::new(),
            commands: Vec::with_capacity(10),
            assembler: Assembler::new(),
        }
    }

    pub fn run(&mut self) {
        println!("Welcome to the tox programming language");
        let mut input = String::with_capacity(1024);

        loop {
            let _ = io::stdout().write(b"lexer>> ");
            let _ = io::stdout().flush();

            input.clear();

            io::stdin()
                .read_line(&mut input)
                .expect("Couldn't read input");

            self.commands.push(input.to_string());

            let commands = input.trim().split(" ").collect::<Vec<&str>>();

            match commands[0] {
                ".quit" => ::std::process::exit(0),
                ".program" => {
                    self.vm.disassemble("Program");
                }
                ".registers" => {
                    println!("Listing registers and all contents:");
                    println!("{:#?}", self.vm.registers());
                    println!("End of Register Listing")
                }
                ".load_file" => match commands.get(1) {
                    Some(path) => {
                        if let Err(_) = self.parse_file(path) {
                            println!("An error occured");
                            continue;
                        };

                        self.vm.run();
                    }
                    None => {
                        println!(".load_file expects an argument",);
                        continue;
                    }
                },
                ".load_tasm" => match commands.get(1) {
                    Some(path) => {
                        if let Err(_) = self.parse_tasm(path) {
                            println!("An error occured");
                            continue;
                        };

                        self.vm.run();
                    }
                    None => {
                        println!(".load_tasm expects an argument",);
                        continue;
                    }
                },
                ".history" => {
                    for command in self.commands.iter() {
                        println!("{}", command);
                    }
                }
                _ => println!("Unkown command \"{}\"", commands[0]),
            }
        }
    }
    /// Parses a tasm file,returns void on error
    fn parse_tasm(&mut self, path: &str) -> Result<(), ()> {
        let mut file = match File::open(path) {
            Ok(file) => file,
            Err(_) => return Err(()),
        };

        let mut contents = String::new();

        file.read_to_string(&mut contents)
            .expect("something went wrong reading the file");

        let input = contents.trim();

        let bytecode = match self.assembler.assemble(&input) {
            Some(bytecode) => bytecode,
            None => return Err(())
        };

        self.vm.code(bytecode);

        Ok(())
    }

    /// Parses the tox programming languages returns void on error due to the fact that the reporter will
    /// report any type errors or any compilation errors.
    /// Can result in a panic if a file cannot be read to a string
    fn parse_file(&mut self, path: &str) -> Result<(), ()> {
        let mut file = match File::open(path) {
            Ok(file) => file,
            Err(_) => return Err(()),
        };

        let mut contents = String::new();

        file.read_to_string(&mut contents)
            .expect("something went wrong reading the file");

        let input = contents.trim();

        let reporter = Reporter::new();

        let tokens = match Lexer::new(&input, reporter.clone()).lex() {
            Ok(tokens) => tokens,

            Err(_) => {
                reporter.emit(&input);
                return Err(());
            }
        };

        let strings = Rc::new(SymbolFactory::new());
        let mut symbols = Table::new(Rc::clone(&strings));

        let ast = match Parser::new(tokens, reporter.clone(), &mut symbols).parse() {
            Ok(statements) => statements,
            Err(_) => {
                reporter.emit(&input);
                return Err(());
            }
        };

        let mut tyenv = TypeEnv::new(&strings);
        let mut resolver = Resolver::new(reporter.clone());

        resolver.resolve(&ast, &tyenv).unwrap();

        match TyChecker::new(reporter.clone()).analyse(&ast, &mut tyenv) {
            Ok(_) => (),
            Err(_) => {
                reporter.emit(&input);
                return Err(());
            }
        };

        let mut compiler = Compiler::new();

        compiler.compile(&ast).expect("Couldn't compile the file");

        let bytecode = match compiler.assemble() {
            Some(bytecode) => bytecode,
            None => return Err(()),
        };

        self.vm.code(bytecode);

        Ok(())
    }
}
