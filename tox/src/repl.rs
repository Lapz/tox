use frontend::{Compiler, Infer};
use std::fs::File;
use std::io::{self, Read, Write};
use std::rc::Rc;
use syntax::lexer::Lexer;
use syntax::parser::Parser;
use util::emmiter::Reporter;
use util::symbol::{SymbolFactory, Symbols};
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
            Ok(bytecode) => bytecode,
            Err(e) => {
                println!("{:?}", e);
                return Err(());
            }
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

        let mut reporter = Reporter::new();

        let mut lexer = Lexer::new(input, reporter.clone());

        let tokens = match lexer.lex() {
            Ok(tokens) => tokens,
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
                reporter.emit(&input);
                return Err(());
            }
        };

        let mut infer = Infer::new();

        let typed_ast = match infer.infer(ast, &strings, &mut reporter) {
            Ok(ast) => ast,
            Err(_) => {
                reporter.emit(input);
                ::std::process::exit(65)
            }
        };

        let mut compiler = Compiler::new();

        compiler
            .compile(&typed_ast)
            .expect("Couldn't compile the file");

        let bytecode = match Assembler::new().assemble_file("output.tasm") {
            Ok(bytecode) => bytecode,
           Err(_) => ::std::process::exit(0),
        };

        self.vm.code(bytecode);

        Ok(())
    }
}
