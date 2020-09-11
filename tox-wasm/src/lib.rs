use frontend::compile as compile_to_bytecode;
use frontend::Infer;
use std::rc::Rc;
use syntax::parser::Parser;
use util::{emmiter::Reporter, symbol::SymbolFactory, symbol::Symbols};
use vm::VM;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn compile(input: &str) -> Result<(), JsValue> {
    if input.is_empty() {
        return Ok(());
    }

    let mut reporter = Reporter::new();
    let mut output = String::new();

    let strings = Rc::new(SymbolFactory::new());
    let mut symbols = Symbols::new(Rc::clone(&strings));

    let ast = match Parser::new(input, reporter.clone(), &mut symbols).parse() {
        Ok(statements) => statements,
        Err(_) => {
            if cfg!(target_arch = "wasm32") {
                reporter.emit(input, &mut output);
                return Err(output.into());
            } else {
                return Ok(());
            }
        }
    };

    let mut infer = Infer::new();

    let typed_ast = match infer.infer(ast, &strings, &mut reporter) {
        Ok(ast) => {
            if cfg!(target_arch = "wasm32") {
                reporter.emit(input, &mut output);
            }
            ast
        }
        Err(_) => {
            if cfg!(target_arch = "wasm32") {
                reporter.emit(input, &mut output);
                return Err(output.into());
            } else {
                return Ok(());
            }
        }
    };

    // if compile_vm {
    let (program, objects) = match compile_to_bytecode(&typed_ast, &symbols, &mut reporter) {
        Ok(functions) => functions,
        Err(_) => {
            if cfg!(target_arch = "wasm32") {
                reporter.emit(input, &mut output);
                return Err(output.into());
            } else {
                return Ok(());
            }
        }
    };

    let mut vm = VM::new(symbols.symbol("main"), &program, objects).unwrap();

    vm.run();
    Ok(())
}
