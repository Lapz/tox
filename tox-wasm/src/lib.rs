// Use `wee_alloc` as the global allocator.
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;
#[cfg(target_arch = "wasm32")]
use frontend::compile as compile_to_bytecode;
#[cfg(target_arch = "wasm32")]
use frontend::Infer;
#[cfg(target_arch = "wasm32")]
use std::fmt::Write;
#[cfg(target_arch = "wasm32")]
use std::panic;
#[cfg(target_arch = "wasm32")]
use std::rc::Rc;
#[cfg(target_arch = "wasm32")]
use syntax::parser::Parser;
#[cfg(target_arch = "wasm32")]
use util::{emmiter::Reporter, symbol::SymbolFactory, symbol::Symbols};
#[cfg(target_arch = "wasm32")]
use vm::VM;
#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;

#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
pub fn compile(input: &str) -> Result<JsValue, JsValue> {
    panic::set_hook(Box::new(console_error_panic_hook::hook));
    if input.is_empty() {
        return Ok("".into());
    }

    let mut reporter = Reporter::new();
    let mut output = String::new();

    let strings = Rc::new(SymbolFactory::new());
    let mut symbols = Symbols::new(Rc::clone(&strings));

    let ast = match Parser::new(input, reporter.clone(), &mut symbols).parse() {
        Ok(statements) => statements,
        Err(_) => {
            reporter.emit(input, &mut output);
            return Err(output.into());
        }
    };

    let mut infer = Infer::new();

    let typed_ast = match infer.infer(ast, &strings, &mut reporter) {
        Ok(ast) => {
            reporter.emit(input, &mut output);
            ast
        }
        Err(_) => {
            reporter.emit(input, &mut output);
            return Err(output.into());
        }
    };

    let (program, objects) = match compile_to_bytecode(&typed_ast, &symbols, &mut reporter) {
        Ok(functions) => functions,
        Err(_) => {
            reporter.emit(input, &mut output);
            return Err(output.into());
        }
    };

    let mut vm = VM::new(symbols.symbol("main"), &program, objects).unwrap();

    vm.run();

    let mut print_string = String::new();

    vm.stdout
        .iter()
        .for_each(|val| write!(print_string, "$ {}\n\r", val).unwrap());

    Ok(print_string.into())
}
