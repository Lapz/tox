mod gen_tasm;
mod label;
mod tasm;
use ast::{Function, Program};
use std::fs::File;
use std::io::{self, Write};

pub struct Compiler {
    file: File,
    indent_level: usize,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            file: File::create("output.tasm").expect("Couldn't create the file"),
            indent_level: 0,
        }
    }

    pub fn compile(&mut self, ast: &Program) -> io::Result<()> {
        for function in ast.functions.iter() {
            self.compile_function(function)?;
        }

        write!(&mut self.file, "HLT")?;

        Ok(())
    }

    fn compile_function(&mut self, func: &Function) -> io::Result<()> {
        self.build_statement(&func.body)
    }
}
