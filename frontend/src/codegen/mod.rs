mod gen_tasm;
mod label;

use ast::Program;
use std::fs::File;
use std::io::{self, Write};

pub struct Compiler {
    file: File,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            file: File::create("output.tasm").expect("Couldn't create the file"),
        }
    }

    pub fn compile(&mut self, ast: &Program) -> io::Result<()> {
        for node in ast.statements.iter() {
            self.build_statement(node)?;
        }

        write!(&mut self.file, "HLT")?;

        Ok(())
    }
}
