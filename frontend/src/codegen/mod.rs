mod gen_tasm;
mod label;
mod tasm;
use ast::{Function, Program};
use std::fs::File;
use std::io::{self, Write};
use std::collections::HashMap;
use codegen::label::Label;
use codegen::tasm::TASM;
use std::iter::repeat;
pub struct Compiler {
    file: File,
    indent_level: usize,
    strings:HashMap<Label,String>
}

impl Compiler  {
    pub fn new() -> Self {
        Compiler {
            file: File::create("output.tasm").expect("Couldn't create the file"),
            indent_level: 0,
            strings:HashMap::new()
        }
    }

    pub fn compile(&mut self, ast: &Program) -> io::Result<()> {

        for function in ast.functions.iter() {
            self.process_strings(&function.body);
        }

        //hello: .asciiz 'Hello everyone!'

        self.write(TASM::DIRECTIVE(".data".into()))?;

        self.indent_level += 1;

        for (label,string) in self.strings.iter() {
            write!(&mut self.file,"{}{}: .asciiz \"{}\"\n",repeat_string("\t", self.indent_level),label,string)?;
        }

        write!(&mut self.file, ".code\n")?;

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

fn repeat_string(s: &str, count: usize) -> String {
    repeat(s).take(count).collect()
}
