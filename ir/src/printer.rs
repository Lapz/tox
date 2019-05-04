use crate::instructions::{BlockEnd, Function, Instruction, Program};
use std::io::{self, Write};
use util::symbol::Symbols;

pub struct Printer<'a> {
    symbols: &'a Symbols<()>,
}

impl<'a> Printer<'a> {
    pub fn new(symbols: &'a Symbols<()>) -> Self {
        Self { symbols }
    }

    pub fn print_program<T: Write>(mut self, p: &Program, out: &mut T) -> io::Result<()> {
        for function in p.functions.iter() {
            self.print_function(function, out)?;
            write!(out, "")?;
        }

        Ok(())
    }

    pub fn print_function<T: Write>(&mut self, f: &Function, out: &mut T) -> io::Result<()> {
        write!(out, "function ")?;
        write!(out, "{}", &self.symbols.name(f.name))?;

        write!(out, "(")?;

        for (i, param) in f.params.iter().enumerate() {
            if (i + 1) == f.params.len() {
                write!(out, "{}", param)?;
            } else {
                write!(out, "{},", param)?;
            }
        }

        write!(out, ")")?;

        write!(out, "\n{{\n")?;

        write!(out, "start: {}\n", f.start_block)?;

        for (id, block) in f.blocks.iter() {
            write!(out, "{}:", id)?;

            for inst in block.instructions.iter() {
                write!(out, "\t")?;
                self.print_instructions(&inst, out)?;
                write!(out, "\n")?;
            }

            match block.end {
                BlockEnd::End => write!(out, "\n\tend")?,
                BlockEnd::Jump(ref id) => write!(out, "\n\tgoto {}", id)?,
                BlockEnd::Link(ref id) => write!(out, "\n\tlink {}", id)?,
                BlockEnd::Return(ref value) => write!(out, "\n\treturn {}", value)?,
                BlockEnd::Branch(ref value, ref t, ref f) => {
                    write!(out, "\n\tbranch {} {} {}", value, t, f)?
                }
            }

            write!(out, "\n")?;
        }

        write!(out, "}}\n")?;

        Ok(())
    }

    pub fn print_instructions<T: Write>(&mut self, i: &Instruction, out: &mut T) -> io::Result<()> {
        match *i {
            Instruction::Array(ref l, ref s) => write!(out, "{} <- [{}]", l, s),
            Instruction::StatementStart => write!(out, ""),
            Instruction::Binary(ref res, ref lhs, ref op, ref rhs) => {
                write!(out, "{} <- {} {} {}", res, lhs, op, rhs)
            }
            Instruction::Store(ref dest, ref source) => write!(out, "{} <- {}", dest, source),
            Instruction::StoreI(ref dest, ref source) => write!(out, "{} <- {}", dest, source),
            Instruction::Cast(ref dest, _, ref ty) => write!(out, "{} as {}", dest, ty),
            Instruction::Unary(ref dest, ref source, ref op) => {
                write!(out, "{} <- {}{}", dest, op, source)
            }
            Instruction::Return(ref label) => write!(out, "return @{}", label),
            Instruction::Call(ref dest, ref callee, ref args) => {
                write!(out, "{} <- call {} ", dest, callee)?;

                for arg in args {
                    write!(out, "{}\n", arg)?;
                }

                write!(out, "")?;

                Ok(())
            }
        }
    }
}
