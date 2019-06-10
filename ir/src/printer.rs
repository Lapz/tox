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
            let mut inst_counter = 0;

            for inst in block.instructions.iter() {
                if !inst.is_start() {
                    write!(out, "\n{}\t", inst_counter)?;
                    self.print_instructions(&inst, out)?;
                    inst_counter += 1;
                }
                // write!(out, "\n")?;
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
            Instruction::StatementStart => write!(out, ""),
            Instruction::Binary(ref res, ref lhs, ref op, ref rhs) => write!(
                out,
                "{} <- {} {} {}",
                res.pretty(&self.symbols),
                lhs.pretty(&self.symbols),
                op,
                rhs.pretty(&self.symbols)
            ),
            Instruction::Store(ref dest, ref source) => write!(
                out,
                "{} <- {}",
                dest.pretty(&self.symbols),
                source.pretty(&self.symbols)
            ),
            Instruction::StoreI(ref dest, ref source) => {
                write!(out, "{} <- {}", dest.pretty(&self.symbols), source)
            }
            Instruction::Cast(ref dest, _, ref ty) => {
                write!(out, "{} as {}", dest.pretty(&self.symbols), ty)
            }
            Instruction::Unary(ref dest, ref source, ref op) => write!(
                out,
                "{} <- {}{}",
                dest.pretty(&self.symbols),
                op,
                source.pretty(&self.symbols)
            ),
            Instruction::Phi(ref dest, ref lhs, ref rhs) => write!(
                out,
                "{} <- φ({},{})",
                dest.pretty(&self.symbols),
                lhs.pretty(&self.symbols),
                rhs.pretty(&self.symbols)
            ),
            Instruction::Call(ref dest, ref callee, ref args) => {
                write!(
                    out,
                    "{} <- call {} ",
                    dest.pretty(&self.symbols),
                    self.symbols.name(*callee)
                )?;

                for arg in args {
                    write!(out, "{}\n", arg.pretty(&self.symbols))?;
                }

                write!(out, "")?;

                Ok(())
            }
        }
    }
}
