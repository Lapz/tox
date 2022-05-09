// use std::io::{self, Write};
// use errors::WithError;
// use crate::db::IrDatabase;
//
// pub struct Printer<DB>  {
//     db: DB,
// }
//
// fn repeat_string(s: &str, count: usize) -> String {
//     use std::iter::repeat;
//     repeat(s).take(count).collect()
// }
//
// impl<'a, DB> Printer<&'a DB> where  DB: IrDatabase,{
//
//     pub fn print_program<T: Write>(mut self, p: &Program, out: &mut T) -> io::Result<()> {
//         for function in p.functions.iter() {
//             self.print_function(function, out)?;
//             write!(out, "\n")?;
//         }
//
//         Ok(())
//     }
//
//     pub fn print_function<T: Write>(&mut self, f: &Function, out: &mut T) -> io::Result<()> {
//         write!(out, "fn ")?;
//         write!(out, "{}", self.interner.lookup(f.name))?;
//
//         write!(out, "(")?;
//
//         for (i, param) in f.params.iter().enumerate() {
//             if (i + 1) == f.params.len() {
//                 write!(out, "{}", param)?;
//             } else {
//                 write!(out, "{},", param)?;
//             }
//         }
//
//         write!(out, ") {{\n\n")?;
//
//         write!(out, "{}start {}\n\n", repeat_string(" ", 4), f.start_block)?;
//
//         for (id, block) in f.blocks.iter() {
//             let str_id = format!("{}", id);
//             let indent_level = 4 + str_id.len();
//             write!(out, "{}{:<2}:\n", repeat_string(" ", 4), str_id)?;
//
//             for inst in block.instructions.iter() {
//                 self.print_instructions(indent_level, &inst, out)?;
//             }
//
//             write!(out, "{} ", repeat_string(" ", indent_level))?;
//
//             match block.end {
//                 BlockEnd::End => write!(out, "end")?,
//                 BlockEnd::Jump(ref id) => write!(out, "goto {}", id)?,
//                 BlockEnd::Link(ref id) => write!(out, "link {}", id)?,
//                 BlockEnd::Return(ref value) => write!(out, "return {}", value)?,
//                 BlockEnd::Branch(ref value, ref t, ref f) => {
//                     write!(out, "branch {} {} {}", value, t, f)?
//                 }
//             }
//
//             write!(out, "\n")?;
//         }
//
//         write!(out, "}}\n")?;
//
//         Ok(())
//     }
//
//     pub fn print_instructions<T: Write>(
//         &mut self,
//         indent_level: usize,
//         i: &Instruction,
//         out: &mut T,
//     ) -> io::Result<()> {
//         write!(out, "{} ", repeat_string(" ", indent_level))?;
//         match *i {
//             Instruction::Array(ref l, ref s) => writeln!(out, "{} <- [{}]", l, s),
//
//             Instruction::Binary(ref res, ref lhs, ref op, ref rhs) => {
//                 writeln!(out, "{} <- {} {} {}", res, lhs, op, rhs)
//             }
//             Instruction::Store(ref dest, ref source, ref offset) => {
//                 if offset > &0 {
//                     writeln!(out, "{} <- mem[{}+{}]", dest, source, offset)
//                 } else {
//                     writeln!(out, "{} <- {}", dest, source)
//                 }
//             }
//             Instruction::StoreI(ref dest, ref source) => writeln!(out, "{} <- {}", dest, source),
//             Instruction::Cast(ref dest, _, ref ty) => writeln!(out, "{} as {}", dest, ty),
//             Instruction::Unary(ref dest, ref source, ref op) => {
//                 writeln!(out, "{} <- {}{}", dest, op, source)
//             }
//             Instruction::Return(ref label) => writeln!(out, "return @{}", label),
//             Instruction::Call(ref dest, ref callee, ref args) => {
//                 writeln!(out, "{} <- call {} ", dest, self.interner.lookup(*callee))?;
//
//                 for arg in args {
//                     writeln!(out, "{}\n", arg)?;
//                 }
//
//                 writeln!(out, "")?;
//
//                 Ok(())
//             }
//         }
//     }
// }
