use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    fs::File,
    io::Write,
    sync::Arc,
    usize,
};

use errors::{FileId, WithError};
use semant::{
    hir::{BinOp, Expr, ExprId, FunctionAstMap, FunctionId, Literal, NameId, Stmt, StmtId},
    infer::{self},
    Resolver, SmolStr, Span, Type,
};

use crate::db::CodegenDatabase;

const FP_MAX: usize = 8;
const GP_MAX: usize = 6;
#[derive(Debug)]
struct Frame {
    /// The total space on the stack
    stack_size: usize,
}
#[derive(Debug)]
pub(crate) struct Codegen<DB> {
    db: DB,
    file: File,
    frame_info: HashMap<NameId, Frame>,
}

macro_rules! emit {
    ($self:ident, $fmt:expr) => {

  {  writeln!(&mut $self.file,$fmt)}

    };

   ($self:ident,$fmt:expr,$($arg:tt)+) => {

    writeln!(&mut $self.file,$fmt,$($arg)+)

    };

}

const fn align_to(n: usize, align: usize) -> usize {
    (n + align - 1) / align * align
}

const REGS: [Register; 6] = [RDI, RSI, RDX, RCX, R8, R9];

pub enum Register {
    Offset(usize),
    RAX,
    RDI,
    RSI,
    RDX,
    RCX,
    R8,
    R9,
    RSP,
    RBP,
    Label(usize),
}

use Register::*;

impl<'a, DB> Codegen<&'a DB>
where
    DB: CodegenDatabase,
{
    fn get_frame_info(&mut self, function: &infer::Function) -> Frame {
        // If a function has many parameters, some parameters are
        // inevitably passed by stack rather than by register.
        // The first passed-by-stack parameter resides at RBP+16.
        let mut top = 16;
        let mut bottom = 0;

        let gp = 0;
        let bp = 0;

        for (_, ty) in &function.params {
            top = align_to(top, 8);

            top += ty.size();
        }

        for (_, ty) in &function.params {
            // AMD64 System V ABI has a special alignment rule for an array of
            // length at least 16 bytes. We need to align such array to at least
            // 16-byte boundaries. See p.14 of
            // https://github.com/hjl-tools/x86-psABI/wiki/x86-64-psABI-draft.pdf.
            let alignment = match ty {
                ty @ Type::Con(semant::TypeCon::Array { .. }) => {
                    if ty.size() >= 16 {
                        std::cmp::max(16, ty.align())
                    } else {
                        ty.align()
                    }
                }
                ty => ty.align(),
            };

            bottom += ty.size();

            bottom = align_to(bottom, alignment);
        }

        Frame {
            stack_size: align_to(bottom, 16),
        }
    }

    pub fn generate_function(&mut self, function: &infer::Function) -> std::io::Result<()> {
        emit!(self, ".text")?;
        emit!(
            self,
            ".type {}, @function",
            self.db.lookup_intern_name(function.name.item)
        );
        emit!(self, "{}:", self.db.lookup_intern_name(function.name.item));
        emit!(self, "\tpush %rbp");
        emit!(self, "\tmov %rsp, %rbp");

        let frame = self.get_frame_info(function);
        emit!(self, "\tsub ${}, %rsp", frame.stack_size);

        let offset = function.params.len() * 8;

        // if let Some(body) = &function.body {
        //     for stmt in body {
        //         self.generate_statement(stmt, &function.ast_map)?;
        //     }
        // }

        self.emit("popq %rbp")?;
        self.emit("ret")?;
        Ok(())
    }

    pub fn emit<S>(&mut self, op: S) -> std::io::Result<()>
    where
        S: AsRef<str> + Display,
    {
        writeln!(&mut self.file, "\t{}", op)?;
        Ok(())
    }

    pub fn generate_statement(
        &mut self,
        id: &Span<StmtId>,
        map: &FunctionAstMap,
    ) -> std::io::Result<()> {
        let stmt = map.stmt(&id.item);

        match stmt {
            Stmt::Let {
                pat,
                ascribed_type,
                initializer,
            } => Ok(()),
            Stmt::Expr(id) => {
                let _ = self.generate_expr(id, map);

                Ok(())
            }
            Stmt::Error => self.emit("nop"),
        }
    }

    pub fn generate_expr(
        &mut self,
        id: &Span<ExprId>,
        map: &FunctionAstMap,
    ) -> std::io::Result<Register> {
        let expr = map.expr(&id.item);

        match expr {
            Expr::Tuple(_)
            | Expr::Unary { .. }
            | Expr::Return(_)
            | Expr::Match { .. }
            | Expr::Enum { .. }
            | Expr::RecordLiteral { .. }
            | Expr::Field(_)
            | Expr::Array(_)
            | Expr::Block(_, _)
            | Expr::Break
            | Expr::Call { .. }
            | Expr::Cast { .. }
            | Expr::Closure { .. }
            | Expr::Continue
            | Expr::If { .. }
            | Expr::Ident(_)
            | Expr::Index { .. }
            | Expr::While { .. } => unimplemented!(),
            Expr::Paren(expr) => self.generate_expr(expr, map),
            Expr::Binary { lhs, op, rhs } => {
                match op {
                    BinOp::Plus | BinOp::Minus => {
                        let lhs = self.generate_expr(lhs, map);
                        self.emit("pushq %rax")?;

                        let rhs = self.generate_expr(rhs, map);

                        self.emit("popq %rdx")?;
                        self.emit(format!(
                            "{} %rdx,%rax",
                            match op {
                                BinOp::Plus => {
                                    "addq"
                                }
                                BinOp::Minus => {
                                    "subq"
                                }
                                BinOp::Mult => {
                                    "mulq"
                                }
                                _ => unreachable!(),
                            }
                        ))?;
                    }
                    BinOp::Mult => {
                        let lhs = self.generate_expr(lhs, map);
                        self.emit("pushq %rax")?;

                        let rhs = self.generate_expr(rhs, map);
                        self.emit("popq %rdx")?;
                        self.emit("mulq %rdx")?;
                    }

                    BinOp::Div => {
                        let _ = self.generate_expr(rhs, map);
                        self.emit("pushq %rax")?;

                        let _ = self.generate_expr(lhs, map);

                        self.emit("popq %rdi")?;

                        self.emit("cltd")?;

                        self.emit("divq %rdi")?;
                    }
                    _ => {
                        unimplemented!()
                    }
                }

                Ok(Register::RAX)
            }
            Expr::Literal(literal) => {
                let literal = self.db.lookup_intern_literal(*literal);

                match literal {
                    Literal::True => {
                        self.emit("movq $1,%rax")?;
                        Ok(Register::RAX)
                    }
                    Literal::False => {
                        self.emit("movq $0,%rax")?;
                        Ok(Register::RAX)
                    }
                    Literal::Int(int) => {
                        let int = int.parse::<i64>().unwrap();

                        writeln!(&mut self.file, "\tmovq ${},%rax", int)?;
                        Ok(Register::RAX)
                    }
                    Literal::Float(float) => {
                        // let float = float.parse::<f64>().unwrap();
                        // self.emit_store_immediate(dest, Value::Float(Box::new(float.to_be_bytes())))

                        unimplemented!();
                    }
                    Literal::String(string) => {
                        // Support strings

                        // let label = self.label_count;
                        // self.label_count += 1;

                        // self.constants.push((label, string.clone()));

                        // Ok(Register::Label(label))
                        unimplemented!()
                    }
                    Literal::Nil => {
                        writeln!(&mut self.file, "\tmovq $0,%rax")?;
                        Ok(Register::RAX)
                    }
                }
            }
        }
    }
}

pub fn compile_to_asm_query(db: &impl CodegenDatabase, file: FileId) -> WithError<()> {
    let WithError(program, mut errors) = db.lower(file);
    let WithError(resolver, _) = db.resolve_source_file(file);
    let WithError(typed_program, error) = db.infer(file);
    errors.extend(error);
    let name = format!("{}.asm", db.name(file));

    let file = File::create(&name).expect("couldn't generate file");

    println!("{:#?}", resolver);

    let mut builder = Codegen {
        db,
        file,
        frame_info: HashMap::default(),
    };

    for function in &typed_program.functions {
        builder.generate_function(function).unwrap()
    }
    // builder.generate_constants().unwrap();

    WithError((), vec![])
}
