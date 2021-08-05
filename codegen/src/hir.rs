use std::{collections::HashMap, fmt::Display, fs::File, io::Write, usize};

use errors::{FileId, WithError};
use semant::{
    hir::{BinOp, Literal, NameId},
    typed::{self, Expr, Pattern, Stmt},
    Function, Program, Span, Type, TypeCon, Typed,
};

use crate::db::CodegenDatabase;

const FP_MAX: usize = 8;
const GP_MAX: usize = 6;

#[repr(C)]
union FloatParts {
    f_bits: f32,
    i_bits: i32,
}
#[derive(Debug)]
struct Frame {
    /// The total space on the stack
    stack_size: isize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct VarInfo {
    local: bool,
    offset: isize,
}
#[derive(Debug)]
pub(crate) struct Codegen<DB> {
    db: DB,
    file: File,
    frame_info: HashMap<NameId, Frame>,
    offsets: HashMap<NameId, VarInfo>,
    current_name: Option<NameId>,
}

macro_rules! emit {
    ($self:ident, $fmt:expr) => {

  {  writeln!(&mut $self.file,$fmt)}

    };

   ($self:ident,$fmt:expr,$($arg:tt)+) => {

    writeln!(&mut $self.file,$fmt,$($arg)+)

    };

}

const fn align_to(n: isize, align: isize) -> isize {
    (n + align - 1) / align * align
}

impl<'a, DB> Codegen<&'a DB>
where
    DB: CodegenDatabase,
{
    fn assign_offset_for_pat(&mut self, pat: &Typed<Pattern>, offset: isize, local: bool) {
        match &pat.item {
            Pattern::Bind { name } => {
                self.offsets.insert(name.item, VarInfo { offset, local });
            }
            Pattern::Placeholder => {}
            Pattern::Tuple(pats) => {
                for p in pats {
                    self.assign_offset_for_pat(p, offset, local);
                }
            }
            Pattern::Literal(_) => {}
        }
    }
    fn get_frame_info(&mut self, function: &Typed<Function>) -> Frame {
        // If a function has many parameters, some parameters are
        // inevitably passed by stack rather than by register.
        // The first passed-by-stack parameter resides at RBP+16.
        let mut top = 16isize;
        let mut bottom = 0isize;

        let gp = 0;
        let bp = 0;

        for param in &function.item.params {
            top = align_to(top, 8);

            self.assign_offset_for_pat(&param.item.pat, top, false);

            top += param.ty.size();
        }

        for stmt in &function.item.body {
            match &stmt.item {
                Stmt::Let { pat, .. } => {
                    // AMD64 System V ABI has a special alignment rule for an array of
                    // length at least 16 bytes. We need to align such array to at least
                    // 16-byte boundaries. See p.14 of
                    // https://github.com/hjl-tools/x86-psABI/wiki/x86-64-psABI-draft.pdf.
                    let alignment = match &stmt.ty {
                        ty @ Type::Con(semant::TypeCon::Array { .. }) => {
                            if ty.size() >= 16 {
                                std::cmp::max(16, ty.align())
                            } else {
                                ty.align()
                            }
                        }
                        ty => ty.align(),
                    };

                    bottom += stmt.ty.size();

                    bottom = align_to(bottom, alignment);

                    self.assign_offset_for_pat(pat, -bottom, true);
                }
                _ => {}
            }
        }

        Frame {
            stack_size: align_to(bottom, 16),
        }
    }

    pub fn generate_function(&mut self, function: &Typed<Function>) -> std::io::Result<()> {
        emit!(self, ".text")?;

        self.current_name = Some(function.item.name.item);

        let name = self.db.lookup_intern_name(function.item.name.item);
        // emit!(
        //     self,
        //     ".type {}, %function",
        //     self.db.lookup_intern_name(function.name.item)
        // );

        if name.as_str() == "main" {
            emit!(self, ".globl _main");
            emit!(self, "_main:");
        } else {
            emit!(self, "{}:", name);
        }

        emit!(self, "\tpush %rbp");
        emit!(self, "\tmov %rsp, %rbp");

        let frame = self.get_frame_info(function);
        emit!(self, "\tsub ${}, %rsp", frame.stack_size);

        for stmt in &function.item.body {
            self.generate_statement(stmt)?;
        }

        emit!(self, ".L.return.{}:", name);
        self.emit("mov %rbp, %rsp")?;
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

    pub fn push(&mut self) -> std::io::Result<()> {
        emit!(self, "push %rax")
    }
    pub fn generate_statement(&mut self, stmt: &Typed<Stmt>) -> std::io::Result<()> {
        emit!(self, "# span {:?}", stmt.span);
        match &stmt.item {
            Stmt::Let {
                pat,
                ascribed_type,
                initializer,
            } => {
                self.get_addr(pat)?;

                self.push()?;

                if let Some(init) = initializer {
                    self.generate_expr(init)?;
                    self.store(&init.ty)?;
                } else {
                    self.store(&stmt.ty)?;
                }

                Ok(())
            }
            Stmt::Expr(expr) => {
                self.generate_expr(expr)?;

                Ok(())
            }
            Stmt::Error => self.emit("nop"),
        }
    }

    pub fn compare_zero(&mut self, ty: &Type) -> std::io::Result<()> {
        match ty {
            Type::Con(TypeCon::Float) => {
                emit!(self, "xorps %%xmm1, %%xmm1")?;
                emit!(self, "ucomiss %%xmm1, %%xmm0")
            }

            Type::Con(TypeCon::Float) if ty.size() <= 4 => emit!(self, "cmp $0,%eax"),
            _ => {
                emit!(self, "cmp $0,%rax")
            }
        }
    }

    pub fn get_addr(&mut self, pat: &Typed<Pattern>) -> std::io::Result<()> {
        match &pat.item {
            Pattern::Bind { name } => {
                let info = self.offsets.get(&name.item).unwrap();

                if info.local {
                    emit!(self, "lea {}(%rbp), %rax", info.offset)?;
                } else {
                }
                Ok(())
            }
            Pattern::Placeholder => todo!(),
            Pattern::Tuple(_) => todo!(),
            Pattern::Literal(_) => todo!(),
        }
    }

    pub fn load(&mut self, ty: &Type) -> std::io::Result<()> {
        match ty {
            Type::Con(TypeCon::Float) => {
                emit!(self, "movss %rax, %xmm0")?;
            }

            _ => {}
        }

        match ty.size() {
            1 => emit!(self, "movsbl (%rax), %eax"),
            2 => emit!(self, "movswl  (%rax), %eax"),
            4 => emit!(self, "mov (%rax), %rax"),
            _ => {
                emit!(self, "mov (%rax), %rax")
            }
        }
    }

    pub fn store(&mut self, ty: &Type) -> std::io::Result<()> {
        emit!(self, "pop %rdi");

        match ty {
            Type::Con(TypeCon::Float) => {
                emit!(self, "movss %xmm0, (%rdi)")?;
            }

            _ => {}
        }

        println!("{:?}", ty);

        match ty.size() {
            1 => emit!(self, " mov %al, (%rdi)"),
            2 => emit!(self, "mov %ax, (%rdi)"),
            4 => emit!(self, " mov %rax, (%rdi)"),
            _ => {
                emit!(self, "mov %rax, (%rdi)")
            }
        }
    }

    pub fn generate_expr(&mut self, expr: &Typed<Expr>) -> std::io::Result<()> {
        match &expr.item {
            Expr::Tuple(_)
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
            | Expr::Index { .. }
            | Expr::While { .. } => unimplemented!(),

            Expr::Return(expr) => {
                if let Some(e) = expr {
                    self.generate_expr(e)?;
                }

                let name = self.db.lookup_intern_name(self.current_name.unwrap());

                emit!(self, "jmp .L.return.{}", name)?;
            }
            Expr::Ident(ident) => {
                let info = self.offsets.get(&ident.item).unwrap();

                if info.local {
                    emit!(self, "lea {}(%rbp), %rax", info.offset)?;
                    self.load(&expr.ty)?;
                } else {
                    // TODO handle params and global variables
                }
            }
            Expr::Unary { expr, op } => match op {
                semant::hir::UnaryOp::Minus => {
                    self.generate_expr(expr)?;
                    let ty = Type::Unknown;

                    match ty {
                        Type::Con(TypeCon::Float) => {
                            emit!(self, "mov $1, %rax")?;
                            emit!(self, "shl $31, %rax")?;
                            emit!(self, "movq %raq, %xmm1")?;
                            emit!(self, "xorps %xmm1, %xmm0");
                        }
                        _ => {
                            emit!(self, "neg %rax");
                        }
                    }
                }
                semant::hir::UnaryOp::Excl => {
                    // self.generate_expr(expr, expr)?;
                    unimplemented!()
                }
            },
            Expr::Paren(expr) => {
                self.generate_expr(expr)?;
            }
            Expr::Binary { lhs, op, rhs } => match op {
                BinOp::Plus | BinOp::Minus => {
                    let lhs = self.generate_expr(lhs);
                    self.emit("pushq %rax")?;

                    let rhs = self.generate_expr(rhs);

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
                    let lhs = self.generate_expr(lhs);
                    self.emit("pushq %rax")?;

                    let rhs = self.generate_expr(rhs);
                    self.emit("popq %rdx")?;
                    self.emit("mulq %rdx")?;
                }

                BinOp::Div => {
                    let _ = self.generate_expr(rhs);
                    self.emit("pushq %rax")?;

                    let _ = self.generate_expr(lhs);

                    self.emit("popq %rdi")?;

                    self.emit("cltd")?;

                    self.emit("divq %rdi")?;
                }
                _ => {
                    unimplemented!()
                }
            },
            Expr::Literal(literal) => {
                let literal = self.db.lookup_intern_literal(*literal);

                match literal {
                    Literal::True => {
                        self.emit("mov $1,%rax")?;
                    }
                    Literal::False => {
                        self.emit("movq $0,%rax")?;
                    }
                    Literal::Int(int) => {
                        let int = int.parse::<i64>().unwrap();

                        writeln!(&mut self.file, "\tmovq ${},%rax", int)?;
                    }
                    Literal::Float(float) => {
                        let float = float.parse::<f32>().unwrap();

                        let u = unsafe { std::mem::transmute::<f32, FloatParts>(float) };

                        emit!(self, "mov ${}, %eax # float {}", unsafe { u.i_bits }, float);

                        emit!(self, "movq %rax, %xmm0");
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
                        writeln!(&mut self.file, "\tmov $0,%rax")?;
                    }
                }
            }
        };

        Ok(())
    }
}

pub fn compile_to_asm_query(db: &impl CodegenDatabase, file: FileId) -> WithError<()> {
    let WithError(program, mut errors) = db.lower(file);
    let WithError(resolver, _) = db.resolve_source_file(file);
    let WithError(typed_program, error) = db.infer(file);
    errors.extend(error);
    let name = format!("{}.asm", db.name(file));

    let file = File::create(&name).expect("couldn't generate file");

    let mut builder = Codegen {
        db,
        file,
        frame_info: HashMap::default(),
        offsets: HashMap::default(),
        current_name: None,
    };

    for function in &typed_program.functions {
        builder.generate_function(function).unwrap()
    }

    println!("{:#?}", builder.offsets);
    // builder.generate_constants().unwrap();

    WithError((), vec![])
}
