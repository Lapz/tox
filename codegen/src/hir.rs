use std::{
    collections::HashMap, fmt::Display, fs::File, hint::unreachable_unchecked, io::Write, usize,
};

use errors::{FileId, WithError};

use semant::{
    hir::{BinOp, Literal, NameId},
    typed::{Expr, Pattern, Stmt},
    Function, Type, TypeCon, Typed,
};

use crate::db::CodegenDatabase;

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
struct Block {
    start: String,
}
#[derive(Debug)]
pub(crate) struct Codegen<DB> {
    db: DB,
    file: File,
    file_id: FileId,
    frame_info: HashMap<NameId, Frame>,
    offsets: HashMap<NameId, VarInfo>,
    current_name: Option<NameId>,
    current_block: Option<Block>,
    label_count: usize,
}

macro_rules! emit {


    ($self:ident, $fmt:expr) => {

  {   write!(&mut $self.file,"\t");
      writeln!(&mut $self.file,$fmt)}

    };

   ($self:ident,$fmt:expr,$($arg:tt)+) => {
   {
       write!(&mut $self.file,"\t");
        writeln!(&mut $self.file,$fmt,$($arg)+)
    }

    };

    ($self:expr, $fmt:expr) => {
        write!(&mut $self,"\t");
         writeln!(&mut $self,$fmt)
     };

      ($self:expr,$fmt:expr,$($arg:tt)+) => {
   {
       write!(&mut $self,"\t");
        writeln!(&mut $self,$fmt,$($arg)+)
    }

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

    fn get_offset_for_pat(&mut self, pat: &Typed<Pattern>) -> Option<isize> {
        match &pat.item {
            Pattern::Bind { name } => Some(self.offsets.get(&name.item).unwrap().offset),
            Pattern::Placeholder => None,
            Pattern::Tuple(pats) => {
                for p in pats {
                    return self.get_offset_for_pat(p);
                }

                None
            }
            Pattern::Literal(_) => None,
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
                Stmt::Let {
                    pat, initializer, ..
                } => {
                    // AMD64 System V ABI has a special alignment rule for an array of
                    // length at least 16 bytes. We need to align such array to at least
                    // 16-byte boundaries. See p.14 of
                    // https://github.com/hjl-tools/x86-psABI/wiki/x86-64-psABI-draft.pdf.
                    let alignment = match initializer.as_ref().map(|ty| &ty.ty).unwrap_or(&stmt.ty)
                    {
                        ty @ Type::Con(semant::TypeCon::Array { .. }) => {
                            if ty.size() >= 16 {
                                std::cmp::max(16, ty.align())
                            } else {
                                ty.align()
                            }
                        }
                        ty => ty.align(),
                    };
                    println!("Alignment {:?} {:?}", alignment, stmt.ty);

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

        emit!(self, "push %rbp");
        emit!(self, "mov %rsp, %rbp");

        let frame = self.get_frame_info(function);
        emit!(self, "sub ${}, %rsp", frame.stack_size);

        for stmt in &function.item.body {
            self.generate_statement(stmt)?;
        }

        emit!(self, "lea format(%rip), %rdi")?;
        emit!(self, "mov (%rax), %esi")?;
        emit!(self, "xor %eax, %eax")?;
        emit!(self, "call _printf")?;

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

    pub fn pushf(&mut self) -> std::io::Result<()> {
        emit!(self, "sub $8, %rsp")?;
        emit!(self, "movsd %xmm0, (%rsp)")
    }

    pub fn popf(&mut self, reg: usize) -> std::io::Result<()> {
        emit!(self, "movsd (%rsp), %xmm{}", reg)?;
        emit!(self, "add $8, %rsp")
    }
    pub fn generate_statement(&mut self, stmt: &Typed<Stmt>) -> std::io::Result<()> {
        // emit!(
        //     self,
        //     ".loc {} {} {}",
        //     self.file_id.as_intern_id().as_u32(),
        //     stmt.span.0.to_usize(),
        //     stmt.span.1.to_usize()
        // );
        match &stmt.item {
            Stmt::Let {
                pat, initializer, ..
            } => {
                if let Some(init) = initializer {
                    self.generate_expr(init)?;
                    // self.store(&init.ty)?;
                } else {
                    // self.store(&stmt.ty)?;
                }

                let offset = self.get_offset_for_pat(pat);

                if let Some(offset) = offset {
                    emit!(self, "movq %rax, {}(%rbp)", offset)?;
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

    pub fn get_addr(&mut self, pat: &Typed<Pattern>) -> std::io::Result<()> {
        match &pat.item {
            Pattern::Bind { name } => {
                let info = self.offsets.get(&name.item).unwrap();

                if info.local {
                    emit!(self, "movq {}(%rbp), %rax", info.offset)?;
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
            4 => emit!(self, "movslq (%rax), %rax"),
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

        match ty.size() {
            1 => emit!(self, "mov %al, (%rdi)"),
            2 => emit!(self, "mov %ax, (%rdi)"),
            4 => emit!(self, "mov %eax, (%rdi)"),
            _ => {
                emit!(self, "mov %rax, (%rdi)")
            }
        }
    }

    fn cmp_zero(&mut self, ty: &Type) -> std::io::Result<()> {
        match ty {
            Type::Con(TypeCon::Float) => {
                emit!(self, "xorps %xmm1, %xmm1")?;
                emit!(self, "ucomiss %xmm1, %xmm0")
            }
            ty if ty.size() <= 4 => {
                emit!(self, "cmp $0, %eax")
            }
            _ => {
                emit!(self, "cmp $0, %rax")
            }
        }
    }

    fn new_label(&mut self) -> String {
        let count = self.label_count;

        self.label_count += 1;

        format!("L.{}", count)
    }

    pub fn generate_expr(&mut self, expr: &Typed<Expr>) -> std::io::Result<()> {
        match &expr.item {
            Expr::Tuple(_)
            | Expr::Match { .. }
            | Expr::Enum { .. }
            | Expr::RecordLiteral { .. }
            | Expr::Field(_)
            | Expr::Array(_)
            | Expr::Break
            | Expr::Call { .. }
            | Expr::Cast { .. }
            | Expr::Closure { .. }
            | Expr::Continue
            | Expr::Index { .. } => unimplemented!("{:?}", expr.item),

            Expr::Block(stmts, _) => {
                for stmt in stmts {
                    self.generate_statement(stmt)?;
                }
            }

            Expr::If {
                cond,
                then_branch,
                else_branch: Some(else_branch),
            } => {
                let after = self.new_label();
                let else_expr = self.new_label();

                self.generate_expr(cond)?;

                emit!(self, "jz {}", else_expr);

                self.generate_expr(then_branch)?;

                emit!(self, "jmp {}", after)?;

                emit!(self, "{}:", else_expr)?;

                self.generate_expr(else_branch)?;

                emit!(self, "{}:", after)?;
            }
            Expr::If {
                cond,
                then_branch,
                else_branch: None,
            } => {
                let after = self.new_label();

                self.generate_expr(cond)?;

                emit!(self, "jz {}", after);

                self.generate_expr(then_branch)?;
                emit!(self, "{}:", after)?;
            }
            Expr::While { cond, body } => {
                let start = self.new_label();
                let after = self.new_label();

                emit!(self, "{}:", start);

                self.generate_expr(cond)?;

                emit!(self, "jz {}", after);

                self.generate_expr(body)?;

                emit!(self, "jmp {}", start)?;

                emit!(self, "{}:", after)?;
            }
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
                            emit!(self, "negq %rax");
                        }
                    }
                }
                semant::hir::UnaryOp::Excl => {
                    self.generate_expr(expr)?;
                    self.cmp_zero(&expr.ty)?;

                    emit!(self, "sete %al")?;
                    emit!(self, "movzx %al, %rax")?;
                }
            },
            Expr::Paren(expr) => {
                self.generate_expr(expr)?;
            }

            Expr::Binary { lhs, op, rhs } => {
                match (op, &lhs.ty) {
                    (BinOp::Equal, _) => {}
                    (BinOp::MinusEqual, _) => {}
                    (BinOp::PlusEqual, _) => {}
                    (BinOp::MultEqual, _) => {}
                    (BinOp::DivEqual, _) => {}
                    (op, Type::Con(TypeCon::Int) | Type::Con(TypeCon::Bool)) => {
                        match op {
                            BinOp::Plus | BinOp::Minus => {
                                self.generate_expr(lhs)?;
                                self.push()?;
                                self.generate_expr(rhs)?;
                                emit!(self, "popq %rdx")?;
                                emit!(
                                    self,
                                    "{} %rdx, %rax",
                                    match op {
                                        BinOp::Plus => {
                                            "addq"
                                        }
                                        BinOp::Minus => {
                                            "subq"
                                        }

                                        _ => unsafe { unreachable_unchecked() }, // safe because we check in the statement before that we are only using + - *;
                                    }
                                )?;
                            }

                            BinOp::Mult => {
                                self.generate_expr(lhs)?;
                                self.push()?;

                                self.generate_expr(rhs)?;

                                emit!(self, "popq %rdx")?;
                                emit!(self, "mulq %rdx")?;
                            }

                            BinOp::Div => {
                                self.generate_expr(rhs)?;
                                self.push()?;

                                self.generate_expr(lhs)?;

                                emit!(self, "popq %rdi")?;

                                if lhs.ty.size() == 8 {
                                    emit!(self, "cqto")?
                                } else {
                                    emit!(self, "cqto")?
                                }

                                emit!(self, "divq %rdi")?;
                            }
                            BinOp::LessThan
                            | BinOp::LessThanEqual
                            | BinOp::GreaterThan
                            | BinOp::GreaterThanEqual
                            | BinOp::EqualEqual
                            | BinOp::NotEqual => {
                                self.generate_expr(lhs)?;
                                self.push()?;

                                self.generate_expr(rhs)?;

                                emit!(self, "popq %rdx")?;

                                emit!(self, "cmpq %rax, %rdx")?;

                                match op {
                                    BinOp::LessThan => {
                                        emit!(self, "setl %al")?;
                                    }
                                    BinOp::LessThanEqual => {
                                        emit!(self, "setle %al")?;
                                    }

                                    BinOp::GreaterThan => {
                                        emit!(self, "setg %al")?;
                                    }
                                    BinOp::GreaterThanEqual => {
                                        emit!(self, "setge %al")?;
                                    }

                                    BinOp::NotEqual => {
                                        emit!(self, "setne %al")?;
                                    }

                                    BinOp::EqualEqual => {
                                        emit!(self, "sete %al")?;
                                    }
                                    _ => unreachable!(),
                                }

                                emit!(self, "andb $1,%al")?;

                                emit!(self, "movzbq  %al, %rax")?;
                            }

                            _ => {
                                unreachable!()
                            }
                        }
                    }
                    (op, Type::Con(TypeCon::Float)) => {
                        self.generate_expr(lhs)?;
                        self.pushf()?;
                        self.generate_expr(rhs)?;
                        self.popf(1)?;

                        match op {
                            BinOp::Plus => emit!(self, "addss %xmm1, %xmm0")?,
                            BinOp::Minus => emit!(self, "subss %xmm1, %xmm0")?,
                            BinOp::Mult => emit!(self, "mulss %xmm1, %xmm0")?,
                            BinOp::Div => emit!(self, "divss %xmm1, %xmm0")?,
                            BinOp::LessThan
                            | BinOp::LessThanEqual
                            | BinOp::GreaterThan
                            | BinOp::GreaterThanEqual
                            | BinOp::EqualEqual
                            | BinOp::NotEqual => {
                                emit!(self, "ucomiss %xmm0, %xmm1")?;

                                match op {
                                    BinOp::LessThan => {
                                        emit!(self, "ucomiss %xmm1, %xmm0")?;
                                        emit!(self, "seta %al")?;
                                    }
                                    BinOp::LessThanEqual => {
                                        emit!(self, "ucomiss %xmm1, %xmm0")?;
                                        emit!(self, "setae %al")?;
                                    }

                                    BinOp::GreaterThan => {
                                        emit!(self, "seta %al")?;
                                    }
                                    BinOp::GreaterThanEqual => {
                                        emit!(self, "setae %al")?;
                                    }

                                    BinOp::NotEqual => {
                                        emit!(self, "setne %al")?;
                                        emit!(self, "setp %cl")?;
                                        emit!(self, "orb %cl, %al")?;
                                    }

                                    BinOp::EqualEqual => {
                                        emit!(self, "setne %al")?;
                                        emit!(self, "setp %cl")?;
                                        emit!(self, "and %cl, %al")?;
                                    }
                                    _ => unreachable!(),
                                }

                                emit!(self, "andb $1,%al")?;

                                emit!(self, "movzbq  %al, %rax")?;
                            }
                            _ => {
                                unreachable!()
                            }
                        }
                    }

                    (BinOp::And, _) => {
                        let skip = self.new_label();
                        let after = self.new_label();

                        self.generate_expr(lhs)?;
                        self.cmp_zero(&lhs.ty)?;

                        emit!(self, "je {}", skip)?;

                        self.generate_expr(rhs)?;
                        self.cmp_zero(&rhs.ty)?;

                        emit!(self, "je {}", skip)?;

                        emit!(self, "movq $1, %rax")?;

                        emit!(self, "jmp {}", after)?;
                        emit!(self, "{}:", skip)?;
                        emit!(self, "movq $0, %rax")?;
                        emit!(self, "{}:", after)?;
                    }
                    (BinOp::Or, _) => {
                        let after = self.new_label();
                        let true_part = self.new_label();

                        self.generate_expr(lhs)?;
                        self.cmp_zero(&lhs.ty)?;

                        emit!(self, "jne {}", after)?;

                        self.generate_expr(rhs)?;
                        self.cmp_zero(&rhs.ty)?;

                        emit!(self, "jne {}", after)?;

                        emit!(self, "movq $0, %rax")?;

                        emit!(self, "jmp {}", after)?;
                        emit!(self, "{}:", true_part)?;
                        emit!(self, "movq $1, %rax")?;
                        emit!(self, "{}:", after)?;
                    }

                    _ => {
                        unreachable!()
                    }
                }
            }
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

pub fn compile_to_asm_query(db: &impl CodegenDatabase, file_id: FileId) -> WithError<()> {
    let WithError(program, mut errors) = db.lower(file_id);
    let WithError(resolver, _) = db.resolve_source_file(file_id);
    let WithError(typed_program, error) = db.infer(file_id);
    errors.extend(error);
    let name = format!("{}.s", db.name(file_id));

    let file = File::create(&name).expect("couldn't generate file");

    let mut builder = Codegen {
        db,
        file,
        file_id,
        frame_info: HashMap::default(),
        offsets: HashMap::default(),
        current_name: None,
        current_block: None,
        label_count: 0,
    };

    emit!(builder.file, ".file 1 \"{}\"", db.name(file_id)).unwrap();

    for function in &typed_program.functions {
        builder.generate_function(function).unwrap()
    }

    emit!(builder.file, "format: .asciz \"%d\n\"");

    assert!(builder.current_block.is_none()); // TODO debug mode this

    println!("{:#?}", builder.offsets);
    // builder.generate_constants().unwrap();

    WithError((), vec![])
}
