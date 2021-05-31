use std::{
    fmt::{write, Display},
    fs::File,
    io::{Seek, SeekFrom, Write},
    usize,
};

use errors::{FileId, WithError};
use semant::{
    hir::{BinOp, Expr, ExprId, Function, FunctionAstMap, FunctionId, Literal, Stmt, StmtId},
    SmolStr, Span,
};

use crate::db::CodegenDatabase;

#[derive(Debug)]
pub(crate) struct Codegen<DB> {
    db: DB,
    file: File,
    label_count: usize,
    constants: Vec<(usize, SmolStr)>,
}

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

const RUNTIME_START: &'static str = r#"
.text # code segment
.globl _main # label at start of compiled static main
_main:
        pushq   %rbp
        movq    %rsp, %rbp
        call    main
        popq    %rbp
        ret
"#;

impl<'a, DB> Codegen<&'a DB>
where
    DB: CodegenDatabase,
{
    pub fn generate_main(&mut self) -> std::io::Result<()> {
        writeln!(&mut self.file, "{}", RUNTIME_START)?;
        Ok(())
    }

    fn generate_constants(&mut self) -> std::io::Result<()> {
        for (label, string) in &self.constants {
            writeln!(
                &mut self.file,
                ".LC{}:\n \t.string \"{}\"",
                label, &**string
            )?;
        }

        Ok(())
    }

    pub fn generate_function(&mut self, function: &Function) -> std::io::Result<()> {
        writeln!(
            &mut self.file,
            "{}:\n\tpushq %rbp
            \tmovq %rsp, %rbp",
            self.db.lookup_intern_name(function.name.item)
        )?;

        if let Some(body) = &function.body {
            for stmt in body {
                self.generate_statement(stmt, &function.ast_map)?;
            }
        }

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
            Expr::Paren(_)
            | Expr::Tuple(_)
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
            Expr::Binary { lhs, op, rhs } => {
                let lhs = self.generate_expr(lhs, map);
                self.emit("pushq %rax")?;

                let rhs = self.generate_expr(rhs, map);

                self.emit("popq %rdx")?;

                match op {
                    BinOp::Plus => {
                        self.emit("addq %rdx,%rax")?;
                    }
                    BinOp::Minus => {
                        self.emit("subq %rdx,%rax")?;
                    }
                    BinOp::Mult => {
                        self.emit("mulq %rdx,%rax")?;
                    }
                    BinOp::Div => {
                        self.emit("divq %rax")?;
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

                        let label = self.label_count;
                        self.label_count += 1;

                        self.constants.push((label, string.clone()));

                        Ok(Register::Label(label))
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
    let WithError(type_map, error) = db.infer(file);
    errors.extend(error);
    let name = format!("{}.asm", db.name(file));

    let file = File::create(&name).expect("couldn't generate file");

    let mut builder = Codegen {
        db,
        file,
        label_count: 0,
        constants: vec![],
    };

    builder.generate_main().unwrap();

    for function in &program.functions {
        builder.generate_function(function).unwrap()
    }
    builder.generate_constants().unwrap();

    WithError((), vec![])
}
