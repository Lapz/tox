use std::fs::File;
use std::io::{self, Read, Write};
use syntax::ast::{
    expr::{Expression, Literal, Op, UnaryOp},
    statement::Statement,
};

use label::Label;
use util::pos::Spanned;
use vm::Assembler;

#[derive(Debug)]
pub struct Compiler {
    file: File,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            file: File::create("output.tasm").expect("Couldn't create the file"),
        }
    }

    pub fn compile(&mut self, ast: &[Spanned<Statement>]) -> io::Result<()> {
        for node in ast {
            self.build_statement(node)?;
        }

        write!(&mut self.file, "HLT")?;

        Ok(())
    }

    pub fn assemble(&mut self) -> Option<Vec<u8>> {
        let mut assembler = Assembler::new();

        let mut contents = String::new();

        File::open("output.tasm")
            .expect("Couldn't open the file")
            .read_to_string(&mut contents)
            .expect("Coudln't read to file");

        assembler.assemble(&contents)
    }

    fn build_statement(&mut self, statement: &Spanned<Statement>) -> io::Result<()> {
        match &statement.value {
            Statement::Block(ref block) => {
                let label = Label::new();
                write!(&mut self.file, "{}", label);

                for statement in block {
                    self.build_statement(statement)?;
                }

                Ok(())
            }

            Statement::Expr(ref expr) => {
                self.build_expr(expr)?;
                Ok(())
            }

            _ => unimplemented!(),
        }
    }

    fn build_expr(&mut self, expr: &Spanned<Expression>) -> io::Result<()> {
        match &expr.value {
            Expression::Literal(ref literal) => match literal {
                Literal::Int(ref int) => {
                    write!(&mut self.file, "LOAD $0 #{}\n", int)?;
                    Ok(())
                }
                Literal::True(_) => {
                    write!(&mut self.file, "LOAD $0 #1\n")?;
                    Ok(())
                }

                Literal::False(_) => {
                    write!(&mut self.file, "LOAD $0 #0\n")?;
                    Ok(())
                }
                _ => unimplemented!(),
            },

            Expression::Binary {
                ref rhs,
                ref op,
                ref lhs,
            } => {
                match op.value {
                    Op::Plus => {
                        self.build_expr(lhs)?;

                        write!(&mut self.file, "PUSH $0\n")?;

                        self.build_expr(rhs)?;

                        write!(&mut self.file, "POP $1\n")?;
                        write!(&mut self.file, "ADD  $1 $0 $0\n")?;
                    }
                    Op::Minus => {
                        self.build_expr(lhs)?;

                        write!(&mut self.file, "PUSH $0\n")?;

                        self.build_expr(rhs)?;

                        write!(&mut self.file, "POP $1\n")?;
                        write!(&mut self.file, "SUB  $1 $0 $0\n")?;
                    }
                    Op::Slash => {
                        self.build_expr(lhs)?;

                        write!(&mut self.file, "PUSH $0\n")?;

                        self.build_expr(rhs)?;

                        write!(&mut self.file, "POP $1\n")?;
                        write!(&mut self.file, "DIV  $1 $0 $0\n")?;
                    }

                    Op::Star => {
                        self.build_expr(lhs)?;

                        write!(&mut self.file, "PUSH $0\n")?;

                        self.build_expr(rhs)?;

                        write!(&mut self.file, "POP $1\n")?;
                        write!(&mut self.file, "STAR  $1 $0 $0\n")?;
                    }

                    Op::Modulo => {
                        self.build_expr(lhs)?;

                        write!(&mut self.file, "PUSH $0\n")?;

                        self.build_expr(rhs)?;

                        write!(&mut self.file, "POP $1\n")?;

                        write!(&mut self.file, "MOD  $1 $0 $0 \n")?;
                    }

                    Op::Exponential => {
                        self.build_expr(lhs)?;

                        write!(&mut self.file, "PUSH $0\n")?;

                        self.build_expr(rhs)?;

                        write!(&mut self.file, "POP $1\n")?;

                        write!(&mut self.file, "EXPON  $1 $0 $0 \n")?;
                    }

                    Op::EqualEqual => {
                        self.build_expr(lhs)?;

                        write!(&mut self.file, "PUSH $0\n")?;

                        self.build_expr(rhs)?;

                        write!(&mut self.file, "POP $1\n")?;

                        write!(&mut self.file, "EQUAL $0 $1 \n")?;
                    }

                    Op::BangEqual => {
                        self.build_expr(lhs)?;

                        write!(&mut self.file, "PUSH $0\n")?;

                        self.build_expr(rhs)?;

                        write!(&mut self.file, "POP $1\n")?;

                        write!(&mut self.file, "EQUAL $0 $1 \n")?;
                        write!(&mut self.file, "NOT \n")?;
                    }

                    Op::LessThan => {
                        self.build_expr(lhs)?;

                        write!(&mut self.file, "PUSH $0\n")?;

                        self.build_expr(rhs)?;

                        write!(&mut self.file, "POP $1\n")?;

                        write!(&mut self.file, "LESS $0 $1 \n")?;
                    }

                    Op::LessThanEqual => {
                        self.build_expr(lhs)?;

                        write!(&mut self.file, "PUSH $0\n")?;

                        self.build_expr(rhs)?;

                        write!(&mut self.file, "POP $1\n")?;

                        write!(&mut self.file, "GREATER $0 $1 \n")?;

                        write!(&mut self.file, "NOT \n")?;
                    }

                    Op::GreaterThan => {
                        self.build_expr(lhs)?;

                        write!(&mut self.file, "PUSH $0\n")?;

                        self.build_expr(rhs)?;

                        write!(&mut self.file, "POP $1\n")?;

                        write!(&mut self.file, "GREATER  $1 $0 $0 \n")?;
                    }

                    Op::GreaterThanEqual => {
                        self.build_expr(lhs)?;

                        write!(&mut self.file, "PUSH $0\n")?;

                        self.build_expr(rhs)?;

                        write!(&mut self.file, "POP $1\n")?;

                        write!(&mut self.file, "LESS $0 $1 \n")?;

                        write!(&mut self.file, "NOT \n")?;
                    }

                    Op::And => {
                        self.build_expr(lhs)?;

                        write!(&mut self.file, "EQUAL $0 $1\n")?;

                        let label = Label::new();

                        write!(&mut self.file, "JMPEQ @{}\n", label);

                        self.build_expr(rhs)?;

                        write!(&mut self.file, "{}:\n", label);
                    }

                    Op::Or => {
                        self.build_expr(lhs)?;

                        write!(&mut self.file, "EQUAL $0 $1\n")?;

                        let label = Label::new();

                        write!(&mut self.file, "JMPNEQ @{}\n", label);

                        self.build_expr(rhs)?;

                        write!(&mut self.file, "{}:\n", label);
                    }
                }

                Ok(())
            }

            Expression::Unary { ref expr, ref op } => {
                self.build_expr(expr)?;
                write!(&mut self.file, "PUSH $0\n")?;

                match op.value {
                    UnaryOp::Minus => {
                        write!(&mut self.file, "LOAD $0 #0\n")?;
                        write!(&mut self.file, "POP $1\n")?;
                        write!(&mut self.file, "SUB  $1 $0 $0\n")?;
                    }
                    UnaryOp::Bang => unimplemented!(),
                }

                Ok(())
            }

            _ => unimplemented!(),
        }
    }
}
