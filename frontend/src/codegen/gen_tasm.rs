use ast::*;
use codegen::label::Label;
use codegen::Compiler;
use std::io::{self, Read, Write};

impl Compiler {
    pub(crate) fn build_statement(&mut self, statement: &Statement) -> io::Result<()> {
        match statement {
            Statement::Block(ref block) => {
                let label = Label::new();
                write!(&mut self.file, "{}", label)?;

                for statement in block {
                    self.build_statement(statement)?;
                }
            }

            Statement::Expr(ref expr) => {
                self.build_expr(expr)?;
            }
            _ => unimplemented!(),
        }

        Ok(())
    }

    fn build_expr(&mut self, expr: &TypedExpression) -> io::Result<()> {
        match &*expr.expr {
            Expression::Literal(ref literal) => match literal {
                Literal::Int(ref int) => {
                    write!(&mut self.file, "LOAD $0 #{}\n", int)?;
                }
                Literal::True(_) => {
                    write!(&mut self.file, "LOAD $0 #1\n")?;
                }

                Literal::False(_) => {
                    write!(&mut self.file, "LOAD $0 #0\n")?;
                }
                _ => unimplemented!(),
            },

            Expression::Binary(ref lhs, ref op, ref rhs) => match op {
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
            },

            Expression::Unary(ref op, ref expr) => {
                self.build_expr(expr)?;
                write!(&mut self.file, "PUSH $0\n")?;

                match op {
                    UnaryOp::Minus => {
                        // Negation is desugared into 0-$VAL;
                        write!(&mut self.file, "LOAD $0 #0\n")?;
                        write!(&mut self.file, "POP $1\n")?;
                        write!(&mut self.file, "SUB  $0 $1 $0\n")?;
                    }
                    UnaryOp::Bang => {
                        write!(&mut self.file, "NOT $0\n")?;
                    }
                }
            }

            _ => unimplemented!(),
        }

        Ok(())
    }
}
