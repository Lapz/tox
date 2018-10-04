use ast::*;
use codegen::label::Label;
use codegen::tasm::{Register, TASM};
use codegen::Compiler;
use std::io::{self, Write};
use std::iter::repeat;

use std::str;


impl  Compiler {
    pub(crate) fn write(&mut self, inst: TASM) -> io::Result<()> {
        write!(
            &mut self.file,
            "{}{}\n",
            repeat_string("\t", self.indent_level),
            inst
        )
    }


    pub fn process_strings(&mut self,statement:&Statement) -> () {

        match *statement {
            Statement::Block(ref block) => {
                for statement in block {
                    if let Statement::Expr(ref expr) = statement {
                        if let Expression::Literal(Literal::Str(ref bytes)) = &*expr.expr {
                            let label = Label::new();

                            self.strings.insert(label,str::from_utf8(bytes).unwrap().to_owned());

                        }
                    }
                }
            },
            _ => (),

        }

    }

    pub(crate) fn build_statement(&mut self, statement: &Statement) -> io::Result<()> {
        match statement {
            Statement::Block(ref block) => {
                let label = Label::new();
                self.write(TASM::LABEL(label))?;

                self.indent_level += 1;

                for statement in block {
                    self.build_statement(statement)?;
                }
                self.indent_level -= 1;
            }

            Statement::Expr(ref expr) => {
                self.build_expr(expr)?;
            }
            Statement::While(ref cond, ref body) => {
                let label = Label::new();
                self.write(TASM::LABEL(label))?;

                self.build_statement(body)?;

                self.build_expr(cond)?;

                self.write(TASM::SET(Register(0)))?;

                self.write(TASM::JMPEQ(label))?;
            },

            Statement::Return(ref expr) => {},

            Statement::If { ref cond, ref then, ref otherwise } => {
                let label = Label::new();


                if let Some(ref other) = *otherwise {
                    // Generate the condition
                    self.build_expr(cond)?;


                    self.write(TASM::JMPNEQ(label))?; // jump to the label if the cond is false;

                    self.build_statement(then)?; // else follow through to the body

                    self.write(TASM::LABEL(label))?; // The label to jump to if the cond is false

                    self.build_statement(&other)?;
                } else {
                    // Generate the condition
                    self.build_expr(cond)?;

                    self.write(TASM::JMPNEQ(label))?; // jump to the label if the cond is false;

                    self.build_statement(then)?; // Follow through to the body

                    self.write(TASM::LABEL(label))?; // The label to jump to if the cond is false
                }

            }

            _ => unimplemented!(),
        }

        Ok(())
    }

    fn build_expr(&mut self, expr: &TypedExpression) -> io::Result<()> {
        match &*expr.expr {
            Expression::Literal(ref literal) => match literal {
                Literal::Int(ref int) => {
                    self.write(TASM::LOAD(Register(0), *int))?;
                }
                Literal::True(_) => {
                    self.write(TASM::LOAD(Register(0), 1))?;
                }

                Literal::False(_) => {
                    self.write(TASM::LOAD(Register(0), 0))?;
                }
                Literal::Nil => self.write(TASM::LOAD(Register(0), 0x2))?,

                Literal::Str(ref bytes) => (), // Strings are handled by a string preprocessing step
                _ => unimplemented!()

            },

            Expression::Binary(ref lhs, ref op, ref rhs) => match op {
                Op::Plus => {
                    self.build_expr(lhs)?;

                    self.write(TASM::PUSH(Register(0)))?;

                    self.build_expr(rhs)?;

                    self.write(TASM::POP(Register(1)))?;

                    self.write(TASM::ADD(Register(1), Register(0), Register(0)))?;
                }
                Op::Minus => {
                    self.build_expr(lhs)?;

                    self.write(TASM::PUSH(Register(0)))?;

                    self.build_expr(rhs)?;

                    self.write(TASM::POP(Register(1)))?;
                    self.write(TASM::SUB(Register(1), Register(0), Register(0)))?;
                }
                Op::Slash => {
                    self.build_expr(lhs)?;

                    self.write(TASM::PUSH(Register(0)))?;

                    self.build_expr(rhs)?;

                    self.write(TASM::POP(Register(1)))?;
                    self.write(TASM::DIV(Register(1), Register(0), Register(0)))?;
                }

                Op::Star => {
                    self.build_expr(lhs)?;

                    self.write(TASM::PUSH(Register(0)))?;

                    self.build_expr(rhs)?;

                    self.write(TASM::POP(Register(1)))?;
                    self.write(TASM::MUL(Register(1), Register(0), Register(0)))?;
                }

                Op::Modulo => {
                    self.build_expr(lhs)?;

                    self.write(TASM::PUSH(Register(0)))?;

                    self.build_expr(rhs)?;

                    self.write(TASM::POP(Register(1)))?;

                    self.write(TASM::MOD(Register(1), Register(0), Register(0)))?;
                }

                Op::Exponential => {
                    self.build_expr(lhs)?;

                    self.write(TASM::PUSH(Register(0)))?;

                    self.build_expr(rhs)?;

                    self.write(TASM::POP(Register(1)))?;

                    self.write(TASM::EXPON(Register(1), Register(0), Register(0)))?;
                }

                Op::EqualEqual => {
                    self.build_expr(lhs)?;

                    self.write(TASM::PUSH(Register(0)))?;

                    self.build_expr(rhs)?;

                    self.write(TASM::POP(Register(1)))?;

                    self.write(TASM::EQUAL(Register(0), Register(1)))?;
                }

                Op::BangEqual => {
                    self.build_expr(lhs)?;

                    self.write(TASM::PUSH(Register(0)))?;

                    self.build_expr(rhs)?;

                    self.write(TASM::POP(Register(1)))?;

                    self.write(TASM::EQUAL(Register(0), Register(1)))?;
                    self.write(TASM::NOT)?;
                }

                Op::LessThan => {
                    self.build_expr(lhs)?;

                    self.write(TASM::PUSH(Register(0)))?;

                    self.build_expr(rhs)?;

                    self.write(TASM::POP(Register(1)))?;

                    self.write(TASM::LESS(Register(1), Register(0)))?;
                }

                Op::LessThanEqual => {
                    self.build_expr(lhs)?;

                    self.write(TASM::PUSH(Register(0)))?;

                    self.build_expr(rhs)?;

                    self.write(TASM::POP(Register(1)))?;

                    self.write(TASM::GREATER(Register(0), Register(1)))?;

                    self.write(TASM::NOT)?;
                }

                Op::GreaterThan => {
                    self.build_expr(lhs)?;

                    self.write(TASM::PUSH(Register(0)))?;

                    self.build_expr(rhs)?;

                    self.write(TASM::POP(Register(1)))?;

                    self.write(TASM::GREATER(Register(0), Register(1)))?;
                }

                Op::GreaterThanEqual => {
                    self.build_expr(lhs)?;

                    self.write(TASM::PUSH(Register(0)))?;

                    self.build_expr(rhs)?;

                    self.write(TASM::POP(Register(1)))?;

                    self.write(TASM::LESS(Register(0), Register(1)))?;

                    self.write(TASM::NOT)?;
                }

                Op::And => {
                    self.build_expr(lhs)?;

                    self.write(TASM::EQUAL(Register(0), Register(1)))?;

                    let label = Label::new();

                    self.write(TASM::JMPEQ(label))?;

                    self.build_expr(rhs)?;

                    self.write(TASM::LABEL(label))?;
                }

                Op::Or => {
                    self.build_expr(lhs)?;

                    self.write(TASM::EQUAL(Register(0), Register(1)))?;

                    let label = Label::new();

                    self.write(TASM::JMPNEQ(label))?;

                    self.build_expr(rhs)?;

                    self.write(TASM::LABEL(label))?;;
                }
            },

            Expression::Grouping(ref expr) => self.build_expr(expr)?,

            Expression::Unary(ref op, ref expr) => {
                self.build_expr(expr)?;
                self.write(TASM::PUSH(Register(0)))?;

                match op {
                    UnaryOp::Minus => {
                        // Negation is desugared into 0-$VAL;
                        self.write(TASM::LOAD(Register(0), 0))?;
                        self.write(TASM::POP(Register(1)))?;
                        self.write(TASM::SUB(Register(1), Register(0), Register(0)))?;
                    }
                    UnaryOp::Bang => {
                        // TODO WOKR ON IMPLEMENT THIS
                        write!(&mut self.file, "NOT $0\n")?;
                    }
                }
            }

            ref e => unimplemented!(),
        }

        Ok(())
    }
}

fn repeat_string(s: &str, count: usize) -> String {
    repeat(s).take(count).collect()
}
