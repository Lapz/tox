use ast::*;
use codegen::label::Label;
use codegen::Compiler;
use std::io::{self, Write};
use codegen::tasm::{TASM,Register};
use std::iter::repeat;
enum Type {
    Float,
    Int,
}


impl Compiler {
    
    fn write(&mut self,inst:TASM) -> io::Result<()> {
        write!(&mut self.file, "{}{}\n",repeat_string("\t",self.indent_level),inst)
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
            },
            Statement::While(ref cond,ref body) => {
                let label = Label::new();
                self.write(TASM::LABEL(label))?;

                self.build_statement(body)?;

                self.build_expr(cond)?;

                self.write(TASM::JMPEQ(label))?;


            }
            _ => unimplemented!(),
        }

        Ok(())
    }

    fn build_expr(&mut self, expr: &TypedExpression) -> io::Result<()> {
        match &*expr.expr {
            Expression::Literal(ref literal) => match literal {
                Literal::Int(ref int) => {
                    self.write(TASM::LOAD(Register(0),*int))?;
                    // write!(&mut self.file, "LOAD $0 #{}\n", int)?;
                }
                Literal::True(_) => {
                    self.write(TASM::LOAD(Register(0),1))?;
                }

                Literal::False(_) => {
                     self.write(TASM::LOAD(Register(0),0))?;
                },
                Literal::Nil => self.write(TASM::LOAD(Register(0),-1))?,
                ref e => unimplemented!("{:?}",e),
            },

            Expression::Binary(ref lhs, ref op, ref rhs) => match op {
                Op::Plus => {
                    self.build_expr(lhs)?;

                    self.write(TASM::PUSH(Register(0)))?;

                    self.build_expr(rhs)?;

                    self.write(TASM::POP(Register(1)))?;


                    self.write(TASM::ADD(Register(1),Register(0),Register(0) ))?;

                }
                Op::Minus => {
                    self.build_expr(lhs)?;

                    self.write(TASM::PUSH(Register(0)))?;

                    self.build_expr(rhs)?;

                    self.write(TASM::POP(Register(1)))?;
                    self.write(TASM::SUB(Register(1),Register(0),Register(0) ))?;
                }
                Op::Slash => {
                    self.build_expr(lhs)?;

                    self.write(TASM::PUSH(Register(0)))?;

                    self.build_expr(rhs)?;

                    self.write(TASM::POP(Register(1)))?;
                   self.write(TASM::DIV(Register(1),Register(0),Register(0) ))?;
                }

                Op::Star => {
                    self.build_expr(lhs)?;

                    self.write(TASM::PUSH(Register(0)))?;

                    self.build_expr(rhs)?;

                    self.write(TASM::POP(Register(1)))?;
                    self.write(TASM::MUL(Register(1),Register(0),Register(0) ))?;
                }

                Op::Modulo => {
                    self.build_expr(lhs)?;

                    self.write(TASM::PUSH(Register(0)))?;

                    self.build_expr(rhs)?;

                    self.write(TASM::POP(Register(1)))?;

                   self.write(TASM::MOD(Register(1),Register(0),Register(0) ))?;
                }

                Op::Exponential => {
                    self.build_expr(lhs)?;

                    self.write(TASM::PUSH(Register(0)))?;

                    self.build_expr(rhs)?;

                    self.write(TASM::POP(Register(1)))?;

                    self.write(TASM::EXPON(Register(1),Register(0),Register(0) ))?;
                }

                Op::EqualEqual => {
                    self.build_expr(lhs)?;

                    self.write(TASM::PUSH(Register(0)))?;

                    self.build_expr(rhs)?;

                    self.write(TASM::POP(Register(1)))?;

                    self.write(TASM::EQUAL(Register(0),Register(1)))?;
                }

                Op::BangEqual => {
                    self.build_expr(lhs)?;

                    self.write(TASM::PUSH(Register(0)))?;

                    self.build_expr(rhs)?;

                    self.write(TASM::POP(Register(1)))?;

                    self.write(TASM::EQUAL(Register(0),Register(1)))?;
                    self.write(TASM::NOT)?;
                }

                Op::LessThan => {
                    self.build_expr(lhs)?;

                    self.write(TASM::PUSH(Register(0)))?;

                    self.build_expr(rhs)?;

                    self.write(TASM::POP(Register(1)))?;

                    self.write(TASM::LESS(Register(0),Register(1)))?;
                }

                Op::LessThanEqual => {
                    self.build_expr(lhs)?;

                    self.write(TASM::PUSH(Register(0)))?;

                    self.build_expr(rhs)?;

                    self.write(TASM::POP(Register(1)))?;

                    self.write(TASM::GREATER(Register(0),Register(1)))?;

                     self.write(TASM::NOT)?;
                }

                Op::GreaterThan => {
                    self.build_expr(lhs)?;

                    self.write(TASM::PUSH(Register(0)))?;

                    self.build_expr(rhs)?;

                    self.write(TASM::POP(Register(1)))?;

                    self.write(TASM::GREATER(Register(0),Register(1)))?;
                }

                Op::GreaterThanEqual => {
                    self.build_expr(lhs)?;

                    self.write(TASM::PUSH(Register(0)))?;

                    self.build_expr(rhs)?;

                    self.write(TASM::POP(Register(1)))?;

                    self.write(TASM::LESS(Register(0),Register(1)))?;

                    self.write(TASM::NOT)?;
                }

                Op::And => {
                    self.build_expr(lhs)?;

                    self.write(TASM::EQUAL(Register(0),Register(1)))?;
                    

                    let label = Label::new();

                    self.write(TASM::JMPEQ(label))?;



                    self.build_expr(rhs)?;

                    self.write(TASM::LABEL(label))?;
                }

                Op::Or => {
                    self.build_expr(lhs)?;

                    self.write(TASM::EQUAL(Register(0),Register(1)))?;

                    let label = Label::new();

                    self.write(TASM::JMPNEQ(label))?;

                    self.build_expr(rhs)?;

                   self.write(TASM::LABEL(label))?;;
                }
            },

            Expression::Unary(ref op, ref expr) => {
                self.build_expr(expr)?;
                self.write(TASM::PUSH(Register(0)))?;

                match op {
                    UnaryOp::Minus => {
                        // Negation is desugared into 0-$VAL;
                        self.write(TASM::LOAD(Register(0),0))?;
                        self.write(TASM::POP(Register(1)))?;
                        self.write(TASM::SUB(Register(1),Register(0),Register(0) ))?;
                    }
                    UnaryOp::Bang => {
                        // TODO WOKR ON IMPLEMENT THIS
                        write!(&mut self.file, "NOT $0\n")?;
                    }
                }
            }

            _ => unimplemented!(),
        }

        Ok(())
    }
}


fn repeat_string(s: &str, count: usize) -> String {
    repeat(s).take(count).collect()
}