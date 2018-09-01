use std::fs::File;
use std::io::{self,Write,Read};
use syntax::ast::{statement::Statement,expr::{Expression,Literal,Op}};

use label::Label;
use vm::Assembler;
use util::pos::Spanned;

#[derive(Debug)]
pub struct Compiler {
    file:File
}



impl Compiler {
    pub fn new() -> Self {
        Compiler {
            file:File::create("output.tasm").expect("Couldn't create the file")
        }
    }

    pub fn compile(&mut self,ast:&[Spanned<Statement>]) -> io::Result<()> {
        for node in ast {
            self.build_statement(node)?;
        }

        Ok(())
    }

    pub fn assemble(&mut self) ->Option<Vec<u8> >{

        let mut assembler = Assembler::new();

        let mut contents = String::new();

        File::open("output.tasm").expect("Couldn't open the file").read_to_string(&mut contents).expect("Coudln't read to file");

        assembler.assemble(&contents)
        
    }


    fn build_statement(&mut self,statement:&Spanned<Statement>) -> io::Result<()> {

        match &statement.value {
            Statement::Block(ref block) => {
                let label = Label::new();
                write!(&mut self.file,"{}",label);

                for statement in block {
                    self.build_statement(statement)?;
                }

                Ok(())
            },


            Statement::Expr(ref expr) => {
                self.build_expr(expr)?;
                Ok(())
            }

            _ => unimplemented!()
        }
    }

    fn build_expr(&mut self,expr:&Spanned<Expression>) -> io::Result<()> {
        match &expr.value {
            Expression::Literal(ref literal) => {
                match literal {
                    Literal::Int(ref int) => {
                        write!(&mut self.file,"LOAD $0 #{}\n",int)?;
                        Ok(())
                    },
                    _ => unimplemented!()
                }
            },

            Expression::Binary {ref rhs,ref op,ref lhs} => {
                self.build_expr(lhs)?;

                write!(&mut self.file,"PUSH $0\n")?;

                self.build_expr(rhs)?;

                write!(&mut self.file,"POP $1\n")?;


                match op.value {
                    Op::Plus => {
                         write!(&mut self.file,"ADD $0 $1 $0\n")?;
                    },
                    Op::Minus => {
                          write!(&mut self.file,"SUB $0 $1 $0\n")?;
                    },
                    Op::Slash => {
                         write!(&mut self.file,"DIV $0 $1 $0\n")?;
                    },

                    Op::Star => {
                        write!(&mut self.file,"STAR $0 $1 $0\n")?;
                    }

                    _ => unimplemented!()
                }

              


               
                Ok(())

            }

            _ => unimplemented!()
        }
    }
}