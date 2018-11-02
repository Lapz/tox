mod gen_tasm;
mod label;
mod tasm;

use ast;
use infer::types::Type;
use opcode;
use util::emmiter::Reporter;
use util::pos::{Span, Spanned};
use util::symbol::Symbol;
use std::collections::HashMap;
use vm::{Chunk, Function, Value};
type ParseResult<T> = Result<T, ()>;

#[derive(Debug,Clone,Copy)]
struct LoopDescription {
    /// The index of the start label
    start: i32,
    /// The index of the end label
    end: i32,
}
pub struct Builder<'a> {
    /// The current chunk
    chunk: Chunk,
    /// All local variables
    locals: HashMap<Symbol, u8>,

    current_loop: Option<LoopDescription>,
    reporter: &'a mut Reporter,
    line: u32,
}

impl<'a> Builder<'a> {
    pub fn new(reporter: &'a mut Reporter) -> Self {
        Builder {
            chunk: Chunk::new(),
            locals: HashMap::new(),
            line: 0,
            current_loop: None,
            reporter,
        }
    }

    pub fn emit_byte(&mut self, byte: u8) {
        self.chunk.write(byte, self.line)
    }

    pub fn emit_bytes(&mut self, byte1: u8, byte2: u8) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    pub fn emit_constant(&mut self, constant: Value, span: Span) -> ParseResult<()> {
        let value = self.make_constant(constant, span)?;
        self.emit_bytes(opcode::CONSTANT, value);
        Ok(())
    }

    pub fn make_constant(&mut self, value: Value, span: Span) -> ParseResult<u8> {
        let index = self.chunk.add_constant(value);

        if index > 256 {
            self.reporter.error("too many constants in one chunk", span);
            Err(())
        } else {
            Ok(index as u8)
        }
    }

    pub fn set_span(&mut self, span: Span) {
        if span.start.line > self.line {
            self.line = span.start.line
        }
    }

    pub fn compile_statement(&mut self, statement: &Spanned<ast::Statement>) -> ParseResult<()> {
        use ast::Statement;
        self.set_span(statement.span);
        match statement.value {
            Statement::Block(ref statements) => {
                for statement in statements {
                    self.compile_statement(statement)?;
                }

                Ok(())
            }

            Statement::Break => {
                let description = self.current_loop.expect("Using break outside a loop");

                self.emit_bytes(opcode::JUMP, description.end as u8);

                Ok(())
            }

            Statement::Continue => {
                let description = self.current_loop.expect("Using break outside a loop");

                self.emit_bytes(opcode::JUMP, description.start as u8);
                Ok(())
            },


            Statement::Expr(ref expr) => {
                self.compile_expression(expr)?;
                Ok(())
            },

            Statement::Print(ref expr) => {
                self.compile_expression(expr)?;

                self.emit_byte(opcode::PRINT);

                Ok(())
            }
            
            Statement::Return(ref expr) => {
                self.compile_expression(expr)?;

                self.emit_byte(opcode::RETURN);

                Ok(())
            }

            // Statement::If {ref cond,ref other,..}=> {
                
            // },

            Statement::Var { ref ident,ref ty,ref expr} => {
                
                if let Some(ref expr) = *expr {
                    self.compile_expression(expr)?;
                }else {
                    self.emit_constant(Value::nil(), statement.span)?;
                }
                println!("{}",self.chunk.code.len()-1);
                let pos = (self.chunk.code.len()-1) as u8;// The position at which the variable is stored in the code
            
                self.locals.insert(*ident, pos);
                
                Ok(())
            },



            // Statement::While(ref cond,ref body) => {

            //     self.compile_expression(cond)?;

            //     let start_index = self.chunk.code.len() -1;


            //     Ok(())
            // }

            _ => unimplemented!(),
        }
    }

    pub fn compile_expression(&mut self, expr: &Spanned<ast::TypedExpression>) -> ParseResult<()> {
        use ast::{Expression, Literal, Op};
        self.set_span(expr.span);

        match expr.value.expr.value {
            Expression::Literal(ref literal) => match *literal {
                Literal::False(_) => {
                    self.emit_byte(opcode::FALSE);
                }
                Literal::True(_) => {
                    self.emit_byte(opcode::TRUE);
                }
                Literal::Nil => {
                    self.emit_byte(opcode::NIL);
                }
                Literal::Int(ref n) => {
                    self.emit_constant(Value::int(*n), expr.value.expr.span)?;
                }
                Literal::Float(ref f) => {
                    self.emit_constant(Value::float(*f), expr.value.expr.span)?;
                }
                Literal::Str(ref bytes) => {
                    unimplemented!("String are not implemented")
                }
            },

            Expression::Binary(ref lhs, ref op, ref rhs) => {
                self.compile_expression(rhs)?;
                self.compile_expression(lhs)?;

                match (&expr.value.ty, op) {
                    (Type::Int, Op::Plus) => self.emit_byte(opcode::ADD),
                    (Type::Float, Op::Plus) => self.emit_byte(opcode::ADDF),

                    (Type::Int, Op::Minus) => self.emit_byte(opcode::SUB),
                    (Type::Float, Op::Minus) => self.emit_byte(opcode::SUBF),

                    (Type::Int, Op::Slash) => self.emit_byte(opcode::DIV),
                    (Type::Float, Op::Slash) => self.emit_byte(opcode::DIVF),

                    (Type::Int, Op::Star) => self.emit_byte(opcode::MUL),
                    (Type::Float, Op::Star) => self.emit_byte(opcode::MULF),

                    (Type::Int, Op::LessThan) => self.emit_byte(opcode::LESS),
                    (Type::Float, Op::LessThan) => self.emit_byte(opcode::LESSF),

                    (Type::Int, Op::LessThanEqual) => self.emit_bytes(opcode::LESS, opcode::NOT),
                    (Type::Float, Op::LessThanEqual) => self.emit_bytes(opcode::LESSF, opcode::NOT),

                    (Type::Int, Op::GreaterThan) => self.emit_byte(opcode::GREATER),
                    (Type::Float, Op::GreaterThan) => self.emit_byte(opcode::GREATERF),

                    (Type::Int, Op::GreaterThanEqual) => {
                        self.emit_bytes(opcode::GREATER, opcode::NOT)
                    }
                    (Type::Float, Op::GreaterThanEqual) => {
                        self.emit_bytes(opcode::GREATERF, opcode::NOT)
                    }

                    (_, Op::EqualEqual) => self.emit_byte(opcode::EQUAL),
                    (_, Op::BangEqual) => self.emit_bytes(opcode::EQUAL, opcode::NOT),

                    (ref ty, ref op) => unimplemented!(" ty {:?} op {:?}", ty, op),
                }
            }

            Expression::Grouping(ref expr) => {
                self.compile_expression(expr)?;
            }

            Expression::Ternary(ref cond, ref if_true, ref if_false) => {}

            Expression::Unary(ref op, ref expr) => {
                use ast::UnaryOp;

                self.compile_expression(expr)?;

                match *op {
                    UnaryOp::Bang => {
                        self.emit_byte(opcode::NOT);
                    }

                    UnaryOp::Minus => match &expr.value.ty {
                        Type::Int => self.emit_byte(opcode::NEGATE),
                        Type::Float => self.emit_byte(opcode::NEGATEF),
                        _ => unreachable!(),
                    },
                }
            },

            Expression::Var(ref ident,_) => {

                if let Some(pos) = self.locals.get(ident) {
                    self.emit_bytes(opcode::GETLOCAL, *pos);
                } else {
                    unimplemented!("Params ");
                }

            }

            ref e => unimplemented!("{:?}", e),
        }

        Ok(())
    }
}

fn compile_function(func: &ast::Function, reporter: &mut Reporter) -> ParseResult<Function> {
    let mut builder = Builder::new(reporter);

    builder.compile_statement(&func.body)?;

    Ok(Function {
        name: func.name,
        locals: builder.locals,
        body: builder.chunk,
    })
}

pub fn compile(ast: &ast::Program, reporter: &mut Reporter) -> ParseResult<Vec<Function>> {
    let mut funcs = Vec::new();

    for function in ast.functions.iter() {
        funcs.push(compile_function(function, reporter)?);
    }

    Ok(funcs)
}
