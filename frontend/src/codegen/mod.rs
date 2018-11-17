use ast;
use infer::types::Type;
use opcode;
use std::collections::HashMap;
use util::emmiter::Reporter;
use util::pos::{Span, Spanned};
use util::symbol::Symbol;
use vm::{Chunk, Function, FunctionObject, RawObject, StringObject, Value,Class};
type ParseResult<T> = Result<T, ()>;

#[derive(Debug, Clone, Copy)]
struct LoopDescription {
    /// The index of the start label
    start: i32,
    /// The index of the end label
    end: i32,
}
pub struct Builder<'a> {
    /// The current chunk
    chunk: Chunk,
    /// A count of all local vars
    /// The number is the postion of the local on the local stack
    locals: HashMap<Symbol, usize>,

    params: HashMap<Symbol, usize>,
    current_loop: Option<LoopDescription>,
    ///  A linked list of all the objects allocated. This
    /// is passed to the vm so runtime collection can be done
    pub objects: RawObject,
    reporter: &'a mut Reporter,
    closures: Vec<Function>,
    line: u32,
}

impl<'a> Builder<'a> {
    pub fn new(
        reporter: &'a mut Reporter,
        objects: RawObject,
        params: HashMap<Symbol, usize>,
    ) -> Self {
        Builder {
            chunk: Chunk::new(),
            locals: HashMap::new(),
            line: 0,
            current_loop: None,
            params,
            objects,
            reporter,
            closures: Vec::new(),
        }
    }

    pub fn emit_byte(&mut self, byte: u8) {
        self.chunk.write(byte, self.line)
    }

    pub fn patch_jump(&mut self, offset: usize) {
        // -2 to adjust for the bytecode for the jump offset itself.
        let jump = self.chunk.code.len() - offset - 2;

        self.chunk.code[offset] = ((jump >> 8) & 0xff) as u8;
        self.chunk.code[offset + 1] = (jump & 0xff) as u8;
    }

    pub fn emit_jump(&mut self, byte: u8) -> usize {
        self.emit_byte(byte);
        self.emit_bytes(0xff, 0xff);
        self.chunk.code.len() - 2
    }

    pub fn emit_loop(&mut self, loop_start: usize) {
        self.emit_byte(opcode::LOOP);

        let offset = self.chunk.code.len() - loop_start + 2;

        self.emit_bytes(((offset >> 8) & 0xff) as u8, (offset & 0xff) as u8)
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

            Statement::If {
                ref cond,
                ref then,
                otherwise:None
            } => {

                self.compile_expression(cond)?;

                let false_label = self.emit_jump(opcode::JUMPNOT);

                self.emit_byte(opcode::POP);

                self.compile_statement(then)?;

                self.patch_jump(false_label);

                self.emit_byte(opcode::POP);

                Ok(())

            }

            Statement::If {ref cond,ref then,otherwise:Some(ref otherwise)} => {

                self.compile_expression(cond)?;

                let false_label = self.emit_jump(opcode::JUMPNOT);

                 self.emit_byte(opcode::POP);

                self.compile_statement(then)?;

                let end_label = self.emit_jump(opcode::JUMP);

                self.patch_jump(false_label);

                self.emit_byte(opcode::POP);

                self.compile_statement(otherwise)?;

                self.patch_jump(end_label);

                Ok(())
            },

            Statement::Var { ref ident,ref expr,..} => {

                //
                if let Some(ref expr) = *expr {
                    self.compile_expression(expr)?;
                }else {
                    self.emit_constant(Value::nil(), statement.span)?;
                } // Compile the expression

                self.locals.insert(*ident, ident.0 as usize);

                self.emit_bytes(opcode::SETLOCAL,ident.0 as u8); // Write the symbol id

                Ok(())
            },

            Statement::While(ref cond,ref body) => {
                let start_label = self.chunk.code.len();

                self.compile_expression(cond)?;

                let out = self.emit_jump(opcode::JUMPNOT);

                self.emit_byte(opcode::POP);

                self.compile_statement(body)?;

                self.emit_loop(start_label); // Jumps back to the start


                self.patch_jump(out); // the outer label

                self.emit_byte(opcode::POP); //removes cond from stack


                Ok(())
            }

            // _ => unimplemented!(),
        }
    }


   

    pub fn compile_expression(&mut self, expr: &Spanned<ast::TypedExpression>) -> ParseResult<()> {
        use ast::{AssignOperator, Expression, Literal, Op};
        self.set_span(expr.span);

        match expr.value.expr.value {
            Expression::Assign(ref ident, ref op, ref expr) => {
                let pos = if let Some(pos) = self.locals.get(ident) {
                    *pos
                } else {
                    unreachable!(); // Params are treated as locals so it should be present
                };

                match *op {
                    AssignOperator::Equal => {
                        self.compile_expression(expr)?;
                        self.emit_bytes(opcode::SETLOCAL, pos as u8);
                    }
                    AssignOperator::MinusEqual => {
                        self.emit_bytes(opcode::GETLOCAL, pos as u8); // get the var

                        let opcode = match expr.value.ty {
                            Type::Int => opcode::SUB,
                            Type::Float => opcode::SUBF,
                            _ => unreachable!(), // type checker should prevent this
                        };

                        self.compile_expression(expr)?; // get the expr

                        self.emit_byte(opcode);

                        self.emit_bytes(opcode::SETLOCAL, pos as u8); // store it in x
                    }

                    AssignOperator::PlusEqual => {
                        self.emit_bytes(opcode::GETLOCAL, pos as u8); // get the var

                        let opcode = match expr.value.ty {
                            Type::Int => opcode::ADD,
                            Type::Float => opcode::ADDF,
                            _ => unreachable!(), // type checker should prevent this
                        };

                        self.compile_expression(expr)?; // get the expr

                        self.emit_byte(opcode);

                        self.emit_bytes(opcode::SETLOCAL, pos as u8); // store it in x
                    }

                    AssignOperator::SlashEqual => {
                        self.emit_bytes(opcode::GETLOCAL, pos as u8); // get the var

                        let opcode = match expr.value.ty {
                            Type::Int => opcode::DIV,
                            Type::Float => opcode::DIVF,
                            _ => unreachable!(), // type checker should prevent this
                        };

                        self.compile_expression(expr)?; // get the expr

                        self.emit_byte(opcode);

                        self.emit_bytes(opcode::SETLOCAL, pos as u8); // store it in x
                    }

                    AssignOperator::StarEqual => {
                        self.emit_bytes(opcode::GETLOCAL, pos as u8); // get the var

                        let opcode = match expr.value.ty {
                            Type::Int => opcode::MUL,
                            Type::Float => opcode::MULF,
                            _ => unreachable!(), // type checker should prevent this
                        };

                        self.compile_expression(expr)?; // get the expr

                        self.emit_byte(opcode);

                        self.emit_bytes(opcode::SETLOCAL, pos as u8); // store it in x
                    }
                }
            }

            Expression::Array(ref exprs) => {
                for expr in exprs.iter().rev() {
                    // reverse because items how items are popped off the stack
                    self.compile_expression(expr)?;
                }

                self.emit_bytes(opcode::ARRAY, exprs.len() as u8);
            }

            Expression::Index(ref target, ref index) => {
                
                match expr.value.ty {
                    Type::Str => {
                        self.compile_expression(target)?;
                        self.compile_expression(index)?;

                        self.emit_byte(opcode::INDEXSTRING);
                    }
                    Type::Array(_) => {
                        self.compile_expression(target)?;
                        self.compile_expression(index)?;

                        self.emit_byte(opcode::INDEXARRAY);
                    },

                    _ => unimplemented!() // 
                }
            }

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
                Literal::Str(ref string) => {
                    let object = StringObject::new(string, self.objects);

                    self.emit_constant(Value::object(object), expr.value.expr.span)?;
                }
            },

            Expression::Binary(ref lhs, ref op, ref rhs) => {
                if *op == Op::And {
                    self.compile_and(lhs, rhs)?;
                } else if *op == Op::Or {
                    self.compile_or(lhs, rhs)?;
                } else {
                    self.compile_expression(lhs)?;
                    self.compile_expression(rhs)?;

                    match (&expr.value.ty, op) {
                        (Type::Int, Op::Plus) => self.emit_byte(opcode::ADD),
                        (Type::Float, Op::Plus) => self.emit_byte(opcode::ADDF),

                        (Type::Int, Op::Minus) => self.emit_byte(opcode::SUB),
                        (Type::Float, Op::Minus) => self.emit_byte(opcode::SUBF),

                        (Type::Int, Op::Slash) => self.emit_byte(opcode::DIV),
                        (Type::Float, Op::Slash) => self.emit_byte(opcode::DIVF),

                        (Type::Int, Op::Star) => self.emit_byte(opcode::MUL),
                        (Type::Float, Op::Star) => self.emit_byte(opcode::MULF),

                        // For comparisson the lhs and the rhs should be the same so only
                        // check the type of the lhs
                        (Type::Bool, Op::LessThan) => {
                            if lhs.value.ty == Type::Int {
                                self.emit_byte(opcode::LESS)
                            } else {
                                self.emit_byte(opcode::LESSF)
                            }
                        }

                        (Type::Bool, Op::LessThanEqual) => {
                            if lhs.value.ty == Type::Int {
                                self.emit_bytes(opcode::LESS, opcode::NOT)
                            } else {
                                self.emit_bytes(opcode::LESSF, opcode::NOT)
                            }
                        }

                        (Type::Bool, Op::GreaterThan) => {
                            if lhs.value.ty == Type::Int {
                                self.emit_byte(opcode::GREATER)
                            } else {
                                self.emit_byte(opcode::GREATERF)
                            }
                        }

                        (Type::Bool, Op::GreaterThanEqual) => {
                            if lhs.value.ty == Type::Int {
                                self.emit_bytes(opcode::GREATER, opcode::NOT)
                            } else {
                                self.emit_bytes(opcode::GREATERF, opcode::NOT)
                            }
                        }

                        (Type::Str, Op::Plus) => self.emit_byte(opcode::CONCAT),

                        (_, Op::EqualEqual) => self.emit_byte(opcode::EQUAL),
                        (_, Op::BangEqual) => self.emit_bytes(opcode::EQUAL, opcode::NOT),

                        // #[cfg(not(feature = "debug"))]
                        // _ => unsafe {
                        //     ::std::hint::unreachable_unchecked() // only in release mode for that extra speed boost
                        // },

                        // #[cfg(feature = "debug")]
                        (ref ty, ref op) => unimplemented!(" ty {:?} op {:?}", ty, op),
                    }
                }
            }

            Expression::Call(ref callee, ref args) => {
                if args.is_empty() {
                    self.emit_bytes(opcode::CALL, callee.0 as u8);
                    self.emit_byte(0);
                    return Ok(());
                }

                for arg in args {
                    self.compile_expression(arg)?;
                }

                match expr.value.ty {
                    Type::Fun(_, _, true) => {
                        self.emit_byte(opcode::CALLCLOSURE);
                        self.emit_byte(args.len() as u8);
                    }
                    _ => {
                        self.emit_bytes(opcode::CALL, callee.0 as u8);
                        self.emit_byte(args.len() as u8);
                    }
                }
            },

            Expression::ClassInstance(ref symbol,ref properties) => {
                
                self.emit_bytes(opcode::CLASSINSTANCE,symbol.0 as u8);
                self.emit_byte(properties.len() as u8);
                
                for (ident,expr) in properties.iter() { //rev because poped of stack            
                    self.compile_expression(expr)?;
                    self.emit_bytes(opcode::SETPROPERTY,ident.0 as u8);
                }
                
                
                

                
            }

            Expression::Grouping(ref expr) => {
                self.compile_expression(expr)?;
            }

            Expression::Ternary(ref cond, ref if_true, ref if_false) => {
                self.compile_expression(cond)?;

                let false_label = self.emit_jump(opcode::JUMPNOT);

                self.compile_expression(if_true)?;

                let end_label = self.emit_jump(opcode::JUMP);

                self.patch_jump(false_label);

                self.compile_expression(if_false)?;

                self.patch_jump(end_label);
            }

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
            }

            Expression::Var(ref ident, _) => {
                if let Some(pos) = self.locals.get(ident).cloned() {
                    self.emit_bytes(opcode::GETLOCAL, pos as u8);
                } else if let Some(offset) = self.params.get(ident).cloned() {
                    self.emit_bytes(opcode::GETPARAM, offset as u8);
                } else {
                    unreachable!(); // Params are treated as locals so it should be present
                }
            },
            
            Expression::Get(ref property,ref object) => {
                self.compile_expression(object)?;

                self.emit_byte(opcode::GETPROPERTY);
                self.emit_byte(property.0 as u8);
               
            },

            Expression::Closure(ref func) => {
                let closure = compile_function(func, self.reporter, self.objects)?;

                let func = FunctionObject::new(closure.params.len(), closure, self.objects);

                self.emit_constant(Value::object(func), expr.span)?;
            }

            ref e => unimplemented!("{:?}", e),
        }

        Ok(())
    }

    fn compile_and(
        &mut self,
        lhs: &Spanned<ast::TypedExpression>,
        rhs: &Spanned<ast::TypedExpression>,
    ) -> ParseResult<()> {
        self.compile_expression(lhs)?;

        let false_label = self.emit_jump(opcode::JUMPNOT);

        self.compile_expression(rhs)?;

        self.patch_jump(false_label);

        Ok(())
    }

    fn compile_or(
        &mut self,
        lhs: &Spanned<ast::TypedExpression>,
        rhs: &Spanned<ast::TypedExpression>,
    ) -> ParseResult<()> {
        self.compile_expression(lhs)?;

        let else_label = self.emit_jump(opcode::JUMPIF);

        self.compile_expression(rhs)?;

        self.patch_jump(else_label);

        // self.emit_byte(opcode::POP);

        Ok(())
    }

}


fn compile_class(class:&ast::Class,reporter:&mut Reporter,objects:RawObject) -> ParseResult<Class> {
    
    let mut methods = HashMap::new();

    for method in class.methods.iter() {
        methods.insert(method.name, compile_function(method,reporter, objects)?);
    }

    Ok(Class {
        name:class.name,
        methods
    })
}


fn compile_function(
    func: &ast::Function,
    reporter: &mut Reporter,
    objects: RawObject,
) -> ParseResult<Function> {
    let mut params = HashMap::new();

    for (i, param) in func.params.iter().enumerate() {
        params.insert(param.name, i);
    } // store param id and the index in the vec

    let mut builder = Builder::new(reporter, objects, params);

    builder.compile_statement(&func.body)?;

    Ok(Function {
        name: func.name,
        locals: builder.locals,
        body: builder.chunk,
        params: builder.params,
    })
}

pub fn compile(
    ast: &ast::Program,
    reporter: &mut Reporter,
) -> ParseResult<(Vec<Function>,Vec<Class>,RawObject)> {
    let mut funcs = Vec::new();
    let mut classes = Vec::new();

    let objects = ::std::ptr::null::<RawObject>() as RawObject;

    for function in ast.functions.iter() {
        funcs.push(compile_function(function, reporter, objects)?);
    }

    for class in ast.classes.iter() {
        classes.push(compile_class(class,reporter,objects)?);
    }

    Ok((funcs, classes,objects))
}
