use super::Function;
use chunk::Chunk;
use opcode;
use std::mem;
use util::symbol::Symbol;
use value::Value;
use std::collections::HashMap;
/// The max size of the stack
const STACK_MAX: usize = 256;

pub struct StackFrame<'a> {
    ip: usize,
    locals:HashMap<u8,Value>,
    function: &'a Function,
}
pub struct VM<'a> {
    stack: [Value; STACK_MAX],
    frames: Vec<StackFrame<'a>>,
    current_frame: StackFrame<'a>,
    functions: &'a [Function],
    heap: Vec<u8>,
    equal_flag: bool,
    stack_top: usize,
}

#[derive(Debug)]
pub enum Error {
    NoMain,
    UnknownOpcode
}

impl<'a> VM<'a> {
    pub fn new(main: Symbol, functions: &'a [Function]) -> Result<Self, Error> {
        let mut main_funciton = None;

        {
            for func in functions.iter() {
                if func.name == main {
                    main_funciton = Some(func);
                }
            }
        }

        if main_funciton.is_none() {
            return Err(Error::NoMain);
        }

        let current_frame = StackFrame {
            ip: 0,
            locals: HashMap::new(),
            function: main_funciton.unwrap(),
        };

        Ok(VM {
            stack: [Value::nil(); STACK_MAX],
            current_frame,
            functions,
            frames: Vec::new(),
            equal_flag: false,
            heap: Vec::new(),
            stack_top: 4,
        })
    }

    pub fn run(&mut self) {

        #[cfg(feature="debug")]
        {
            for func in self.functions {
                func.body.disassemble("DEBUG")
            }
        }

        // return;

        loop {
            
                if self.current_frame.ip >= self.current_frame.function.body.code.len() {
                    return;
                }

                #[cfg(feature="stack")]
                {
                    print!("[");
                    
                    for val in &self.stack[0..self.stack_top] {
                        print!("{},",val);
                    }

                    print!("]\n")
                }
            

            match self.read_byte() {
                opcode::HLT => {
                    break;
                }

                opcode::RETURN => {
                    let value = self.pop();

                    match self.frames.pop() {
                        Some(frame) => {
                            self.current_frame = frame;
                            self.push(value);
                        }

                        None => {
                            continue; // Were are return from a top level function main
                        }
                    }

                    // println!("{}", value);
                }

                opcode::CONSTANT => {
                    let constant = self.read_constant();
                    self.push(constant);
                }

                opcode::PRINT => {
                    let value = self.pop();

                    println!("{}", value);
                }

                opcode::NEGATE => {
                    let val = Value::int(-self.pop().as_int());
                    self.push(val)
                }

                opcode::NEGATEF => {
                    let val = Value::float(-self.pop().as_float());
                    self.push(val)
                }

                opcode::NIL => self.push(Value::nil()),
                opcode::TRUE => self.push(Value::bool(true)),
                opcode::FALSE => self.push(Value::bool(false)),
                opcode::NOT => {
                    let val = Value::bool(!self.pop().as_bool());
                    self.push(val)
                }
                opcode::EQUAL => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(Value::bool(a == b));
                }

                opcode::LESS => binary_op!(<,as_int,bool,self),
                opcode::LESSF => binary_op!(<,as_float,bool,self),
                opcode::GREATER => binary_op!(>,as_int,bool,self),
                opcode::GREATERF => binary_op!(>,as_float,bool,self),
                opcode::ADD => binary_op!(+,as_int,int,self),
                opcode::ADDF => binary_op!(+,as_float,float,self),
                opcode::SUB => binary_op!(-,as_int,int,self),
                opcode::SUBF => binary_op!(-,as_float,float,self),
                opcode::MUL => binary_op!(*,as_int,int,self),
                opcode::MULF => binary_op!(*,as_float,float,self),
                opcode::DIV => binary_op!(/,as_int,int,self),
                opcode::DIVF => binary_op!(/,as_float,float,self),
                opcode::LOOP => {
                    let address = self.read_16_bits();

                    self.current_frame.ip = address as usize
                }
                opcode::JUMP => {
                    let address = self.read_16_bits();
                    self.current_frame.ip += address as usize;
                },

                opcode::JUMPIF => {
                    let address = self.read_16_bits();

                     if self.stack[self.stack_top-1].as_bool() {
                        self.current_frame.ip += address as usize;
                    }
                }
                opcode::JUMPNOT => {
                    let address = self.read_16_bits();

                    if !self.stack[self.stack_top-1].as_bool() {
                        self.current_frame.ip += address as usize;
                    }

                }
                opcode::GETLOCAL => {
                    let addres = self.read_byte();
                    
                    let val = self.current_frame.locals[&addres];

                    self.push(val);
                },

                opcode::SETLOCAL => {
                    let ident = self.read_byte();

                    let val = self.stack[self.stack_top-1]; // do it manually because don't modify the stack

                    self.current_frame.locals.insert(ident,val);

                },

                opcode::CALL => {
                    let symbol = self.read_byte();
                    let symbol = Symbol(symbol as u64);

                    
            
                    let mut function = None;

                    {
                        for func in self.functions.iter() {
                            if func.name == symbol {
                                function = Some(func);
                            }
                        }
                    }

                    let call_frame = StackFrame {
                        ip: 0,
                        locals: HashMap::new(),
                        function: function.unwrap(),
                    };

                    self.frames.push(::std::mem::replace(&mut self.current_frame, call_frame)); 
                    // swaps the current frame with the one we are one and then 


                }


                opcode::POP => {
                    self.pop();
                }

                

                #[cfg(not(feature="debug"))] 
                _ => {
                    panic!("Unknown opcode found");
                },
                #[cfg(feature="debug")] 
                ref e => {
                    
                    {
                        println!("unknown opcode found :{}",e);
                    }
                    continue;
                }
            }
        }
    }

    fn read_constant(&mut self) -> Value {
        let index = self.read_byte() as usize;
        self.current_frame.function.body.constants[index]
    }

    fn read_16_bits(&mut self) -> u16 {
        let result = ((self.current_frame.function.body.code[self.current_frame.ip] as u16) << 8) | self.current_frame.function.body.code[self.current_frame.ip + 1] as u16;
        // Shifts the instruction by 8 to the right and or all the 1's and 0's
        self.current_frame.ip += 2;

        result
    }

    fn read_byte(&mut self) -> u8 {
        let byte = self.current_frame.function.body.code[self.current_frame.ip];
        self.current_frame.ip += 1;
        byte
    }

    fn push(&mut self, val: Value) {
        self.stack[self.stack_top] = val;
        self.stack_top += 1;
    }

    fn pop(&mut self) -> Value {
        self.stack_top -= 1;
        self.stack[self.stack_top]
    }
}



use std::fmt::{self, Debug};

impl<'a> Debug for VM<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut debug_trait_builder = f.debug_struct("VM");

        let _ = debug_trait_builder.field("stack", &self.stack[0..].iter());
        let _ = debug_trait_builder.field("heap", &self.heap);
        let _ = debug_trait_builder.field("ip", &self.current_frame.ip);

        let _ = debug_trait_builder.field("equal_flag", &self.equal_flag);
        debug_trait_builder.finish()

        //  f.debug_list().entries(self.stack[0..].iter()).finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use opcode;

}