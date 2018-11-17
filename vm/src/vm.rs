use super::{Function,Class};
use object::{ArrayObject, RawObject, StringObject,InstanceObject};
use opcode;
use std::collections::HashMap;
use util::symbol::Symbol;
use value::Value;
/// The max size of the stack
const STACK_MAX: usize = 256;

pub struct StackFrame<'a> {
    ip: usize,
    locals: HashMap<u8, Value>,
    function: &'a Function,
    params: HashMap<u8, Value>,
}
pub struct VM<'a> {
    stack: [Value; STACK_MAX],
    frames: Vec<StackFrame<'a>>,
    current_frame: StackFrame<'a>,
    functions: &'a [Function],
    classes: &'a [Class],
    objects: RawObject,
    heap: Vec<u8>,
    equal_flag: bool,
    stack_top: usize,
}

#[derive(Debug)]
pub enum Error {
    NoMain,
    UnknownOpcode,
}

impl<'a> VM<'a> {
    pub fn new(main: Symbol, functions: &'a [Function],classes:&'a [Class],objects: RawObject) -> Result<Self, Error> {
        let mut main_function = None;

        {
            for func in functions.iter() {
                if func.name == main {
                    main_function = Some(func);
                }
            }
        }

        if main_function.is_none() {
            return Err(Error::NoMain);
        }

        let current_frame = StackFrame {
            ip: 0,
            locals: HashMap::new(),
            function: main_function.unwrap(),
            params: HashMap::new(),
        };

        Ok(VM {
            stack: [Value::nil(); STACK_MAX],
            current_frame,
            functions,
            classes,
            frames: Vec::new(),
            equal_flag: false,
            heap: Vec::new(),
            stack_top: 4,
            objects,
        })
    }

    pub fn run(&mut self) {
        #[cfg(feature = "debug")]
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

            #[cfg(feature = "stack")]
            {
                print!("[");

                for val in &self.stack[0..self.stack_top] {
                    print!("{},", val);
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

                opcode::ARRAY => {
                    let len = self.read_byte();

                    let items: Vec<Value> = (0..len).map(|_| self.pop()).collect();

                    let array = ArrayObject::new(items, self.objects);

                    self.push(Value::object(array));
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

                    self.current_frame.ip -= address as usize
                }
                opcode::JUMP => {
                    let address = self.read_16_bits();
                    self.current_frame.ip += address as usize;
                }

                opcode::JUMPIF => {
                    let address = self.read_16_bits();

                    if self.stack[self.stack_top - 1].as_bool() {
                        self.current_frame.ip += address as usize;
                    }
                }
                opcode::JUMPNOT => {
                    let address = self.read_16_bits();

                    if !self.stack[self.stack_top - 1].as_bool() {
                        self.current_frame.ip += address as usize;
                    }
                }
                opcode::GETLOCAL => {
                    let local = self.read_byte();

                    let val = self.current_frame.locals[&local];

                    self.push(val);
                }

                opcode::SETLOCAL => {
                    let ident = self.read_byte();

                    let val = self.stack[self.stack_top - 1]; // do it manually because we don't  want to modify the stack

                    self.current_frame.locals.insert(ident, val);
                }

                opcode::GETPARAM => {
                    let param = self.read_byte();

                    let val = self.current_frame.params[&param];

                    self.push(val);
                }

                opcode::CALLCLOSURE => {
                    let arg_count = self.read_byte();

                    let mut params = HashMap::new();

                    for i in 0..arg_count {
                        params.insert(i, self.pop());
                    }

                    let closure = self.pop();

                    let closure = &closure.as_function().function;

                    let call_frame = StackFrame {
                        ip: 0,
                        locals: HashMap::new(),
                        function: closure,
                        params,
                    };

                    self.frames
                        .push(::std::mem::replace(&mut self.current_frame, call_frame));
                }

                opcode::CALL => {
                    let symbol = self.read_byte();
                    let arg_count = self.read_byte();

                    let symbol = Symbol(symbol as u64);

                    let mut function = None;

                    {
                        for func in self.functions.iter() {
                            if func.name == symbol {
                                function = Some(func);
                            }
                        }
                    }

                    let mut params = HashMap::new();

                    for i in 0..arg_count {
                        params.insert(i, self.pop());
                    }

                    let call_frame = StackFrame {
                        ip: 0,
                        locals: HashMap::new(),
                        function: function.unwrap(),
                        params,
                    };

                    self.frames
                        .push(::std::mem::replace(&mut self.current_frame, call_frame));
                    // swaps the current frame with the one we are one and then
                }

                opcode::POP => {
                    self.pop();
                }

                opcode::INDEXARRAY => {
                    let index = self.pop().as_int() as usize;

                    let array = self.pop();
                    let array = array.as_array();

                    self.push(array.items[index]);
                }

                opcode::INDEXSTRING => {
                    let index = self.pop().as_int() as usize;

                    let string = self.pop();
                    let string = string.as_string();

                    let slice = &string.chars.string()[index..index+1];
                    let result = StringObject::new(slice, self.objects);

                    self.push(Value::object(result))
                },

                opcode::CLASSINSTANCE => {
                    
                    let symbol = self.read_byte();
                    
                    let symbol = Symbol(symbol as u64);


                    let mut class = None;

                    {
                        for klass in self.classes.iter() {
                            if klass.name == symbol {
                                class = Some(klass);
                            }
                        }
                    }

                    let class = class.unwrap();

                    let methods = class.methods.clone();
                    let mut properties = HashMap::new();

                    // for _ in 0..num_properties {
                    //     properties.insert(Symbol(self.read_byte() as u64), self.pop());
                    // }

                    // println!("{:?}",properties);

                    let instance = InstanceObject::new(methods,properties,self.objects);

                    self.push(Value::object(instance));

                },

                opcode::SETPROPERTY => {
                    let property = Symbol(self.read_byte() as u64);
           
                    let instance = self.stack[self.stack_top-1];
                    let value = self.stack[self.stack_top];
                    
                    let mut instance = instance.as_mut_instance();

                    
                    instance.properties.insert(property, value);
                },
                opcode::CONCAT => self.concat(),

                #[cfg(not(feature = "debug"))]
                _ => {
                    panic!("Unknown opcode found");
                }
                #[cfg(feature = "debug")]
                ref e => {
                    {
                        println!("unknown opcode found :{}", e);
                    }
                    continue;
                }
            }
        }
    }

    fn concat(&mut self) {
        let b = self.pop();
        let b = b.as_string();
        let a = self.pop();
        let a = a.as_string();

        let length = a.chars.string().len() + b.chars.string().len();

        let mut new = String::with_capacity(length);

        new.push_str(a.chars.string());
        new.push_str(b.chars.string());

        #[cfg(feature = "debug")]
        {
            println!("{:?}", a.chars);
            println!("{:?}", b.chars);
        }

        let result = StringObject::from_owned(new, self.objects);

        self.push(Value::object(result));
    }

    fn read_constant(&mut self) -> Value {
        let index = self.read_byte() as usize;
        self.current_frame.function.body.constants[index]
    }

    fn read_16_bits(&mut self) -> u16 {
        let result = ((self.current_frame.function.body.code[self.current_frame.ip] as u16) << 8)
            | self.current_frame.function.body.code[self.current_frame.ip + 1] as u16;
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
