use super::{Function, Program};
use crate::native;
use crate::object::{
    ArrayObject, EnumObject, FunctionObject, InstanceObject, NativeObject, RawObject, StringObject,
};
use crate::opcode;
use crate::value::Value;
use fnv::FnvHashMap;
use util::symbol::Symbol;
#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;
/// The max size of the stack
const STACK_MAX: usize = 256;

#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

#[cfg(target_arch = "wasm32")]
macro_rules! log {
    ($($t:tt)*) =>( log(&format!($($t)*)));
}
#[derive(Debug)]
pub struct StackFrame<'a> {
    ip: usize,
    locals: FnvHashMap<u8, Value>,
    function: &'a Function,
    params: FnvHashMap<u8, Value>,
}

pub struct VM<'a> {
    stack: [Value; STACK_MAX],
    frames: Vec<StackFrame<'a>>,
    current_frame: StackFrame<'a>,
    native_functions: FnvHashMap<Symbol, Value>,
    program: &'a Program,
    objects: RawObject,
    stack_top: usize,
}

#[derive(Debug)]
pub enum Error {
    NoMain,
    UnknownOpcode,
}

impl<'a> VM<'a> {
    pub fn new(main: Symbol, program: &'a Program, objects: RawObject) -> Result<Self, Error> {
        let main_function = program.functions.get(&main);

        if main_function.is_none() {
            return Err(Error::NoMain);
        }

        let current_frame = StackFrame {
            ip: 0,
            locals: FnvHashMap::default(),
            function: main_function.unwrap(),
            params: FnvHashMap::default(),
        };

        let mut native_functions = FnvHashMap::default();
        native_functions.insert(
            Symbol(1),
            Value::object(NativeObject::new(2, native::random, objects)),
        );

        native_functions.insert(
            Symbol(2),
            Value::object(NativeObject::new(0, native::clock, objects)),
        );

        native_functions.insert(
            Symbol(3),
            Value::object(NativeObject::new(0, native::read, objects)),
        );

        native_functions.insert(
            Symbol(4),
            Value::object(NativeObject::new(1, native::fopen, objects)),
        );

        Ok(VM {
            stack: [Value::nil(); STACK_MAX],
            current_frame,
            program,
            frames: Vec::new(),
            stack_top: 4,
            native_functions,
            objects,
        })
    }

    pub fn run(&mut self) {
        #[cfg(feature = "debug")]
        {
            for (_, func) in self.program.functions.iter() {
                func.body.disassemble(&format!("{}", func.name))
            }

            for (_, class) in self.program.classes.iter() {
                let _: Vec<()> = class
                    .methods
                    .iter()
                    .map(|(_, func)| func.body.disassemble(&format!("{}", func.name)))
                    .collect();
            }
        }

        loop {
            if self.current_frame.ip >= self.current_frame.function.body.code.len() {
                return;
            }

            #[cfg(feature = "stack")]
            {
                println!("[");

                for val in &self.stack[0..self.stack_top] {
                    print!("{},", val);
                }

                println!("]")
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
                }

                opcode::CONSTANT => {
                    let constant = self.read_constant();
                    self.push(constant);
                }

                opcode::PRINT => {
                    let value = self.pop();

                    if cfg!(target_arch = "wasm32") {
                        log!("{}", value);
                    } else {
                        println!("{}", value);
                    }
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
                opcode::INT2FLOAT => {
                    let value = self.pop().as_int();
                    self.push(Value::float(value as f64))
                }
                opcode::FLOAT2INT => {
                    let value = self.pop().as_float();
                    self.push(Value::int(value as i64))
                }
                opcode::BOOL2INT => {
                    let value = self.pop().as_bool();
                    self.push(Value::int(value as i64))
                }

                opcode::FLOAT2STR => {
                    let value = self.pop().as_float();
                    let value = format!("{}", value);
                    self.push(Value::object(StringObject::from_owned(value, self.objects)));
                }

                opcode::INT2STR => {
                    let value = self.pop().as_int();
                    let value = format!("{}", value);
                    self.push(Value::object(StringObject::from_owned(value, self.objects)));
                }

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

                opcode::GETPROPERTY => {
                    let instance = self.pop();
                    let instance = instance.as_instance();

                    let property = Symbol(u64::from(self.read_byte()));
                    let value = instance.properties[&property];

                    self.push(value);
                }

                opcode::GETMETHOD => {
                    let instance = self.pop();
                    let instance = instance.as_instance();

                    let method_name = Symbol(u64::from(self.read_byte()));

                    let method = &instance.methods[&method_name];
                    let value = Value::object(FunctionObject::new(
                        method.params.len(),
                        method.clone(),
                        self.objects,
                    ));

                    self.push(value)
                }

                opcode::ENUM => {
                    let enum_name = Symbol(u64::from(self.read_byte()));
                    let tag = u32::from(self.read_byte());
                    let object = EnumObject::new(enum_name, tag, None, self.objects);
                    self.push(Value::object(object))
                }

                opcode::ENUMDATA => {
                    let enum_name = Symbol(u64::from(self.read_byte()));
                    let tag = u32::from(self.read_byte());
                    let data = self.pop();
                    let object = EnumObject::new(enum_name, tag, Some(data), self.objects);
                    self.push(Value::object(object))
                }

                opcode::SETPROPERTY => {
                    let instance = self.pop();
                    let instance = instance.as_mut_instance();

                    let value = self.pop();

                    let property = Symbol(u64::from(self.read_byte()));

                    instance.properties.insert(property, value);
                }

                opcode::CALLCLOSURE => {
                    let arg_count = self.read_byte();

                    let mut params = FnvHashMap::default();

                    for i in 0..arg_count {
                        params.insert(i, self.pop());
                    }

                    let closure = self.pop();

                    let closure = &closure.as_function().function;

                    let call_frame = StackFrame {
                        ip: 0,
                        locals: FnvHashMap::default(),
                        function: closure,
                        params,
                    };

                    self.frames
                        .push(::std::mem::replace(&mut self.current_frame, call_frame));
                }

                opcode::CALL => {
                    let function_name = Symbol(u64::from(self.read_byte()));
                    let arg_count = self.read_byte();

                    let function = &self.program.functions[&function_name];

                    let mut params = FnvHashMap::default();

                    for i in 0..arg_count {
                        params.insert(i, self.pop());
                    }

                    let call_frame = StackFrame {
                        ip: 0,
                        locals: FnvHashMap::default(),
                        function,
                        params,
                    };

                    self.frames
                        .push(::std::mem::replace(&mut self.current_frame, call_frame));
                    // swaps the current frame with the one we are one and then
                }

                opcode::CALLNATIVE => {
                    let function_name = Symbol(u64::from(self.read_byte()));
                    let function = &self.native_functions[&function_name];
                    let function = function.as_native();

                    let arg_count = function.arity;
                    let result = (function.function)(
                        self.stack[self.stack_top - arg_count as usize..self.stack_top].as_ptr(),
                    );

                    self.stack_top -= arg_count as usize;
                    {
                        self.push(result);
                    }
                }

                opcode::CALLINSTANCEMETHOD => {
                    let method_name = Symbol(u64::from(self.read_byte()));
                    let arg_count = self.read_byte();

                    let instance = self.pop();
                    let instance = instance.as_instance();

                    let function = &instance.methods[&method_name];

                    let mut params = FnvHashMap::default();

                    for i in 0..arg_count {
                        params.insert(i, self.pop());
                    }

                    let call_frame = StackFrame {
                        ip: 0,
                        locals: FnvHashMap::default(),
                        function,
                        params,
                    };

                    self.frames
                        .push(::std::mem::replace(&mut self.current_frame, call_frame));
                }

                opcode::CALLSTATICMETHOD => {
                    let class_name = Symbol(u64::from(self.read_byte()));
                    let method_name = Symbol(u64::from(self.read_byte()));
                    let arg_count = self.read_byte();
                    let function = &self.program.classes[&class_name].methods[&method_name];

                    let mut params = FnvHashMap::default();

                    for i in 0..arg_count {
                        params.insert(i, self.pop());
                    }

                    let call_frame = StackFrame {
                        ip: 0,
                        locals: FnvHashMap::default(),
                        function,
                        params,
                    };

                    self.frames
                        .push(::std::mem::replace(&mut self.current_frame, call_frame));
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

                    let slice = &string.chars.string()[index..=index];
                    let result = StringObject::new(slice, self.objects);

                    self.push(Value::object(result))
                }

                opcode::CLASSINSTANCE => {
                    let class_name = Symbol(u64::from(self.read_byte()));

                    let num_properties = self.read_byte() as usize;

                    let class = &self.program.classes[&class_name];

                    let methods = class.methods.clone();

                    let mut properties = FnvHashMap::default();

                    for _ in 0..num_properties {
                        properties.insert(Symbol(u64::from(self.read_byte())), self.pop());
                    }

                    let instance = InstanceObject::new(methods, properties, self.objects);

                    self.push(Value::object(instance));
                }

                opcode::CONCAT => self.concat(),

                #[cfg(not(feature = "debug"))]
                _ => unsafe {
                    use std::hint::unreachable_unchecked;
                    unreachable_unchecked()
                },
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
        let result = (u16::from(self.current_frame.function.body.code[self.current_frame.ip]) << 8)
            | u16::from(self.current_frame.function.body.code[self.current_frame.ip + 1]);
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
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut debug_trait_builder = f.debug_struct("VM");

        let _ = debug_trait_builder.field("stack", &self.stack[0..].iter());
        let _ = debug_trait_builder.field("ip", &self.current_frame.ip);
        debug_trait_builder.finish()
    }
}
