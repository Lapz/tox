use core::panic;
use salsa::{InternId, InternKey};
use semant::{
    hir::{Name, NameId, ParamId},
    Span,
};
use std::{collections::HashMap, u32, usize};

use crate::{
    chunks::Chunk,
    object::{ArrayObject, FunctionObject, InstanceObject, Object, ObjectType, StringObject},
    value::{self, Value, ValueType},
    CodegenDatabase,
};
use crate::{
    ir::{Function, Program},
    object::RawObject,
};

/// The max size of the stack
const STACK_MAX: usize = 256;
const FRAMES_MAX: usize = 64;

#[derive(Debug)]
pub struct StackFrame<'a> {
    ip: usize,
    locals: HashMap<u8, Value>,
    function: &'a FunctionObject,
    params: HashMap<u8, Value>,
    slots: usize,
}

pub struct VM<'a> {
    stack: [Value; STACK_MAX],
    frames: Vec<StackFrame<'a>>,
    current_frame: StackFrame<'a>,
    native_functions: HashMap<NameId, Value>,
    program: &'a Program,
    objects: RawObject,
    stack_top: usize,
    frame_count: usize,
}

impl<'a> VM<'a> {
    pub fn new(
        db: &impl CodegenDatabase,
        main: Option<usize>,
        program: &'a Program,
        objects: RawObject,
    ) -> Option<Self> {
        if let Some(main) = main {
            let main_function = program.functions.get(&main).cloned();

            match main_function {
                Some(function) => {
                    let object = unsafe { &*(function as *const FunctionObject) };
                    let current_frame = StackFrame {
                        ip: 0,
                        locals: HashMap::default(),
                        function: object,
                        params: HashMap::default(),
                        slots: 0,
                    };

                    let vm = VM {
                        stack: [Value::nil(); STACK_MAX],
                        program,
                        frames: Vec::new(),
                        stack_top: 4,
                        native_functions: HashMap::new(),
                        frame_count: 0,
                        current_frame,
                        objects,
                    };

                    Some(vm)
                }
                None => None,
            }
        } else {
            None
        }
    }

    pub fn run(&mut self) {
        loop {
            if self.current_frame.ip >= self.current_frame.function.function.body.code.len() {
                return;
            }

            // println!(
            //     "{}",
            //     self.current_frame.function.function.body.code[self.current_frame.ip]
            // );
            match self.read_byte() {
                opcode::HLT => {
                    break;
                }

                opcode::RETURN => {
                    let value = self.pop();

                    match self.frames.pop() {
                        Some(mut frame) => {
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
                    let len = self.read_byte();

                    for i in 0..len {
                        println!("{}", self.pop());
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

                opcode::ARRAY | opcode::TUPLE => {
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

                // opcode::GETPROPERTY => {
                //     let instance = self.pop();
                //     let instance = instance.as_instance();

                //     let property = Symbol(u64::from(self.read_byte()));
                //     let value = instance.properties[&property];

                //     self.push(value);
                // }

                // opcode::GETMETHOD => {
                //     let instance = self.pop();
                //     let instance = instance.as_instance();

                //     let method_name = Symbol(u64::from(self.read_byte()));

                //     let method = &instance.methods[&method_name];
                //     let value = Value::object(FunctionObject::new(
                //         method.params.len(),
                //         method.clone(),
                //         self.objects,
                //     ));

                //     self.push(value)
                // }
                // opcode::ENUM => {
                //     let enum_name = Symbol(u64::from(self.read_byte()));
                //     let tag = u32::from(self.read_byte());
                //     let object = EnumObject::new(enum_name, tag, None, self.objects);
                //     self.push(Value::object(object))
                // }

                // opcode::ENUMDATA => {
                //     let enum_name = Symbol(u64::from(self.read_byte()));
                //     let tag = u32::from(self.read_byte());
                //     let data = self.pop();
                //     let object = EnumObject::new(enum_name, tag, Some(data), self.objects);
                //     self.push(Value::object(object))
                // }

                // opcode::SETPROPERTY => {
                //     let instance = self.pop();
                //     let instance = instance.as_mut_instance();

                //     let value = self.pop();

                //     let property = Symbol(u64::from(self.read_byte()));

                //     instance.properties.insert(property, value);
                // }
                opcode::CALLCLOSURE => {
                    // let arg_count = self.read_byte();

                    // let mut params = HashMap::default();

                    // for i in 0..arg_count {
                    //     params.insert(i, self.pop());
                    // }

                    // let closure = self.pop();

                    // let closure = &closure.as_function().function;

                    // let call_frame = StackFrame {
                    //     ip: 0,
                    //     locals: HashMap::default(),
                    //     function: closure,
                    //     params,
                    // };

                    // self.frames
                    //     .push(::std::mem::replace(&mut current_frame, call_frame));
                }

                opcode::CALL => {
                    let arg_count = self.read_byte();

                    let function = self.pop().as_function();

                    self.call(function, arg_count as usize);
                }
                opcode::CALLNATIVE => {
                    let function_name =
                        NameId::from_intern_id(InternId::from(u32::from(self.read_byte())));
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

                // opcode::CALLINSTANCEMETHOD => {
                //     let method_name = Symbol(u64::from(self.read_byte()));
                //     let arg_count = self.read_byte();

                //     let instance = self.pop();
                //     let instance = instance.as_instance();

                //     let function = &instance.methods[&method_name];

                //     let mut params = HashMap::default();

                //     for i in 0..arg_count {
                //         params.insert(i, self.pop());
                //     }

                //     let call_frame = StackFrame {
                //         ip: 0,
                //         locals: HashMap::default(),
                //         function,
                //         params,
                //     };

                //     self.frames
                //         .push(::std::mem::replace(&mut current_frame, call_frame));
                // }

                // opcode::CALLSTATICMETHOD => {
                //     let class_name = Symbol(u64::from(self.read_byte()));
                //     let method_name = Symbol(u64::from(self.read_byte()));
                //     let arg_count = self.read_byte();
                //     let function = &self.program.classes[&class_name].methods[&method_name];

                //     let mut params = HashMap::default();

                //     for i in 0..arg_count {
                //         params.insert(i, self.pop());
                //     }

                //     let call_frame = StackFrame {
                //         ip: 0,
                //         locals: HashMap::default(),
                //         function,
                //         params,
                //     };

                //     self.frames
                //         .push(::std::mem::replace(&mut current_frame, call_frame));
                // }
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
                    let class_name =
                        NameId::from_intern_id(InternId::from(u32::from(self.read_byte())));

                    let num_properties = self.read_byte() as usize;

                    let class = &self.program.classes[&class_name];

                    let methods = class.methods.clone();

                    let mut properties = HashMap::default();

                    for _ in 0..num_properties {
                        properties.insert(
                            NameId::from_intern_id(InternId::from(u32::from(self.read_byte()))),
                            self.pop(),
                        );
                    }

                    let instance = InstanceObject::new(methods, properties, self.objects);

                    self.push(Value::object(instance));
                }
                opcode::CONCAT => self.concat(),

                opcode::IGL => {
                    panic!("Illegal instruction encountered")
                }

                opcode::GET_FUNCTION => {
                    let pos = self.read_byte();

                    let function = self.program.functions[&(pos as usize)];

                    self.push(Value::object(function))
                }

                opcode => {
                    println!("Unknown opcode ${:?}", opcode);
                    self.read_byte();
                    continue;
                }
            }
        }
    }

    fn call(&mut self, function: &'a FunctionObject, arg_count: usize) {
        let mut params = HashMap::default();

        for i in 0..self.stack_top - arg_count - 1 {
            params.insert(i as u8, self.pop());
        }

        let call_frame = StackFrame {
            ip: 0,
            locals: HashMap::default(),
            function,
            params,
            slots: self.stack_top - arg_count - 1,
        };

        self.frames
            .push(::std::mem::replace(&mut self.current_frame, call_frame));

        println!("{:?}", self.frames)
    }

    fn call_value(&mut self, value: Value, arg_count: usize) {
        if value.is_object() {
            self.call(value.as_function(), arg_count)
        } else {
            // Todo intergrate errors
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

        let result = StringObject::from_owned(new, self.objects);

        self.push(Value::object(result));
    }

    fn read_constant(&mut self) -> Value {
        let index = self.read_byte() as usize;
        self.current_frame.function.function.body.constants[index]
    }

    fn read_16_bits(&mut self) -> u16 {
        let result =
            (u16::from(self.current_frame.function.function.body.code[self.current_frame.ip]) << 8)
                | u16::from(
                    self.current_frame.function.function.body.code[self.current_frame.ip + 1],
                );
        // Shifts the instruction by 8 to the right and or all the 1's and 0's
        self.current_frame.ip += 2;

        result
    }

    fn read_byte(&mut self) -> u8 {
        let byte = self.current_frame.function.function.body.code[self.current_frame.ip];
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
