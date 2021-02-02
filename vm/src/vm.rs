// use std::collections::HashMap;

// use semant::{
//     hir::{NameId, ParamId},
//     Span,
// };

// use crate::{chunks::Chunk, value::Value};
// use crate::{codegen::CodegenFunction, object::RawObject};

// /// The max size of the stack
// const STACK_MAX: usize = 256;

// #[derive(Debug)]
// pub struct StackFrame<'a> {
//     ip: usize,
//     locals: HashMap<u8, Value>,
//     function: &'a Function,
//     params: HashMap<u8, Value>,
// }

// #[derive(Debug, Clone, PartialEq)]
// pub struct Function {
//     pub name: Span<NameId>,
//     pub body: Chunk,
//     pub params: HashMap<ParamId, usize>,
// }

// #[derive(Debug, Clone)]
// pub struct Program {
//     pub functions: HashMap<NameId, Function>,
//     // pub classes: HashMap<::util::symbol::Symbol, Class>,
// }

// pub struct VM<'a, DB> {
//     db: DB,
//     stack: [Value; STACK_MAX],
//     frames: Vec<StackFrame<'a>>,
//     current_frame: StackFrame<'a>,
//     native_functions: HashMap<NameId, Value>,
//     program: &'a Program,
//     objects: RawObject,
//     stack_top: usize,
// }

// impl<'a> VM<'a> {
//     pub fn new(program: &'a Program, objects: RawObject) -> Self {
//         let main_function = program.functions.get(&main);
//         let current_frame = StackFrame {
//             ip: 0,
//             locals: HashMap::default(),
//             function: main_function.unwrap(),
//             params: HashMap::default(),
//         };

//         VM {
//             stack: [Value::nil(); STACK_MAX],
//             current_frame,
//             program,
//             frames: Vec::new(),
//             stack_top: 4,
//             native_functions: HashMap::new(),
//             objects,
//         }
//     }
// }
