use crate::CodegenResult;

// use ast;
use fnv::FnvHashMap;
use std::collections::HashMap;
// use infer::types::{Type, TypeCon};
use ir::instructions::*;
use ir::types::{Type, TypeCon};
use opcode;
use std::hash::Hash;
use util::emmiter::Reporter;
use util::pos::{Span, Spanned};
use util::symbol::{Symbol, Symbols};
use vm::{self, Chunk, FunctionObject, RawObject, StringObject};

// #[derive(Debug, Clone, Copy)]
// struct LoopDescription {
//     /// The index of the start label
//     start: usize,
//     /// The index of the end label
//     end: usize,
// }

#[derive(Debug, Clone)]
struct StackedMap<K: Hash + Eq, V: Clone> {
    table: FnvHashMap<K, Vec<V>>,
    scopes: Vec<Option<K>>,
}

impl<K: Hash + Eq + Copy, V: Clone> StackedMap<K, V> {
    pub fn new() -> Self {
        StackedMap {
            table: FnvHashMap::default(),
            scopes: vec![],
        }
    }

    pub fn begin_scope(&mut self) {
        self.scopes.push(None);
    }

    pub fn end_scope(&mut self) {
        while let Some(Some(value)) = self.scopes.pop() {
            let mapping = self.table.get_mut(&value).expect("Symbol not in Symbols");
            mapping.pop();
        }
    }

    /// Enters a peice of data into the current scope
    pub fn insert(&mut self, key: K, value: V) {
        let mapping = self.table.entry(key).or_insert_with(Vec::new);
        mapping.push(value);

        self.scopes.push(Some(key));
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        self.table.get(key).and_then(|vec| vec.last())
    }
}
struct Builder<'a> {
    /// The current chunk
    chunk: Chunk,
    /// A count of all local vars
    /// The number is the postion of the local on the local stack
    locals: StackedMap<Register, usize>,

    params: &'a FnvHashMap<Register, usize>,
    ///  A linked list of all the objects allocated. This
    /// is passed to the vm so runtime collection can be done
    pub objects: RawObject,

    symbols: &'a Symbols<()>,
    /// The reporter used to reporter any errors
    reporter: &'a mut Reporter,
    /// The slot of the variable
    slots: u32,
    ///
    line: u32,
}

impl<'a> Builder<'a> {
    pub fn new(
        reporter: &'a mut Reporter,
        symbols: &'a Symbols<()>,
        objects: RawObject,
        params: &'a FnvHashMap<Register, usize>,
    ) -> Self {
        Builder {
            chunk: Chunk::new(),
            locals: StackedMap::new(),
            line: 0,
            slots: 0,
            symbols,
            params,
            objects,
            reporter,
        }
    }

    pub fn emit_byte(&mut self, byte: u8) {
        self.chunk.write(byte, self.line)
    }

    pub fn new_slot(&mut self) -> u32 {
        let slot = self.slots;
        self.slots += 1;
        slot
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

    pub fn emit_constant(&mut self, constant: vm::Value) -> CodegenResult<()> {
        let value = self.make_constant(constant);
        self.emit_bytes(opcode::CONSTANT, value);
        Ok(())
    }

    pub fn make_constant(&mut self, value: vm::Value) -> u8 {
        let index = self.chunk.add_constant(value);

        index as u8
    }

    pub fn compile_instruction(&mut self, inst: &Instruction) -> CodegenResult<()> {
        let ty = &inst.ty;
        let inst = &inst.instruction;

        match inst {
            Inst::Array(ref pos, ref size) => {
                self.compile_value(pos)?;
                self.emit_bytes(opcode::ARRAY, *size as u8);
            }
            Inst::Binary(_, ref lhs, ref op, ref rhs) => {
                self.compile_value(lhs)?;
                self.compile_value(rhs)?;

                match (ty, op) {
                    (Type::App(TypeCon::Int, _), BinaryOp::Plus) => self.emit_byte(opcode::ADD),
                    (Type::App(TypeCon::Float, _), BinaryOp::Plus) => self.emit_byte(opcode::ADDF),

                    (Type::App(TypeCon::Int, _), BinaryOp::Minus) => self.emit_byte(opcode::SUB),
                    (Type::App(TypeCon::Float, _), BinaryOp::Minus) => self.emit_byte(opcode::SUBF),

                    (Type::App(TypeCon::Int, _), BinaryOp::Div) => self.emit_byte(opcode::DIV),
                    (Type::App(TypeCon::Float, _), BinaryOp::Div) => self.emit_byte(opcode::DIVF),

                    (Type::App(TypeCon::Int, _), BinaryOp::Mul) => self.emit_byte(opcode::MUL),
                    (Type::App(TypeCon::Float, _), BinaryOp::Mul) => self.emit_byte(opcode::MULF),

                    // For comparisson the lhs and the rhs should be the same so only
                    // check the type of the lhs
                    (Type::App(TypeCon::Bool, _), BinaryOp::Lt) => match ty {
                        Type::App(TypeCon::Int, _) => self.emit_byte(opcode::LESS),
                        Type::App(TypeCon::Float, _) => self.emit_byte(opcode::LESSF),
                        _ => unreachable!(),
                    },

                    (Type::App(TypeCon::Bool, _), BinaryOp::Lte) => match ty {
                        Type::App(TypeCon::Int, _) => self.emit_bytes(opcode::LESS, opcode::NOT),
                        Type::App(TypeCon::Float, _) => self.emit_bytes(opcode::LESSF, opcode::NOT),
                        _ => unreachable!(),
                    },

                    (Type::App(TypeCon::Bool, _), BinaryOp::Gt) => match ty {
                        Type::App(TypeCon::Int, _) => self.emit_byte(opcode::GREATER),
                        Type::App(TypeCon::Float, _) => self.emit_byte(opcode::GREATERF),
                        _ => unreachable!(),
                    },

                    (Type::App(TypeCon::Bool, _), BinaryOp::Gte) => match ty {
                        Type::App(TypeCon::Int, _) => self.emit_bytes(opcode::GREATER, opcode::NOT),
                        Type::App(TypeCon::Float, _) => {
                            self.emit_bytes(opcode::GREATERF, opcode::NOT)
                        }
                        _ => unreachable!(),
                    },

                    (Type::App(TypeCon::Str, _), BinaryOp::Plus) => self.emit_byte(opcode::CONCAT),

                    (_, BinaryOp::Equal) => self.emit_byte(opcode::EQUAL),
                    (_, BinaryOp::NotEqual) => self.emit_bytes(opcode::EQUAL, opcode::NOT),

                    // #[cfg(not(feature = "debug"))]
                    // _ => unsafe {
                    //     ::std::hint::unreachable_unchecked() // only in release mode for that extra speed boost
                    // },

                    // #[cfg(feature = "debug")]
                    (ref ty, ref op) => unimplemented!(" ty {:?} op {:?}", ty, op),
                }
            }

            Inst::Call(_, ident, args) => {

                for arg in args.iter() {
                    self.compile_value(arg)?;
                }

                let callee = match ident {
                    Value::Named(ref sym) => *sym,
                    _ => unreachable!()
                };

                let name = self.symbols.name(callee);

                match name.as_str() {
                    "clock" | "rand" => self.emit_bytes(opcode::CALLNATIVE, callee.0 as u8),
                    _ => {
                        self.emit_bytes(opcode::CALL, callee.0 as u8);
                        self.emit_byte(args.len() as u8)
                    }
}


            }

            Inst::Cast(ref value, ref from, ref to) => {
                self.compile_value(value)?;
                match (from, to) {
                    (Type::App(TypeCon::Int, _), Type::App(TypeCon::Float, _)) => {
                        self.emit_byte(opcode::INT2FLOAT)
                    }

                    (Type::App(TypeCon::Float, _), Type::App(TypeCon::Int, _)) => {
                        self.emit_byte(opcode::FLOAT2INT)
                    }

                    (Type::App(TypeCon::Bool, _), Type::App(TypeCon::Int, _)) => {
                        self.emit_byte(opcode::BOOL2INT)
                    }

                    (Type::App(TypeCon::Int, _), Type::App(TypeCon::Str, _)) => {
                        self.emit_byte(opcode::INT2STR)
                    }

                    (Type::App(TypeCon::Float, _), Type::App(TypeCon::Str, _)) => {
                        self.emit_byte(opcode::FLOAT2STR)
                    }

                    _ => unreachable!(), // cast only allows int -> float, float -> int, bool -> int
                }
            }

            Inst::Drop(_) => {
                
            }

            Inst::Print(ref value) => {
                self.compile_value(value)?;
                self.emit_byte(opcode::PRINT)
            }

            Inst::Return(ref value) => {
                self.compile_value(value)?;
                self.emit_byte(opcode::RETURN)
            },

            Inst::StatementStart => (),

            Inst::Store(ref lhs,ref rhs) => {
                self.compile_value(lhs)?;
                self.compile_value(rhs)?;

            }

            Inst::Unary(_, ref operand, ref op) => {
                self.compile_value(operand)?;
                match *op {
                    UnaryOp::Bang => self.emit_byte(opcode::NOT),
                    UnaryOp::Minus => match ty {
                        Type::App(TypeCon::Int, _) => self.emit_byte(opcode::NEGATE),
                        Type::App(TypeCon::Float, _) => self.emit_byte(opcode::NEGATEF),
                        _ => unreachable!(),
                    },
                }
            }

            
        }

        Ok(())
    }

    fn compile_value(&mut self, value: &Value) -> CodegenResult<()> {
        match value {
            Value::Const(ref v) => self.emit_constant(vm::Value::int(*v)),
            Value::Float(ref v) => self.emit_constant(vm::Value::float(*v)),
            Value::Register(ref name) => {
                if let Some(pos) = self.locals.get(name).cloned() {
                    self.emit_bytes(opcode::GETLOCAL, pos as u8);
                } else if let Some(offset) = self.params.get(name).cloned() {
                    self.emit_bytes(opcode::GETPARAM, offset as u8);
                } else {
                    //do nothing because this is a temp and the value should be on the stack
                }

                Ok(())
            }
            Value::Named(ref name) => unimplemented!(),
            Value::Bool(ref b) => {
                if *b {
                    self.emit_byte(opcode::TRUE)
                } else {
                    self.emit_byte(opcode::FALSE)
                }

                Ok(())
            }
            Value::Nil => {
                self.emit_byte(opcode::NIL);
                Ok(())
            }
            Value::Mem(ref bytes) => {
                let object = StringObject::from_bytes(bytes, self.objects);
                self.emit_constant(vm::Value::object(object))
            }
        }
    }


}


fn compile_function(
    func: &Function,
    symbols: &Symbols<()>,
    reporter: &mut Reporter,
    objects: RawObject,
) -> CodegenResult<vm::Function> {
    let mut params = FnvHashMap::default();

    for (i, param) in func.params.iter().enumerate() {
        params.insert(*param, i);
    } // store param id and the index in the vec

    let mut chunks = HashMap::new();

    for (id, block) in func.blocks.iter() {
        let mut builder = Builder::new(reporter, symbols, objects, &params);

        for inst in block.instructions.iter() {
            builder.compile_instruction(inst)?;
        }
        
        builder.chunk.disassemble("test");

        chunks.insert(id, builder.chunk);
    }

    println!("{:?}",chunks );

    unimplemented!()
}

pub fn compile(
    ast: &Program,
    symbols: &Symbols<()>,
    reporter: &mut Reporter,
) -> CodegenResult<(vm::Program, RawObject)> {
    let mut funcs = FnvHashMap::default();
    let mut classes: FnvHashMap<Symbol, vm::Class> = FnvHashMap::default();

    let objects = ::std::ptr::null::<RawObject>() as RawObject;

    for function in ast.functions.iter() {
        funcs.insert(
            function.name,
            compile_function(function, symbols, reporter, objects)?,
        );
    }

    // for class in ast.classes.iter() {
    //     let mut compiled_class = compile_class(class, symbols, reporter, objects)?;

    //     if let Some(ref superclass) = class.superclass {
    //         let superclass = classes.get(&superclass.value).unwrap();

    //         compiled_class
    //             .methods
    //             .extend(superclass.methods.clone().into_iter());
    //     }

    //     classes.insert(class.name, compiled_class);
    // }

    Ok((
        vm::Program {
            functions: funcs,
            classes,
        },
        objects,
    ))
}
