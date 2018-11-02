use super::Function;
use chunk::Chunk;
use opcode;
use std::mem;
use util::symbol::Symbol;
use value::Value;

/// The max size of the stack
const STACK_MAX: usize = 256;

pub struct StackFrame<'a> {
    ip: usize,
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
            function: main_funciton.unwrap(),
        };

        Ok(VM {
            stack: [Value::nil(); STACK_MAX],
            current_frame,
            functions,
            frames: Vec::new(),
            equal_flag: false,
            heap: Vec::new(),
            stack_top: 1,
        })
    }

    pub fn run(&mut self) {
        loop {
            {
                if self.current_frame.ip >= self.current_frame.function.body.code.len() {
                    return;
                }
            }

            match self.read_byte() {
                opcode::HLT => {
                    break;
                }

                opcode::RETURN => {
                    let value = self.pop();

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

                _ => {
                    continue;
                }
            }
        }
    }

    fn read_constant(&mut self) -> Value {
        let index = self.read_byte() as usize;
        self.current_frame.function.body.constants[index]
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

// #[cfg(feature = "debug")]
// impl <'a> VM<'a> {
//     #[cfg(feature = "debug")]
//     pub fn disassemble(&self, name: &str) {
//         println!("== {} ==\n", name);

//         for function in self.functions.iter() {
//             let mut i = 0;

//             function.body.disassemble(&format!("{}",function.name));

//         }

//     }
// }

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

    fn prepend_header(mut b: Vec<u8>) -> Vec<u8> {
        let mut header = Vec::with_capacity(b.len() + PIE_HEADER_LENGTH);

        for byte in &PIE_HEADER_PREFIX[0..] {
            header.push(*byte);
        }

        while header.len() <= PIE_HEADER_LENGTH {
            header.push(0);
        }

        header.append(&mut b);

        header
    }

    #[test]
    fn it_works() {
        let test_vm = VM::new();
        assert_eq!(test_vm.registers[0], 0)
    }

    #[test]

    fn test_hlt_opcode() {
        let mut test_vm = VM::new();

        let mut test_bytes = vec![opcode::HLT, 0, 0, 0];

        test_bytes = prepend_header(test_bytes);
        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.ip, 66);
    }
    #[test]
    fn test_load_opcode() {
        let mut test_vm = VM::new();

        let mut test_bytes = vec![opcode::LOAD, 0, 1, 244, 1];

        test_bytes = prepend_header(test_bytes);
        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.registers[0], 500);
    }

    #[test]
    fn test_add_opcode() {
        let mut test_vm = VM::new();

        let mut test_bytes = vec![
            opcode::LOAD,
            0,
            1,
            244, // LOAD $500 into REG 0
            opcode::LOAD,
            1,
            1,
            250, // LOAD $506 into REG 1
            opcode::ADD,
            0,
            1,
            0, // ADD R1 R2 R1
            1,
        ];

        test_bytes = prepend_header(test_bytes);
        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.registers[0], 1006);
    }

    #[test]
    fn test_sub_opcode() {
        let mut test_vm = VM::new();

        let mut test_bytes = vec![
            opcode::LOAD,
            0,
            1,
            250, // LOAD $500 into REG 0
            opcode::LOAD,
            1,
            1,
            244, // LOAD $506 into REG 1
            opcode::SUB,
            0,
            1,
            0, // SUB R1 R2 R1
            1,
        ];

        test_bytes = prepend_header(test_bytes);
        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.registers[0], 6);
    }

    #[test]
    fn test_mul_opcode() {
        let mut test_vm = VM::new();

        let mut test_bytes = vec![
            opcode::LOAD,
            0,
            0,
            2, // LOAD $2 into REG 0
            opcode::LOAD,
            1,
            0,
            10, // LOAD $10 into REG 1
            opcode::MUL,
            0,
            1,
            0, // MUL R1 R2 R1
            1,
            0,
            0,
            0,
        ];

        test_bytes = prepend_header(test_bytes);
        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.registers[0], 20);
    }

    #[test]
    fn test_div_opcode() {
        let mut test_vm = VM::new();

        let mut test_bytes = vec![
            opcode::LOAD,
            0,
            0,
            5, // LOAD $2 into REG 0
            opcode::LOAD,
            1,
            0,
            3, // LOAD $10 into REG 1
            opcode::DIV,
            0,
            1,
            0, // DIV R1 R2 R1
            1,
            0,
            0,
            0,
        ];

        test_bytes = prepend_header(test_bytes);
        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.registers[0], 1);
        assert_eq!(test_vm.remainder, 2);
    }

    #[test]
    fn test_jmp_opcode() {
        let mut test_vm = VM::new();

        test_vm.registers[0] = 68;

        let mut test_bytes = vec![
            opcode::JMP,
            0,
            0,
            0, // JMP to $0
            opcode::LOAD,
            1,
            0,
            10, // LOAD $10 into REG 0
            opcode::LOAD,
            0,
            0,
            2, // LOAD $2 into REG 0
            1,
            0,
            0,
            0,
        ];

        test_bytes = prepend_header(test_bytes);
        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.registers[0], 2);
        assert_eq!(test_vm.ip, 78);
    }

    #[test]
    fn test_jmpf_opcode() {
        let mut test_vm = VM::new();

        test_vm.registers[0] = 2;

        let mut test_bytes = vec![
            opcode::JMPF,
            0,
            0,
            0, // JMPF by 3
            1,
            0,
            0,
            0,
        ];

        test_bytes = prepend_header(test_bytes);
        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.ip, 70);
    }

    #[test]
    fn test_jmpb_opcode() {
        let mut test_vm = VM::new();

        test_vm.registers[0] = 70;

        let mut test_bytes = vec![
            opcode::JMP,
            0,
            0,
            0, // JMP to 5
            opcode::HLT,
            0,
            0,
            0, //HLT
            opcode::LOAD,
            1,
            0,
            11, // LOAD #11 into $1
            opcode::JMPB,
            1,
            0,
            0, // JMPB by $1(7)
        ];

        test_bytes = prepend_header(test_bytes);
        test_vm.code(test_bytes);

        test_vm.run();
        //        println!("{:?}",&test_vm.registers()[0..]);
        assert_eq!(test_vm.registers[1], 11);
        assert_eq!(test_vm.ip, 70);
    }

    #[test]
    fn test_eq_opcode() {
        let mut test_vm = VM::new();

        test_vm.registers[0] = 5;
        test_vm.registers[1] = 5;

        let mut test_bytes = vec![
            opcode::EQUAL,
            0,
            1,
            0, // EQ $1 $2
            opcode::HLT,
            0,
            0,
            0, //HLT
        ];

        test_bytes = prepend_header(test_bytes);
        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.equal_flag, true);
    }

    #[test]
    fn test_less_opcode() {
        let mut test_vm = VM::new();

        test_vm.registers[0] = 1;
        test_vm.registers[1] = 5;

        let mut test_bytes = vec![
            opcode::LESS,
            0,
            1,
            0, // LESS $1 $2
            opcode::HLT,
            0,
            0,
            0, //HLT
        ];

        test_bytes = prepend_header(test_bytes);
        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.equal_flag, true);
    }

    #[test]
    fn test_not_equal_opcode() {
        let mut test_vm = VM::new();

        test_vm.registers[0] = 5;
        test_vm.registers[1] = 5;

        let mut test_bytes = vec![
            opcode::EQUAL,
            0,
            1,
            0, // EQUAL $1 $2
            opcode::NOT,
            0,
            0,
            0, // NOT
            opcode::HLT,
            0,
            0,
            0, //HLT
        ];

        test_bytes = prepend_header(test_bytes);
        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.equal_flag, false);

        test_vm.ip = 0;

        test_vm.registers[0] = 4;
        test_vm.registers[1] = 5;

        test_vm.run();

        assert_eq!(test_vm.equal_flag, true);
    }

    #[test]
    fn test_less_equal_opcode() {
        let mut test_vm = VM::new();

        test_vm.registers[0] = 5;
        test_vm.registers[1] = 5;

        let mut test_bytes = vec![
            opcode::EQUAL,
            0,
            1,
            0, // EQUAL $1 $2
            opcode::NOT,
            0,
            0,
            0, // NOT
            opcode::HLT,
            0,
            0,
            0, //HLT
        ];

        test_bytes = prepend_header(test_bytes);
        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.equal_flag, false);

        test_vm.ip = 0;

        test_vm.registers[0] = 4;
        test_vm.registers[1] = 5;

        test_vm.run();

        assert_eq!(test_vm.equal_flag, true);
    }

    #[test]
    fn test_jump_equal_opcode() {
        let mut test_vm = VM::new();

        test_vm.registers[0] = 72;
        test_vm.equal_flag = true;

        let mut test_bytes = vec![
            opcode::JMPEQ,
            0,
            72,
            0, // JMPEQUAL $1
            opcode::NOT,
            0,
            0,
            0, // NOT
            opcode::HLT,
            0,
            0,
            0, //HLT
        ];

        test_bytes = prepend_header(test_bytes);
        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.equal_flag, true);
    }

    #[test]
    fn test_alloc_opcode() {
        let mut test_vm = VM::new();

        test_vm.registers[0] = 1024;

        let mut test_bytes = vec![
            opcode::ALLOC,
            0,
            0,
            0, // ALLOC $0
            opcode::HLT,
            0,
            0,
            0, //HLT
        ];

        test_bytes = prepend_header(test_bytes);
        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.heap.len(), 1024);
    }

    #[test]
    fn test_free_opcode() {
        let mut test_vm = VM::new();

        test_vm.registers[0] = 1024;
        test_vm.registers[1] = 512;

        let mut test_bytes = vec![
            opcode::ALLOC,
            0,
            0,
            0, // ALLOC $0
            opcode::FREE,
            1,
            0,
            0, // FREE $1
            opcode::HLT,
            0,
            0,
            0, //HLT
        ];

        test_bytes = prepend_header(test_bytes);
        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.heap.len(), 512);
    }

    #[test]
    fn test_inc_opcode() {
        let mut test_vm = VM::new();

        test_vm.registers[0] = 100;

        let mut test_bytes = vec![
            opcode::INC,
            0,
            0,
            0, // INC $0
            opcode::HLT,
            0,
            0,
            0, //HLT
        ];

        test_bytes = prepend_header(test_bytes);
        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.registers[0], 101);
    }

    #[test]
    fn test_dec_opcode() {
        let mut test_vm = VM::new();

        test_vm.registers[0] = 100;

        let mut test_bytes = vec![
            opcode::DEC,
            0,
            0,
            0, // DEC $0
            opcode::HLT,
            0,
            0,
            0, //HLT
        ];

        test_bytes = prepend_header(test_bytes);
        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.registers[0], 99);
    }

    #[test]
    fn test_push_opcode() {
        let mut test_vm = VM::new();

        test_vm.registers[0] = 100;

        let mut test_bytes = vec![
            opcode::PUSH,
            0,
            0,
            0, // PUSH $0
            opcode::HLT,
            0,
            0,
            0, //HLT
        ];

        test_bytes = prepend_header(test_bytes);
        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.stack[test_vm.stack_top - 1], 100);
    }

    #[test]
    fn test_pop_opcode() {
        let mut test_vm = VM::new();

        test_vm.registers[0] = 100;
        test_vm.registers[1] = 200;

        let mut test_bytes = vec![
            opcode::PUSH,
            0,
            0,
            0, // PUSH $0
            opcode::PUSH,
            1,
            0,
            0, // PUSH $1
            opcode::POP,
            0,
            0,
            0, // POP $0
            opcode::HLT,
            0,
            0,
            0, // HLT
        ];

        test_bytes = prepend_header(test_bytes);
        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.registers[0], 200);
    }

    #[test]
    fn test_set_opcode() {
        let mut test_vm = VM::new();

        test_vm.registers[0] = 1;

        let mut test_bytes = vec![
            opcode::SET,
            0,
            0,
            0, // PUSH $0
            opcode::HLT,
            0,
            0,
            0, //HLT
        ];

        test_bytes = prepend_header(test_bytes);
        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.equal_flag, true);
    }

}
