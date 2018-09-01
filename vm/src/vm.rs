use opcode;

/// The max size of the stack
const STACK_MAX: usize = 256;

pub struct VM {
    registers: [i32; 32],
    stack: [i32; STACK_MAX],
    pub code: Vec<u8>,
    heap: Vec<u8>,
    ip: usize,
    remainder: u32,
    equal_flag: bool,
    stack_top: usize,
}

impl VM {
    pub fn new() -> Self {
        VM {
            ip: 0,
            registers: [0; 32],
            stack: [0; STACK_MAX],
            code: Vec::new(),
            remainder: 0,
            equal_flag: false,
            heap: Vec::new(),
            stack_top: 0,
        }
    }

    pub fn run(&mut self) {
        loop {
            {
                if self.ip >= self.code.len() {
                    return;
                }

                println!("{:?}", &self.stack[0..2]);
            }

            match self.read_byte() {
                opcode::HLT => {
                    break;
                }

                opcode::JMP => {
                    let location = self.registers[self.next_8_bits() as usize];

                    self.ip = location as usize;
                }

                opcode::JMPF => {
                    let value = self.registers[self.next_8_bits() as usize];
                    self.ip += value as usize;
                }

                opcode::JMPB => {
                    let value = self.registers[self.next_8_bits() as usize];
                    self.ip -= value as usize;
                }

                opcode::JMPEQ => {
                    let location = self.registers[self.next_8_bits() as usize];

                    if self.equal_flag {
                        self.ip = location as usize;
                    } else {
                        self.advance(2);
                    }
                }

                opcode::JMPNEQ => {
                    let location = self.registers[self.next_8_bits() as usize];

                    if !self.equal_flag {
                        self.ip = location as usize;
                    } else {
                        self.advance(2);
                    }
                }

                opcode::LOAD => {
                    let register = self.next_8_bits() as usize;
                    let number = self.next_16_bits() as u16;
                    self.registers[register] = number as i32;
                }

                opcode::ALLOC => {
                    let bytes = self.registers[self.next_8_bits() as usize];
                    let size = self.heap.len() + bytes as usize;

                    self.heap.resize(size, 0);
                    self.next_16_bits();
                }

                opcode::FREE => {
                    let bytes = self.registers[self.next_8_bits() as usize];
                    let size = self.heap.len() - bytes as usize;

                    self.heap.resize(size, 0);
                    self.advance(2);
                }

                opcode::ADD => {
                    let lhs = self.registers[self.next_8_bits() as usize];
                    let rhs = self.registers[self.next_8_bits() as usize];
                    self.registers[self.next_8_bits() as usize] = lhs + rhs;
                }

                opcode::SUB => {
                    let lhs = self.registers[self.next_8_bits() as usize];
                    let rhs = self.registers[self.next_8_bits() as usize];
                    self.registers[self.next_8_bits() as usize] = lhs - rhs;
                }

                opcode::MUL => {
                    let lhs = self.registers[self.next_8_bits() as usize];
                    let rhs = self.registers[self.next_8_bits() as usize];
                    self.registers[self.next_8_bits() as usize] = lhs * rhs;
                }

                opcode::DIV => {
                    let lhs = self.registers[self.next_8_bits() as usize];
                    let rhs = self.registers[self.next_8_bits() as usize];
                    self.registers[self.next_8_bits() as usize] = lhs / rhs;
                    self.remainder = (lhs % rhs) as u32;
                }

                opcode::EQUAL => {
                    let lhs = self.registers[self.next_8_bits() as usize];
                    let rhs = self.registers[self.next_8_bits() as usize];

                    if lhs == rhs {
                        self.equal_flag = true;
                    } else {
                        self.equal_flag = false;
                    }

                    self.advance(1);
                }

                opcode::GREATER => {
                    let lhs = self.registers[self.next_8_bits() as usize];
                    let rhs = self.registers[self.next_8_bits() as usize];

                    if lhs > rhs {
                        self.equal_flag = true;
                    } else {
                        self.equal_flag = false;
                    }

                    self.advance(1);
                }

                opcode::LESS => {
                    let lhs = self.registers[self.next_8_bits() as usize];
                    let rhs = self.registers[self.next_8_bits() as usize];

                    if lhs < rhs {
                        self.equal_flag = true;
                    } else {
                        self.equal_flag = false;
                    }
                }

                opcode::NOT => {
                    self.equal_flag = !self.equal_flag;
                    self.advance(3);
                }

                opcode::STORE => {
                    let src = self.registers[self.next_8_bits() as usize];
                    let dest = self.registers[self.next_8_bits() as usize];

                    self.registers[dest as usize] = self.registers[src as usize];

                    self.advance(1);
                }

                opcode::INC => {
                    self.registers[self.next_8_bits() as usize] += 1;
                    self.advance(2);
                }

                opcode::DEC => {
                    self.registers[self.next_8_bits() as usize] -= 1;
                    self.advance(2);
                }

                opcode::PUSH => {
                    let val = self.registers[self.next_8_bits() as usize];
                    self.push(val);
                    self.advance(2)
                }

                opcode::POP => {
                    let val = self.pop();
                    self.registers[self.next_8_bits() as usize] = val;
                    self.advance(2)
                }

                _ => {
                    println!("{:?}", self.equal_flag);
                    //                    println!("ip = {}", self.ip);
                    //                    println!("Unknown opcode {}", self.code[self.ip-1]);
                    //                    println!("{:?}",&self.registers[0..7]);
                    continue;
                }
            }
        }
    }

    fn read_byte(&mut self) -> u8 {
        let byte = self.code[self.ip];
        self.ip += 1;
        byte
    }

    fn advance(&mut self, n: usize) {
        self.ip += n;
    }

    fn next_8_bits(&mut self) -> u8 {
        let result = self.code[self.ip];
        self.ip += 1;

        result
    }

    fn next_16_bits(&mut self) -> u16 {
        let result = ((self.code[self.ip] as u16) << 8) | self.code[self.ip + 1] as u16;
        // Shifts the instruction by 8 to the right and or all the 1's and 0's
        self.ip += 2;

        result
    }

    fn push(&mut self, val: i32) {
        self.stack[self.stack_top] = val;
        self.stack_top += 1;
    }

    fn pop(&mut self) -> i32 {
        self.stack_top -= 1;
        self.stack[self.stack_top]
    }

    pub fn code(&mut self, code: Vec<u8>) {
        self.code = code;
    }
}

use std::fmt::{self, Debug};

impl Debug for VM {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut debug_trait_builder = f.debug_struct("VM");
        let _ = debug_trait_builder.field("registers", &self.registers);
        // let _ = debug_trait_builder.field("stack", &self.stack[0..].iter());
        let _ = debug_trait_builder.field("code", &self.code);
        let _ = debug_trait_builder.field("heap", &self.heap);
        let _ = debug_trait_builder.field("ip", &self.ip);

        let _ = debug_trait_builder.field("remainder", &self.remainder);
        let _ = debug_trait_builder.field("equal_flag", &self.equal_flag);
        debug_trait_builder.finish()

        //  f.debug_list().entries(self.stack[0..].iter()).finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use opcode;

    #[test]
    fn it_works() {
        let test_vm = VM::new();
        assert_eq!(test_vm.registers[0], 0)
    }

    #[test]

    fn test_hlt_opcode() {
        let mut test_vm = VM::new();

        let test_bytes = vec![opcode::HLT, 0, 0, 0];

        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.ip, 1);
    }
    #[test]
    fn test_load_opcode() {
        let mut test_vm = VM::new();

        let test_bytes = vec![opcode::LOAD, 0, 1, 244, 1];

        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.registers[0], 500);
    }

    #[test]
    fn test_add_opcode() {
        let mut test_vm = VM::new();

        let test_bytes = vec![
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

        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.registers[0], 1006);
    }

    #[test]
    fn test_sub_opcode() {
        let mut test_vm = VM::new();

        let test_bytes = vec![
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

        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.registers[0], 6);
    }

    #[test]
    fn test_mul_opcode() {
        let mut test_vm = VM::new();

        let test_bytes = vec![
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

        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.registers[0], 20);
    }

    #[test]
    fn test_div_opcode() {
        let mut test_vm = VM::new();

        let test_bytes = vec![
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

        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.registers[0], 1);
        assert_eq!(test_vm.remainder, 2);
    }

    #[test]
    fn test_jmp_opcode() {
        let mut test_vm = VM::new();

        test_vm.registers[0] = 8;

        let test_bytes = vec![
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

        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.registers[0], 2);
        assert_eq!(test_vm.ip, 13);
    }

    #[test]
    fn test_jmpf_opcode() {
        let mut test_vm = VM::new();

        test_vm.registers[0] = 2;

        let test_bytes = vec![
            opcode::JMPF,
            0,
            0,
            0, // JMPF by 3
            1,
            0,
            0,
            0,
        ];

        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.ip, 5);
    }

    #[test]
    fn test_jmpb_opcode() {
        let mut test_vm = VM::new();

        test_vm.registers[0] = 5;

        let test_bytes = vec![
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
            11, // LOAD #7 into $1
            opcode::JMPB,
            1,
            0,
            0, // JMPB by $1(7)
        ];

        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.registers[1], 11);
        assert_eq!(test_vm.ip, 5);
    }

    #[test]
    fn test_eq_opcode() {
        let mut test_vm = VM::new();

        test_vm.registers[0] = 5;
        test_vm.registers[1] = 5;

        let test_bytes = vec![
            opcode::EQUAL,
            0,
            1,
            0, // EQ $1 $2
            opcode::HLT,
            0,
            0,
            0, //HLT
        ];

        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.equal_flag, true);
    }

    #[test]
    fn test_less_opcode() {
        let mut test_vm = VM::new();

        test_vm.registers[0] = 1;
        test_vm.registers[1] = 5;

        let test_bytes = vec![
            opcode::LESS,
            0,
            1,
            0, // LESS $1 $2
            opcode::HLT,
            0,
            0,
            0, //HLT
        ];

        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.equal_flag, true);
    }

    #[test]
    fn test_not_equal_opcode() {
        let mut test_vm = VM::new();

        test_vm.registers[0] = 5;
        test_vm.registers[1] = 5;

        let test_bytes = vec![
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

        let test_bytes = vec![
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

        test_vm.registers[0] = 8;
        test_vm.equal_flag = true;

        let test_bytes = vec![
            opcode::JMPEQ,
            0,
            0,
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

        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.equal_flag, true);
    }

    #[test]
    fn test_alloc_opcode() {
        let mut test_vm = VM::new();

        test_vm.registers[0] = 1024;

        let test_bytes = vec![
            opcode::ALLOC,
            0,
            0,
            0, // ALLOC $0
            opcode::HLT,
            0,
            0,
            0, //HLT
        ];

        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.heap.len(), 1024);
    }

    #[test]
    fn test_free_opcode() {
        let mut test_vm = VM::new();

        test_vm.registers[0] = 1024;
        test_vm.registers[1] = 512;

        let test_bytes = vec![
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

        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.heap.len(), 512);
    }

    #[test]
    fn test_inc_opcode() {
        let mut test_vm = VM::new();

        test_vm.registers[0] = 100;

        let test_bytes = vec![
            opcode::INC,
            0,
            0,
            0, // INC $0
            opcode::HLT,
            0,
            0,
            0, //HLT
        ];

        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.registers[0], 101);
    }

    #[test]
    fn test_dec_opcode() {
        let mut test_vm = VM::new();

        test_vm.registers[0] = 100;

        let test_bytes = vec![
            opcode::DEC,
            0,
            0,
            0, // DEC $0
            opcode::HLT,
            0,
            0,
            0, //HLT
        ];

        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.registers[0], 99);
    }

    #[test]
    fn test_push_opcode() {
        let mut test_vm = VM::new();

        test_vm.registers[0] = 100;

        let test_bytes = vec![
            opcode::PUSH,
            0,
            0,
            0, // PUSH $0
            opcode::HLT,
            0,
            0,
            0, //HLT
        ];

        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.stack[test_vm.stack_top - 1], 100);
    }

    #[test]
    fn test_pop_opcode() {
        let mut test_vm = VM::new();

        test_vm.registers[0] = 100;
        test_vm.registers[1] = 200;

        let test_bytes = vec![
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

        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.registers[0], 200);
    }

}
