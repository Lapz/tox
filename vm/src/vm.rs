use opcode;

#[derive(Debug)]
pub struct VM {
    registers: [i32; 32],
    pub code: Vec<u8>,
    heap: Vec<u8>,
    ip: usize,
    remainder: u32,
    equal_flag: bool,
}

impl VM {
    pub fn new() -> Self {
        VM {
            ip: 0,
            registers: [0; 32],
            code: Vec::new(),
            remainder: 0,
            equal_flag: false,
            heap: Vec::new(),
        }
    }

    pub fn run(&mut self) {
        loop {
            println!("{:?}", &self.registers[0..7]);
            if self.ip >= self.code.len() {
                return;
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
                        self.next_8_bits();
                        self.next_8_bits();
                    }
                }

                opcode::JMPNEQ => {
                    let location = self.registers[self.next_8_bits() as usize];

                    if !self.equal_flag {
                        self.ip = location as usize;
                    } else {
                        self.next_8_bits();
                        self.next_8_bits();
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
                    self.next_16_bits();
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

                    self.next_8_bits();
                }

                opcode::GREATER => {
                    let lhs = self.registers[self.next_8_bits() as usize];
                    let rhs = self.registers[self.next_8_bits() as usize];

                    if lhs > rhs {
                        self.equal_flag = true;
                    } else {
                        self.equal_flag = false;
                    }

                    self.next_8_bits();
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
                    self.next_8_bits();
                    self.equal_flag = !self.equal_flag;
                    self.next_8_bits();
                    self.next_8_bits();
                }

                opcode::STORE => {
                    let src = self.registers[self.next_8_bits() as usize];
                    let dest = self.registers[self.next_8_bits() as usize];

                    self.registers[dest as usize] = self.registers[src as usize];

                    self.next_8_bits();
                }

                _ => {
                    println!("ip = {}", self.ip);
                    println!("Unknown opcode {:x}", self.code[self.ip]);
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

    pub fn code(&mut self, code: Vec<u8>) {
        self.code = code;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let mut test_vm = VM::new();
        assert_eq!(test_vm.registers[0], 0)
    }

    #[test]

    fn test_hlt_opcode() {
        let mut test_vm = VM::new();

        let test_bytes = vec![0x1, 0, 0, 0];

        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.ip, 1);
    }
    #[test]
    fn test_load_opcode() {
        let mut test_vm = VM::new();

        let test_bytes = vec![0x16, 0, 1, 244, 1];

        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.registers[0], 500);
    }

    #[test]
    fn test_add_opcode() {
        let mut test_vm = VM::new();

        let test_bytes = vec![
            0x16, 0, 1, 244, // LOAD $500 into REG 0
            0x16, 1, 1, 250, // LOAD $506 into REG 1
            0x6, 0, 1, 0, // ADD R1 R2 R1
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
            0x16, 0, 1, 250, // LOAD $500 into REG 0
            0x16, 1, 1, 244, // LOAD $506 into REG 1
            0x7, 0, 1, 0, // SUB R1 R2 R1
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
            0x16, 0, 0, 2, // LOAD $2 into REG 0
            0x16, 1, 0, 10, // LOAD $10 into REG 1
            0x8, 0, 1, 0, // MUL R1 R2 R1
            1, 0, 0, 0,
        ];

        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.registers[0], 20);
    }

    #[test]
    fn test_div_opcode() {
        let mut test_vm = VM::new();

        let test_bytes = vec![
            0x16, 0, 0, 5, // LOAD $2 into REG 0
            0x16, 1, 0, 3, // LOAD $10 into REG 1
            0x9, 0, 1, 0, // DIV R1 R2 R1
            1, 0, 0, 0,
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
            0x2, 0, 0, 0, // JMP to $0
            0x16, 1, 0, 10, // LOAD $10 into REG 0
            0x16, 0, 0, 2, // LOAD $2 into REG 0
            1, 0, 0, 0,
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
            0x18, 0, 0, 0, // JMPF by 3
            1, 0, 0, 0,
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
            0x2, 0, 0, 0, // JMP to 5
            1, 0, 0, 0, //HLT
            0x16, 1, 0, 11, // LOAD #7 into $1
            0x19, 1, 0, 0, // JMPB by $1(7)
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
            0x14, 0, 1, 0, // EQ $1 $2
            1, 0, 0, 0, //HLT
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
            0x17, 0, 1, 0, // GREATER $1 $2
            1, 0, 0, 0, //HLT
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
            0x14, 0, 1, 0, // EQUAL $1 $2
            0x13, 0, 0, 0, // NOT
            1, 0, 0, 0, //HLT
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
            0x14, 0, 1, 0, // EQUAL $1 $2
            0x13, 0, 0, 0, // NOT
            1, 0, 0, 0, //HLT
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
            0x20, 0, 0, 0, // JMPEQUAL $1
            0x13, 0, 0, 0, // NOT
            1, 0, 0, 0, //HLT
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
            0x23, 0, 0, 0, // ALLOC $0
            1, 0, 0, 0, //HLT
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
            0x23, 0, 0, 0, // ALLOC $0
            0x24, 1, 0, 0, // FREE $1
            1, 0, 0, 0, //HLT
        ];

        test_vm.code(test_bytes);

        test_vm.run();

        assert_eq!(test_vm.heap.len(), 512);
    }

}
