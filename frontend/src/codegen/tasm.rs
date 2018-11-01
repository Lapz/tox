use codegen::label::Label;
pub use std::fmt::{self, Display};

#[derive(Debug)]
pub struct Register(pub u8);
/// Enum that used to create a type safe version of tasm
#[derive(Debug)]
pub enum TASM {
    STRINGDIRECTIVE(Label, String),
    DIRECTIVE(String),

    LABEL(Label),
    // ILLEGAL INST
    IGL,
    /// HLT
    /// Stops the running of the vm
    HLT,

    /// JMP Dest
    /// Changes the ip to the value in the register
    /// Allows for jumping forward or backwards
    JMP(Label),

    /// JMPF DEST
    /// increments the `ip` by the value stored in DEST
    JMPF(Label),

    /// JMPB DEST
    /// decrements the `ip` by the value stored in DEST
    JMPB(Label),

    /// JMPS if the equal flag is set,
    JMPEQ(Label),

    /// JMPS if the equal flag is not set,
    JMPNEQ(Label),

    /// ADD SRC SRC DEST
    ADD(Register, Register, Register),

    /// SUB SRC SRC DEST
    SUB(Register, Register, Register),

    /// MUL SRC SRC DEST
    MUL(Register, Register, Register),

    /// DIV SRC SRC DEST
    DIV(Register, Register, Register),

    /// NOT
    /// Set the equal_flag to !equal_flag
    NOT,

    /// EQUAL SRC SRC
    /// Sets the equal_flag to true
    EQUAL(Register, Register),

    /// GREATER SRC SRC
    /// Sets the equal_flag to true
    GREATER(Register, Register),

    /// Less SRC SRC
    /// Sets the equal_flag to
    LESS(Register, Register),

    /// LOAD #NUM DEST
    LOAD(Register, i64),

    /// STORES $SRC $DEST
    /// stores the value in src in dest
    STORE(Register, Register),

    /// ALLOC $BYTES
    /// Extends the heap by n bytes stored in a register
    ALLOC(Register),

    /// FREE $BYTES
    /// Shrinks the heap by n bytes stored in a register
    FREE(Register),

    /// INC $REG
    /// Increase the value stored in the register by
    INC(Register),

    /// DEC $REG
    /// Decrease the value stored in the register by
    DEC(Register),

    /// PUSH $REG
    /// Pushes the value onto the stack
    PUSH(Register),

    /// POP $REG
    /// Pops the value off the top of stack
    POP(Register),

    /// MOD SRC SRC DEST
    /// CALCULATES SRC/SRC AND STORES IT IN DEST
    MOD(Register, Register, Register),

    /// EXPON SRC SRC DEST
    /// CALCULATES SRC^SRC AND STORES IT IN DEST
    EXPON(Register, Register, Register),

    SET(Register),
}

impl Display for TASM {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::TASM::*;
        match *self {
            STRINGDIRECTIVE(ref label, ref string) => {
                write!(f, "{}: .asciiz \"{}\"", label, string)
            }
            DIRECTIVE(ref d) => write!(f, "{}", d),
            LABEL(ref l) => write!(f, "{}:", l),
            IGL => write!(f, "IGL"),
            HLT => write!(f, "HLT"),
            NOT => write!(f, "NOT"),
            JMP(ref label) => write!(f, "JMP @{}", label),
            JMPF(ref label) => write!(f, "JMPF @{}", label),
            JMPB(ref label) => write!(f, "JMPB @{}", label),
            JMPEQ(ref label) => write!(f, "JMPEQ @{}", label),
            JMPNEQ(ref label) => write!(f, "JMPNEQ @{}", label),
            ADD(ref src1, ref src2, ref dest) => write!(f, "ADD {} {} {}", src1, src2, dest),
            SUB(ref src1, ref src2, ref dest) => write!(f, "SUB {} {} {}", src1, src2, dest),
            MUL(ref src1, ref src2, ref dest) => write!(f, "MUL {} {} {}", src1, src2, dest),
            DIV(ref src1, ref src2, ref dest) => write!(f, "DIV {} {} {}", src1, src2, dest),
            MOD(ref src1, ref src2, ref dest) => write!(f, "MOD {} {} {}", src1, src2, dest),
            EXPON(ref src1, ref src2, ref dest) => write!(f, "EXPON {} {} {}", src1, src2, dest),
            EQUAL(ref src1, ref src2) => write!(f, "EQUAL {} {}", src1, src2),
            GREATER(ref src1, ref src2) => write!(f, "GREATER {} {}", src1, src2),
            LESS(ref src1, ref src2) => write!(f, "LESS {} {}", src1, src2),
            LOAD(ref reg, ref val) => write!(f, "LOAD {} #{}", reg, val),
            STORE(ref src1, ref src2) => write!(f, "STORE {} {}", src1, src2),
            ALLOC(ref reg) => write!(f, "ALLOC {}", reg),
            FREE(ref reg) => write!(f, "FREE {}", reg),
            INC(ref reg) => write!(f, "INC {}", reg),
            DEC(ref reg) => write!(f, "DEC {}", reg),
            PUSH(ref reg) => write!(f, "PUSH {}", reg),
            POP(ref reg) => write!(f, "POP {}", reg),
            SET(ref reg) => write!(f, "SET {}", reg),
            _ => unimplemented!(),
        }
    }
}

impl Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "${}", self.0)
    }
}
