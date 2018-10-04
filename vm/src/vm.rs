use chunk::Chunk;
use op::{OpCode, TryFrom};

pub struct VM<'a> {
    pub code: &'a mut Chunk,
    stack: [u8; 256],
    stack_top: usize,
    ip: usize,
}

/// Converts a slice of n length to type

macro_rules! to_num {
    ([$stack:expr, $top:expr] => $type:ty) => {{
        use std::default;
        use std::mem;

        $top -= mem::size_of::<$type>();

        let mut b: [u8; mem::size_of::<$type>()] = default::Default::default();

        b.copy_from_slice(&$stack[$top..$top + mem::size_of::<$type>()]);
        unsafe { mem::transmute::<_, $type>(b) }
    }};
}

macro_rules! push {
    ($bytes:expr => $stack:expr,[$from:expr, $to:expr]) => {{
        let mut b = &mut$stack[$from..($from + $to)];

        b.copy_from_slice($bytes);

        $from += $to;
    }};
}

macro_rules! debug {
    ($($p:tt)*) => {if cfg!(feature = "debug") { println!($($p)*) } else { }}
}

macro_rules! to_bytes {
    ($expr:expr => $type:ty) => {{
        use std::mem;
        unsafe { mem::transmute::<_, [u8; mem::size_of::<$type>()]>($expr) }
    }};
}

macro_rules! binary_op {
    ($op:tt, $_self:ident,$type:ty) => {{
        $_self.ip += 1;

       let a = to_num!([&$_self.stack,$_self.stack_top] => $type);
        let b = to_num!([&$_self.stack,$_self.stack_top] => $type);
        use std::mem;
        push!( &to_bytes!(a $op b => $type)     => $_self.stack,[$_self.stack_top,mem::size_of::<$type>()]);
    }};
}

macro_rules! cmp_op {
    ($op:tt, $_self:ident,$type:ty) => {{
        $_self.ip += 1;

       let a = to_num!([&$_self.stack,$_self.stack_top] => $type);
        let b = to_num!([&$_self.stack,$_self.stack_top] => $type);
        use std::mem;

        push!( &to_bytes!(a $op b => $type)      => $_self.stack,[$_self.stack_top,mem::size_of::<$type>()]);
    }};
}

type VMResult = Result<(), VMError>;

#[derive(Debug)]
pub enum VMError {
    CompilerError,
    RuntimeError,
}

impl<'a> VM<'a> {
    fn reset_stack(&mut self) {
        self.stack_top = 0;
    }

    pub fn new(code: &'a mut Chunk) -> Self {
        VM {
            ip: 0,
            stack_top: 1,
            stack: [0; 256],
            code,
        }
    }

    pub fn run(&mut self) -> VMResult {
        debug!("{:?}", self.code.dissassemble("test"));

        loop {
            if cfg!(feature = "stack") {
                println!("[");

                for (i, byte) in self.stack.iter().enumerate() {
                    if i + 1 == self.stack.len() {
                        print!("{}", byte);
                    } else {
                        print!("{},", byte);
                    }
                }

                println!("]");
            }

            match OpCode::try_from(self.code[self.ip]) {
                Ok(OpCode::Return) => {
                    self.ip += 1;

                    let size = self.code[self.ip] as usize;
                    // 0 - nil
                    // 1 - bool,
                    // 2 - int,
                    // 3 - float

                    match size {
                        1 => println!("{}", to_num!([&self.stack,self.stack_top] => bool)),
                        2 => println!("{}", to_num!([&self.stack,self.stack_top] => i64)),

                        3 => println!("{}", to_num!([&self.stack,self.stack_top] => f64)),

                        _ => unreachable!(),
                    };

                    return Ok(());
                }
                Ok(OpCode::Int) => {
                    self.ip += 1;

                    let index = self.code[self.ip] as usize;

                    push!(&self.code.constants[index..index+8] => self.stack,[self.stack_top,8]);
                    self.ip += 1;

                    // break;
                }
                Ok(OpCode::Float) => {
                    self.ip += 1;

                    let index = self.code[self.ip] as usize;

                    push!(&self.code.constants[index..index+8] => self.stack,[self.stack_top,8]);
                    self.ip += 1;

                    // break;
                }

                Ok(OpCode::String) => {
                    self.ip += 1;
                    let index = self.code[self.ip] as usize;
                    self.ip += 1;

                    let len = self.code[self.ip] as usize;

                    push!(&self.code.constants[index..index+len] => self.stack,[self.stack_top,len]);
                }

                Ok(OpCode::NegInt) => {
                    self.ip += 1;

                    let a = to_num!([&self.stack,self.stack_top] => i64);
                    push!( &to_bytes!(-a => i64)     => self.stack,[self.stack_top,8]);
                }
                Ok(OpCode::NegFloat) => {
                    self.ip += 1;

                    let a = to_num!([&self.stack,self.stack_top] => f64);
                    push!( &to_bytes!(-a => f64)     => self.stack,[self.stack_top,8]);
                }

                Ok(OpCode::AddInt) => binary_op!(+,self,i64),
                Ok(OpCode::DivideInt) => binary_op!(/,self,i64),
                Ok(OpCode::MultiplyInt) => binary_op!(*,self,i64),
                Ok(OpCode::SubtractInt) => binary_op!(-,self,i64),
                Ok(OpCode::AddFloat) => binary_op!(+,self,f64),
                Ok(OpCode::DivideFloat) => binary_op!(/,self,f64),
                Ok(OpCode::MultiplyFloat) => binary_op!(*,self,f64),
                Ok(OpCode::SubtractFloat) => binary_op!(-,self,f64),
                Ok(OpCode::IntNeq) => cmp_op!(!=,self,bool),
                Ok(OpCode::IntEq) => cmp_op!(==,self,bool),
                Ok(OpCode::IntLt) => cmp_op!(<,self,bool),
                Ok(OpCode::IntLtE) => cmp_op!(<=,self,bool),
                Ok(OpCode::IntGt) => cmp_op!(>,self,bool),
                Ok(OpCode::IntGtE) => cmp_op!(>=,self,bool),
                Ok(OpCode::FloatNeq) => cmp_op!(!=,self,bool),
                Ok(OpCode::FloatEq) => cmp_op!(==,self,bool),
                Ok(OpCode::FloatLt) => cmp_op!(<,self,bool),
                Ok(OpCode::FloatLtE) => cmp_op!(<=,self,bool),
                Ok(OpCode::FloatGt) => cmp_op!(>,self,bool),
                Ok(OpCode::FloatGtE) => cmp_op!(>=,self,bool),

                Ok(ref e) => unimplemented!("{:?}", e),
                Err(_) => {
                    println!("{:?}", self.code[self.ip]);
                    self.reset_stack();
                    return Err(VMError::RuntimeError);
                }
            }
        }
    }
}
