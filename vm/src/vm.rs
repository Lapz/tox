use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use chunks::Chunk;
use op::{OpCode, TryFrom};
use std::io::{Cursor, Read};

macro_rules! debug {
    ($($p:tt)*) => (if cfg!(feature = "debug") { println!($($p)*) } else { })
}


pub struct VM {
    chunk: Chunk,
    stack:[u8;256],
    stack_top:usize,
    ip: usize,
}

type VMResult = Result<(),VMError>; 

#[derive(Debug)]
pub enum VMError {
    CompilerError,
    RuntimeError,
}

impl VM {
    pub fn new(chunk: Chunk) -> Self {
        VM { ip: 0,stack_top:1,stack:[0;256],chunk }
    }
    pub fn interpert(&mut self) ->VMResult {
        self.run()?;
        Ok(())
    }

    fn run(&mut self) ->VMResult {
        loop { 

          
                // debug!("{:?}",self.chunk.dissassemble("run"));
            
            
           

            match OpCode::try_from(self.chunk.code[self.ip]) {
                Ok(OpCode::Return) => return Ok(()),
                Ok(OpCode::Constant) => {
                    let start = self.ip + 1 as usize;
                    let end = self.ip + 2 as usize;
                    let mut rdr = Cursor::new(&self.chunk.constants[start..end]);

                    let constant = rdr.read_i64::<LittleEndian>().unwrap();
                    println!("a {:?}", constant);

                    break;
                }
                Ok(OpCode::ConstantLong) => {

                    
                    let start = self.chunk.code[self.ip + 1 ] as usize;
                    let end = self.chunk.code[self.ip + 2] as usize;
                    let mut rdr = Cursor::new(&self.chunk.constants[start..end]);

                    let constant = rdr.read_f64::<LittleEndian>().unwrap();
                    println!("{:?}", constant);

                    

                    break;
                }
                Err(_) => {
                    return Err(VMError::RuntimeError)
                },
            }

            

        }
       Ok(())
    }


    fn push_bytes(&mut self,bytes:&[u8]) {}

    
}
