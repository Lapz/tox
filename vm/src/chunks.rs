use util::pos::Span;
use op::{OpCode,TryFrom};
use value::Value;
use byteorder::{LittleEndian, WriteBytesExt,ReadBytesExt};
use std::io::{Read,Cursor};
use pos::LineStart;


#[derive(Default,Debug)]
pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<u8>,
    lines: Vec<LineStart>,
}

#[derive(Debug)]
enum Type {
    Long,
    Constant,
}


#[cfg(feature = "debug")]
pub fn simple_instruction(name: &str, offset: usize) -> usize {
    println!("{}", name);
    offset + 1
}

impl Chunk {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn write<T: Into<u8>>(&mut self, byte: T, line:u32) {
    
        self.code.push(byte.into());

        let count = self.lines.len() - 1;
     
        if self.lines.len() > 0 && self.lines[count].line == line {
            return ;
        } else {
            let count = self.code.len() - 1;
            self.lines.push(LineStart {
                line,
                offset: count,
            })
        }
    }

    pub fn write_constant(&mut self, bytes:&[u8],ty:Type,line:u32) {
        let start = self.constants.len();
        let index = self.add_constant(bytes, line);

       if let Type::Constant = ty {
           self.write(OpCode::Constant, line);
           self.write(start as u8, line);
            self.write(index as u8, line);
          
       }else {
            self.write(OpCode::ConstantLong, line);
            self.write(start as u8, line);
            self.write(index as u8, line);
       }
    }
    /// Add a constant and returns the index where it was appended
    ///so that we can locate that same constant later.
    pub fn add_constant(&mut self, bytes:&[u8],line:u32) -> usize {
        self.constants.extend(bytes);
        let count = self.code.len();
        self.lines.push(LineStart {
            line,
            offset:count,
        });
        self.constants.len()
    }

    #[cfg(feature = "debug")]
    pub fn dissassemble(&mut self, name: &str) {
        println!("== {} ==", name);

        let mut i = 0;
        while i < self.code.len() {
            i = self.dissassemble_instruction(i);
        }
    }

    #[cfg(feature = "debug")]
    pub fn get_line(&self,instruction:u32) -> u32 {
        let mut start = 0;
        let mut end = self.lines.len() - 1;

        loop {
            let mid = (start + end)/2;

            let line = self.lines[mid];

            if instruction < line.offset as u32 {
                end = mid -1;
            } else if mid == self.lines.len() -1 || instruction < self.lines[mid+1].offset as u32 {
                return line.line
            }
            else {
                start = mid +1;
            }

        }
    }

    #[cfg(feature = "debug")]
    pub fn dissassemble_instruction(&self, offset: usize) -> usize {
        print!("{:04} ", offset);

        let line = self.get_line(offset as u32);

        if offset > 0 && line == self.get_line((offset-1) as u32) {
            print!("   | ");
        } else {
            print!("{:04} ",self.get_line(offset as u32));
        }

        let instruction = self.code[offset];

        match OpCode::try_from(instruction) {
            Ok(OpCode::Return) => simple_instruction("OP_RETURN", offset),
            Ok(OpCode::Constant) => self.constant_instruction("OP_CONSTANT", offset as usize),
            Ok(OpCode::ConstantLong) => self.long_constant_instruction("OP_CONSTANTLONG", offset as usize),
            _ => {
                println!("Unknown opcode {}", instruction);
                offset + 1
            }
        }
    }

    #[cfg(feature = "debug")]
    /// Pulls out the `OpCode::Constant` and prints that out
    /// Matches on the instruction and uses that pointer offset for
    /// were the value is stored
    pub fn constant_instruction(&self, name: &str, offset: usize) -> usize {
        let start = self.code[offset+1] as usize;
        let end = self.code[offset+2] as usize; 
        let mut rdr = Cursor::new(&self.constants[start..end]);

        let constant = rdr.read_i64::<LittleEndian>().unwrap();
        
        println!(
            "{:16} {:04}",
            name, constant,
        );

        offset as usize + 3
    }

    #[cfg(feature = "debug")]
    /// Pulls out the `OpCode::LongConstant` and prints that out
    /// Matches on the instruction and uses that pointer offset for
    /// were the value is stored
    pub fn long_constant_instruction(&self, name: &str, offset: usize) -> usize {
       let start = self.code[offset+1] as usize;
       let end = self.code[offset+2] as usize; 
        let mut rdr = Cursor::new(&self.constants[start..end]);

        let constant = rdr.read_f64::<LittleEndian>().unwrap();
        println!(
            "{:16} {:04}",
            name, constant,
        );

       offset as usize + 3
        
    }


}

#[cfg(test)]
#[cfg(feature = "debug")]
mod tests {

    use util::pos::{Position, Span};
    use super::{Chunk, OpCode,Value,Type};

    #[test]
    fn it_works() {
        let mut chunk = Chunk::new();

        let constant = chunk.write_constant(&unsafe {
            use std::mem;
            mem::transmute::<f64,[u8;mem::size_of::<f64>()]>(1.23)
        },Type::Long,1);
    

         let constant = chunk.write_constant(&unsafe {
            use std::mem;
            mem::transmute::<f64,[u8;mem::size_of::<f64>()]>(12.3)
        },Type::Long,1);


         let constant = chunk.write_constant(&unsafe {
            use std::mem;
            mem::transmute::<i64,[u8;mem::size_of::<f64>()]>(123)
        },Type::Constant,2);
    
      

        chunk.write(
            OpCode::Return,
            3,
        );

       
        chunk.dissassemble("test chunk");
    }
}
