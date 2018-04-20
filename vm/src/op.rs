
#[derive(Clone, Copy, Debug)]
pub enum OpCode {
    /// Return 
    Return = 1,
    /// Ints
    Constant = 2,
    /// Floats
    ConstantLong = 3,
}

pub trait TryFrom<T>: Sized {
    /// The type returned in the event of a conversion error.
    type Error;

    /// Performs the conversion.
    fn try_from(T) -> Result<Self, Self::Error>;
}

impl Into<u8> for OpCode {
    fn into(self) -> u8 {
        self as u8
    }
}


impl TryFrom<u8> for OpCode {
    type Error = ();

    fn try_from(original: u8) -> Result<Self, Self::Error> {
        match original {
            1 => Ok(OpCode::Return),
            2 => Ok(OpCode::Constant),
            3 => Ok(OpCode::ConstantLong),
            _ => Err(()),
        }
    }
}
