macro_rules! op {
    ($($body:tt)*) =>   {
        op_inner! {
            #[derive(Clone, Copy, Debug)]
            pub enum OpCode {
                $($body)*
            }
        }
    }
}

macro_rules! op_inner {
    ($i:item) => {
        $i
    };
}

op! {
    Return,
    Int,
    Float,
    String,
    Nil,
    NegInt,
    AddInt,
    SubtractInt ,
    MultiplyInt ,
    DivideInt,
    NegFloat,
    AddFloat,
    SubtractFloat ,
    MultiplyFloat,
    DivideFloat,
    IntNeq,
    IntEq,
    IntLt,
    IntLtE,
    IntGt,
    IntGtE,
    FloatNeq,
    FloatEq,
    FloatLt,
    FloatLtE,
    FloatGt,
    FloatGtE,
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
        use self::OpCode::*;
        Ok(match original {
            0 => Return,
            1 => Int,
            2 => Float,
            3 => String,
            4 => Nil,
            5 => NegInt,
            6 => AddInt,
            7 => SubtractInt,
            8 => MultiplyInt,
            9 => DivideInt,
            10 => NegFloat,
            11 => AddFloat,
            12 => SubtractFloat,
            13 => MultiplyFloat,
            14 => DivideFloat,
            15 => IntNeq,
            16 => IntEq,
            17 => IntLt,
            18 => IntLtE,
            19 => IntGt,
            20 => IntGtE,
            21 => FloatNeq,
            22 => FloatEq,
            23 => FloatLt,
            24 => FloatLtE,
            25 => FloatGt,
            26 => FloatGtE,
            _ => return Err(()),
        })
    }
}
