use object::Object
use ast::expr::*;
pub struct Interpreter;

impl Interpreter {
    fn interpret(&self) -> Result<Object,RuntimeError> {
        
    }

    fn evaluate_literal(&self, expression:&Literal) -> Result<Object,RuntimeError> {
        match *expression {
            Literal::Float(i) =>Ok(Object::FLoat(i)),
            Literal::Str(ref s) => OK(Object::Str(s.to))
        }
    }

