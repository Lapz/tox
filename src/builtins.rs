use std::time::{SystemTime, UNIX_EPOCH};
use rand::{thread_rng, Rng};

use env::{Entry, Env};
use types::Type;
use object::Object;
use interpreter::RuntimeError;

pub type BuiltInFunction = fn(&[Object]) -> Result<Object, RuntimeError>;

pub struct BuiltIn;

impl Default for BuiltIn {
    fn default() -> Self {
        Self::new()
    }
}

impl BuiltIn {
    pub fn new() -> Self {
        BuiltIn {}
    }
}

impl Env {
    pub fn get_builtins(&mut self) {
        self.add_builtin("clock", vec![], Type::Float, built_in_clock);
        self.add_builtin("hex", vec![Type::Int], Type::Str, built_in_hex);
        self.add_builtin("oct", vec![Type::Int], Type::Str, built_in_oct);
        self.add_builtin("rand", vec![Type::Int, Type::Int], Type::Int, built_in_rand);
      
    }

    fn add_builtin(&mut self, name: &str, params: Vec<Type>, returns: Type, func: BuiltInFunction) {
        let symbol = self.vars.symbol(name);
        let entry = Entry::FunEntry { params, returns };
        self.vars.enter(symbol, entry);
        self.add_object(symbol, Object::BuiltIn(symbol, func));
    }
}

fn built_in_clock(_: &[Object]) -> Result<Object, RuntimeError> {
    let time = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_secs();

    Ok(Object::Int(time as i64))
}

fn built_in_hex(arguments: &[Object]) -> Result<Object, RuntimeError> {
    let number = match arguments.iter().next() {
        Some(&Object::Int(a)) => a,
        _ => unreachable!(),
    };

    Ok(Object::Str(format!("{:#X}", number)))
}

fn built_in_oct(arguments: &[Object]) -> Result<Object, RuntimeError> {
    let number = match arguments.iter().next() {
        Some(&Object::Int(a)) => a,
        _ => unreachable!(),
    };

    Ok(Object::Str(format!("{:#o}", number)))
}

fn built_in_rand(arguments: &[Object]) -> Result<Object, RuntimeError> {
    let mut arguments = arguments.into_iter();
    let min = match arguments.next() {
        Some(&Object::Int(n)) => n,

        _ => unreachable!(),
    };

    let max = match arguments.next() {
        Some(&Object::Int(n)) => n,
        _ => unreachable!(),
    };

    let mut rng = thread_rng();

    Ok(Object::Int(rng.gen_range(min, max)))
}

fn built_in_readline(_: &[Object]) -> Result<Object,RuntimeError> {
    let mut input = String::new();
    
    use std::io;
    
     io::stdin().read_line(&mut input).expect("Unable to read input from stdin");
     
    Ok(Object::Str(input))
    
}

use symbol::Symbol;

lazy_static!{
    pub static ref IO:Object = Object::Class(Symbol(2),None,hashmap!{
        Symbol(3) => Object::BuiltIn(Symbol(3),built_in_readline)
    });
}
