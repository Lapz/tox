use std::time::{SystemTime, UNIX_EPOCH};
use rand::{thread_rng, Rng};

use env::{Entry, Env};
use types::{BaseType, Type};
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
        self.add_builtin(
            "clock",
            vec![],
            Type::Simple(BaseType::Float),
            built_in_clock,
        );
        self.add_builtin(
            "hex",
            vec![Type::Simple(BaseType::Int)],
            Type::Simple(BaseType::Str),
            built_in_hex,
        );
        self.add_builtin(
            "oct",
            vec![Type::Simple(BaseType::Int)],
            Type::Simple(BaseType::Str),
            built_in_oct,
        );
        self.add_builtin(
            "rand",
            vec![Type::Simple(BaseType::Int), Type::Simple(BaseType::Int)],
            Type::Simple(BaseType::Int),
            built_in_rand,
        );
        self.add_builtin(
            "int",
            vec![Type::Simple(BaseType::Str)],
            Type::Simple(BaseType::Int),
            built_in_int,
        );
        self.add_builtin_class(
            "io",
            vec![
                (
                    "readline",
                    built_in_readline,
                    Entry::FunEntry {
                        params: vec![],
                        returns: Type::Simple(BaseType::Str),
                    },
                ),
            ],
        )
    }

    fn add_builtin_class(&mut self, name: &str, methods: Vec<(&str, BuiltInFunction, Entry)>) {
        let symbol = self.vars.symbol(name);

        use std::collections::HashMap;

        let mut methods_ty = HashMap::new();
        let mut map = HashMap::new();

        for method in methods {
            let name = self.vars.symbol(method.0);
            methods_ty.insert(name, method.2);
            map.insert(name, Object::BuiltIn(name, method.1));
        }

        let entry = Entry::VarEntry(Type::Class {
            name: symbol,
            methods: methods_ty,
            fields: HashMap::new(),
        });

        let object = Object::Class(symbol, None, map);

        self.vars.enter(symbol, entry);
        self.objects.enter(symbol, object);
    }

    fn add_builtin(
        &mut self,
        name: &str,
        params: Vec<::types::Type>,
        returns: Type,
        func: BuiltInFunction,
    ) {
        let symbol = self.vars.symbol(name);
        self.vars.enter(symbol, Entry::FunEntry { params, returns });
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

fn built_in_int(arguments: &[Object]) -> Result<Object, RuntimeError> {
    let string = match arguments.iter().next() {
        Some(&Object::Str(ref s)) => s,
        _ => unreachable!(),
    };

    match string.trim().parse::<i64>() {
        Ok(n) => Ok(Object::Int(n)),
        Err(_) => Err(RuntimeError::CantParseAsInt),
    }
}

fn built_in_readline(_: &[Object]) -> Result<Object, RuntimeError> {
    let mut input = String::new();

    use std::io;

    io::stdin()
        .read_line(&mut input)
        .expect("Unable to read input from stdin");

    Ok(Object::Str(input))
}
