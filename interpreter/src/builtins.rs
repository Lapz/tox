use std::time::{SystemTime, UNIX_EPOCH};
use object::Object;
use interpreter::RuntimeError;
use rand::{thread_rng, Rng};
use util::symbol::Symbol;
use util::env::TypeEnv;

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

    pub fn get_built_ins(&self, env: &mut TypeEnv) -> Vec<(Symbol, Object)> {
        vec![
            add_builtin(env.get_symbol("clock"), built_in_clock),
            add_builtin(env.get_symbol("random"), built_in_rand),
            add_builtin(env.get_symbol("oct"), built_in_oct),
            add_builtin(env.get_symbol("hex"), built_in_hex),
            add_builtin(env.get_symbol("to_int"), built_in_to_int),
            add_builtin_class(
                env.get_symbol("io"),
                env,
                vec![("readline", built_in_readline)],
            ),
        ]
    }
}

pub(crate) fn add_builtin(name: Symbol, func: BuiltInFunction) -> (Symbol, Object) {
    (name, Object::BuiltIn(name, func))
}

fn add_builtin_class(
    name: Symbol,
    env: &mut TypeEnv,
    methods: Vec<(&str, BuiltInFunction)>,
) -> (Symbol, Object) {
    use std::collections::HashMap;

    let mut map = HashMap::new();

    for method in methods {
        let name = env.get_symbol(method.0);
        map.insert(name, Object::BuiltIn(name, method.1));
    }

    (name, Object::Class(name, None, map))
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

fn built_in_to_int(arguments: &[Object]) -> Result<Object, RuntimeError> {
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
