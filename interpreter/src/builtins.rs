use interpreter::{ErrorCode, RuntimeError};
use object::Object;
use rand::{thread_rng, Rng};
use std::str;
use std::time::{SystemTime, UNIX_EPOCH};
use util::symbol::Symbol;
use util::symbol::Symbols;

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

    pub fn get_built_ins(&self, env: &mut Symbols<()>) -> Vec<(Symbol, Object)> {
        vec![
            add_builtin(env.symbol("clock"), built_in_clock),
            add_builtin(env.symbol("random"), built_in_rand),
            add_builtin(env.symbol("oct"), built_in_oct),
            add_builtin(env.symbol("hex"), built_in_hex),
            add_builtin(env.symbol("to_int"), built_in_to_int),
            add_builtin(env.symbol("trim"), built_in_trim),
            add_builtin(env.symbol("char_at"), built_in_char_at),
            add_builtin(env.symbol("is_digit"), built_in_is_digit),
            add_builtin_class(env.symbol("io"), env, vec![("readline", built_in_readline)]),
        ]
    }
}

pub(crate) fn add_builtin(name: Symbol, func: BuiltInFunction) -> (Symbol, Object) {
    (name, Object::BuiltIn(name, func))
}

fn add_builtin_class(
    name: Symbol,
    env: &mut Symbols<()>,
    methods: Vec<(&str, BuiltInFunction)>,
) -> (Symbol, Object) {
    use fnv::FnvHashMap;
    let mut map = FnvHashMap::default();
    for method in methods {
        let name = env.symbol(method.0);
        map.insert(name, Object::BuiltIn(name, method.1));
    }
    (name, Object::Class(name, None, map))
}

fn built_in_clock(_: &[Object]) -> Result<Object, RuntimeError> {
    let time = SystemTime::now().duration_since(UNIX_EPOCH).unwrap();
    Ok(Object::Float(
        time.as_secs() as f64 + f64::from(time.subsec_nanos()) * 1e-9,
    ))
}

fn built_in_hex(arguments: &[Object]) -> Result<Object, RuntimeError> {
    let number = match arguments.iter().next() {
        Some(&Object::Int(a)) => a,
        _ => unreachable!(),
    };
    Ok(Object::Str(format!("{:#X}", number).as_bytes().to_vec()))
}

fn built_in_char_at(arguments: &[Object]) -> Result<Object, RuntimeError> {
    let mut arguments = arguments.into_iter();
    let input = match arguments.next() {
        Some(&Object::Str(ref a)) => a,
        _ => unreachable!(),
    };

    let pos = match arguments.next() {
        Some(&Object::Int(a)) => a,
        _ => unreachable!(),
    };

    Ok(Object::Str(vec![input[pos as usize], b'\0']))
}

fn built_in_is_digit(arguments: &[Object]) -> Result<Object, RuntimeError> {
    let mut arguments = arguments.into_iter();
    let input = match arguments.next() {
        Some(&Object::Str(ref a)) => a,
        _ => unreachable!(),
    };

    Ok(Object::Bool((input[0] as char).is_numeric()))
}
fn built_in_oct(arguments: &[Object]) -> Result<Object, RuntimeError> {
    let number = match arguments.iter().next() {
        Some(&Object::Int(a)) => a,
        _ => unreachable!(),
    };
    Ok(Object::Str(format!("{:#o}", number).as_bytes().to_vec()))
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

fn built_in_trim(arguments: &[Object]) -> Result<Object, RuntimeError> {
    let string = match arguments.iter().next() {
        Some(&Object::Str(ref s)) => s.clone(),
        _ => unreachable!(),
    };

    Ok(Object::Str(
        str::from_utf8(&string).unwrap().trim().as_bytes().to_vec(),
    ))
}

fn built_in_to_int(arguments: &[Object]) -> Result<Object, RuntimeError> {
    let string = match arguments.iter().next() {
        Some(&Object::Str(ref s)) => s,
        _ => unreachable!(),
    };

    match str::from_utf8(string)
        .unwrap()
        .trim_matches(char::from(0))
        .parse::<i64>()
    {
        Ok(n) => Ok(Object::Int(n)),
        Err(_) => Err(RuntimeError::new_without_span(ErrorCode::CantParseAsInt(
            string.to_vec(),
        ))),
    }
}

fn built_in_readline(_: &[Object]) -> Result<Object, RuntimeError> {
    let mut input = String::new();
    use std::io;
    io::stdin()
        .read_line(&mut input)
        .expect("Unable to read input from stdin");
    Ok(Object::Str(input.as_bytes().to_vec()))
}
