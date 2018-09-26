use super::interpreter::{RuntimeError,ErrorCode};
use builtins::BuiltInFunction;
use fnv::FnvHashMap;
use interpreter::env::Environment;
use std::cell::RefCell;
use std::cmp::{Ordering, PartialOrd};
use std::fmt::{self, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::Not;
use std::rc::Rc;
use std::str;
use syntax::ast::Statement;
use util::pos::Spanned;
use util::symbol::Symbol;

#[derive(Clone)]
pub enum Object {
    Float(f64),
    BuiltIn(Symbol, BuiltInFunction),
    Class(Symbol, Option<Box<Object>>, FnvHashMap<Symbol, Object>),
    Int(i64),
    Str(Vec<u8>),
    Bool(bool),
    Array(Vec<Object>),
    Function(
        Symbol,
        Vec<Symbol>,
        Box<Spanned<Statement>>,
        Box<Environment>,
    ),
    Instance {
        methods: FnvHashMap<Symbol, Object>,
        fields: Rc<RefCell<FnvHashMap<Symbol, Object>>>,
        sclassmethods: Option<FnvHashMap<Symbol, Object>>,
    },
    Nil,
    None,
}

unsafe impl Sync for Object {}
unsafe impl Send for Object {}

impl Object {
    pub fn is_truthy(&self) -> bool {
        match *self {
            Object::Nil => false,
            Object::Bool(b) => b,
            _ => true,
        }
    }

    pub fn set(&mut self, name: Symbol, value: &Object) {
        match *self {
            Object::Instance { ref mut fields, .. } => {
                fields.borrow_mut().insert(name, value.clone());
            }
            _ => unreachable!(), // Type checking means no dynamically adding fields
        }
    }

    pub fn bind(&self, instance: &Object) -> Object {
        match *self {
            Object::Function(ref name, ref param, ref body, ref fn_env) => {
                let mut environment = Environment::new_with_outer(fn_env);
                environment.define(Symbol(0), instance.clone());
                Object::Function(*name, param.clone(), body.clone(), Box::new(environment))
            }
            ref value @ Object::BuiltIn(_, _) => value.clone(),

            _ => unreachable!(),
        }
    }

    pub fn get_property(&self, name: &Spanned<Symbol>, env: &Environment) -> Result<Object, RuntimeError> {
        match *self {
            Object::Instance {
                ref fields,
                ref methods,
                ref sclassmethods,
            } => {
                if fields.borrow().contains_key(&name.value) {
                    return Ok(fields.borrow().get(&name.value).unwrap().clone());
                }
                if methods.contains_key(&name.value) {
                    return Ok(methods.get(&name.value).unwrap().bind(self));
                }
                if let Some(ref smethods) = *sclassmethods {
                    if smethods.contains_key(&name.value) {
                        return Ok(smethods.get(&name.value).unwrap().bind(self));
                    }
                }
                Err(RuntimeError::new(ErrorCode::UndefinedProperty(name.value),name.span))
            }

            Object::Class(_, ref superclass, ref methods) => {
                if methods.contains_key(&name.value) {
                    return Ok(methods.get(&name.value).unwrap().bind(self));
                } else if superclass.is_some() {
                    return superclass.clone().unwrap().get_property(name, env);
                }
                Err(RuntimeError::new(ErrorCode::UndefinedProperty(name.value),name.span))
            }
            _ => unreachable!(), // Type checking means no trying to access a property
                                 // that dosen't exist
        }
    }

    pub fn call(
        &self,
        arguments: &[Object],
    ) -> Result<Object, RuntimeError> {
        match *self {
            Object::BuiltIn(_, builtin_fn) => builtin_fn(arguments),
            Object::Function(_, ref params, ref body, ref fenv) => {
                let mut local_environment = Environment::new_with_outer(fenv);

                let zipped = params.iter().zip(arguments.iter());

                for symbol in params.iter() {
                    for arg in arguments.iter() {
                        local_environment.define(*symbol,arg.clone());
                    }
                }


//
//                for (_, (symbol, value)) in zipped.enumerate() {
//                    local_environment.define(*symbol, value.clone());
//                }

                use interpreter::evaluate_statement;

                match evaluate_statement(body, &mut local_environment) {
                    Ok(_) => (),
                    Err(e) => match e.code {
                        ErrorCode::Return(ref r) => {
                            return Ok(*r.clone());
                        }

                        _ => return Err(e),
                    },
                }

                Ok(Object::Nil)
            }
            ref e => panic!("{:?} Should not be calling this method ", e),
        }
    }

    pub fn as_string(&self) -> String {
        match *self {
            Object::Instance { .. } => "instance".into(),
            Object::BuiltIn(ref s, _) => format!("fn <builtin<{}>", s),
            Object::Function(ref s, _, _, _) => format!("fn <{}> ", s),
            Object::Class(ref name, _, _) => format!("class <{}>", name),
            Object::Float(f) => f.to_string(),
            Object::None => "None".into(),
            Object::Int(b) => b.to_string(),
            Object::Bool(b) => b.to_string(),
            Object::Nil => "nil".to_string(),
            Object::Str(ref s) => str::from_utf8(s).unwrap().into(),
            Object::Array(ref v) => {
                let mut fmt_string = String::new();
                fmt_string.push_str("[");
                for (i, o) in v.iter().enumerate() {
                    fmt_string.push_str(format!("{}", o).as_str());
                    if i < v.len() - 1 {
                        fmt_string.push_str(", ");
                    }
                }
                fmt_string.push_str("]");

                fmt_string
            }
        }
    }
}

impl Eq for Object {}

impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match *self {
            Object::Int(i) => i.hash(state),
            Object::Float(f) => {
                let hash = f as i64;
                hash.hash(state)
            }
            Object::Bool(ref b) => b.hash(state),
            Object::Str(ref s) => s.hash(state),
            Object::Nil => "nil".hash(state),
            _ => "".hash(state),
        }
    }
}

impl Not for Object {
    type Output = Object;

    fn not(self) -> Object {
        match self {
            Object::Bool(b) => Object::Bool(!b),
            _ => Object::Bool(false),
        }
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Object) -> bool {
        match (self, other) {
            (&Object::Array(ref x), &Object::Array(ref y)) => x == y,
            (&Object::Bool(ref x), &Object::Bool(ref y)) => x == y,
            (&Object::Function(ref x, _, _, _), &Object::Function(ref y, _, _, _))
            | (&Object::Class(ref x, _, _), &Object::Class(ref y, _, _))
            | (&Object::BuiltIn(ref x, _), &Object::BuiltIn(ref y, _)) => x == y,
            (&Object::Nil, &Object::Nil) | (&Object::None, &Object::None) => true,
            (&Object::Int(ref x), &Object::Int(ref y)) => x == y,
            (&Object::Float(ref x), &Object::Float(ref y)) => x == y,
            (&Object::Str(ref x), &Object::Str(ref y)) => x == y,
            _ => false,
        }
    }
}

impl fmt::Debug for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Object::Float(ref n) => write!(f, "{}", n.to_string()),
            Object::Int(ref n) => write!(f, "{}", n.to_string()),
            Object::Class(ref name, _, _) => write!(f, "class <{}>", name),
            Object::None => write!(f, "None"),
            Object::Instance { ref fields, .. } => {
                write!(f, "instance")?;

                let mut fmt_string = String::new();
                fmt_string.push_str("fields ");
                fmt_string.push_str("{");
                for (i, (k, v)) in fields.borrow().iter().enumerate() {
                    fmt_string.push_str(format!("{} : {}", k, v).as_str());
                    if i < fields.borrow().len() - 1 {
                        fmt_string.push_str(", ");
                    }
                }

                fmt_string.push_str("}");
                write!(f, "{}", fmt_string)
            }
            Object::Bool(ref b) => write!(f, "{}", b.to_string()),
            Object::Function(ref s, _, _, _) => write!(f, "fn <{}> ", s),
            Object::BuiltIn(ref v, _) => write!(f, "fn <builtin {}>", v),
            Object::Array(ref v) => {
                let mut fmt_string = String::new();
                fmt_string.push_str("[");
                for (i, o) in v.iter().enumerate() {
                    fmt_string.push_str(format!("{}", o).as_str());
                    if i < v.len() - 1 {
                        fmt_string.push_str(", ");
                    }
                }
                fmt_string.push_str("]");
                write!(f, "{}", fmt_string)
            }
            Object::Nil => write!(f, "nil"),
            Object::Str(ref s) => write!(f, "{}", str::from_utf8(s).unwrap()),
        }
    }
}

impl PartialOrd for Object {
    fn partial_cmp(&self, other: &Object) -> Option<Ordering> {
        match (self, other) {
            (&Object::Float(ref s), &Object::Float(ref o)) => (s.partial_cmp(o)),
            (&Object::Int(ref s), &Object::Int(ref o)) => (s.partial_cmp(o)),
            (&Object::Str(ref s), &Object::Str(ref o)) => (s.partial_cmp(o)),
            (&Object::Bool(ref s), &Object::Bool(ref o)) => (s.partial_cmp(o)),
            (&Object::Array(ref a), &Object::Array(ref o)) => (a.partial_cmp(o)),
            (&Object::Nil, &Object::Nil) | (&Object::None, &Object::None) => Some(Ordering::Equal),
            _ => None,
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            Object::Instance { .. } => write!(f, "instance"),
            Object::Class(ref name, _, _) => write!(f, "class <{}>", name),
            Object::Function(ref s, _, _, _) => write!(f, "fn <{}>", s),
            Object::BuiltIn(ref s, _) => write!(f, "fn <builtin{}>", s),
            Object::None => write!(f, "None"),
            Object::Float(ref n) => write!(f, "{}", n.to_string()),
            Object::Int(ref n) => write!(f, "{}", n.to_string()),
            Object::Bool(ref b) => write!(f, "{}", b.to_string()),
            Object::Array(ref v) => {
                let mut fmt_string = String::new();
                fmt_string.push_str("[");
                for (i, o) in v.iter().enumerate() {
                    fmt_string.push_str(format!("{}", o).as_str());
                    if i < v.len() - 1 {
                        fmt_string.push_str(", ");
                    }
                }
                fmt_string.push_str("]");
                write!(f, "{}", fmt_string)
            }
            Object::Nil => write!(f, "nil"),
            Object::Str(ref s) => write!(f, "{}", str::from_utf8(s).unwrap()),
        }
    }
}
