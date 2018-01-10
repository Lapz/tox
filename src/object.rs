use std::ops::Not;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::cmp::{Ordering, PartialOrd};
use std::fmt::{Display, Formatter};
use std::fmt;
use symbol::Symbol;
use ast::statement::Statement;
use interpreter::RuntimeError;
use env::Env;
use std::rc::Rc;
use std::cell::RefCell;
use builtins::BuiltInFunction;

#[derive(Clone)]
pub enum Object {
    Float(f64),
    BuiltIn(Symbol, BuiltInFunction),
    Class(Symbol, Option<Box<Object>>, HashMap<Symbol, Object>),
    Int(i64),
    Str(String),
    Bool(bool),

    Return(Box<Object>),
    Array(Vec<Object>),
    Dict(HashMap<Object, Object>),
    Function(Symbol, Vec<Symbol>, Statement),
    Instance {
        methods: HashMap<Symbol, Object>,
        fields: Rc<RefCell<HashMap<Symbol, Object>>>,
        sclassmethods: Option<HashMap<Symbol, Object>>,
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
            _ => unimplemented!(),
        }
    }

    pub fn bind(&self, instance: &Object, env: &mut Env) -> Object {
        match *self {
            ref method @ Object::Function(_, _, _) | ref method @ Object::BuiltIn(_, _) => {
                env.begin_scope();

                env.add_object(Symbol(0), instance.clone());

                method.clone()
            }

            _ => unreachable!(),
        }
    }

    pub fn get_property(&self, name: &Symbol, env: &mut Env) -> Result<Object, RuntimeError> {
        match *self {
            Object::Instance {
                ref fields,
                ref methods,
                ref sclassmethods,
            } => {
                if fields.borrow().contains_key(name) {
                    return Ok(fields.borrow().get(name).unwrap().clone());
                }

                if methods.contains_key(name) {
                    return Ok(methods.get(name).unwrap().bind(self, env));
                }

                if let Some(ref smethods) = *sclassmethods {
                    if smethods.contains_key(name) {
                        return Ok(smethods.get(name).unwrap().bind(self, env));
                    }
                }

                Err(RuntimeError::UndefinedProperty)
            }

            Object::Class(_, ref superclass, ref methods) => {
                if methods.contains_key(name) {
                    return Ok(methods.get(name).unwrap().bind(self, env));
                } else if superclass.is_some() {
                    return superclass.clone().unwrap().get_property(name, env);
                }
                Err(RuntimeError::UndefinedProperty)
            }
            _ => unimplemented!(),
        }
    }

    pub fn call(&self, arguments: &[Object], env: &mut Env) -> Result<Object, RuntimeError> {
        match *self {
            Object::BuiltIn(_, builtin_fn) => builtin_fn(arguments),
            Object::Function(_, ref params, ref body) => {
                env.begin_scope();

                let zipped = params.iter().zip(arguments.iter());

                for (_, (symbol, value)) in zipped.enumerate() {
                    env.add_object(*symbol, value.clone());
                }

                use interpreter::evaluate_statement;

                match *body {
                    Statement::Block(ref statements) => for statement in statements {
                        match evaluate_statement(statement, env) {
                            Ok(val) => if let Object::Return(r) = val {
                                env.end_scope();
                                return Ok(*r);
                            },

                            Err(e) => return Err(e),
                        }
                    },
                    _ => unreachable!(),
                }

                env.end_scope();

                Ok(Object::Nil)
            }

            _ => panic!("Should not be calling this method"),
        }
    }

    pub fn as_string(&self) -> String {
        match *self {
            Object::Instance { .. } => "instance".into(),
            Object::BuiltIn(ref s, _) => format!("fn <builtin<{}>", s),
            Object::Function(ref s, _, _) => format!("fn <{}> ", s),
            Object::Class(ref name, _, _) => format!("class <{}>", name),
            Object::Float(f) => f.to_string(),
            Object::None => "None".into(),
            Object::Return(ref val) => val.as_string(),
            Object::Dict(ref hashmap) => {
                let mut fmt_string = String::new();
                fmt_string.push_str("{");
                for (i, (k, v)) in hashmap.iter().enumerate() {
                    fmt_string.push_str(format!("{} : {}", k, v).as_str());
                    if i < hashmap.len() - 1 {
                        fmt_string.push_str(", ");
                    }
                }
                fmt_string.push_str("}");
                fmt_string
            }

            Object::Int(b) => b.to_string(),
            Object::Bool(b) => b.to_string(),
            Object::Nil => "nil".to_string(),
            Object::Str(ref s) => s.clone(),
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

            (&Object::Dict(ref x), &Object::Dict(ref y)) => x == y,
            (&Object::Function(ref x, _, _), &Object::Function(ref y, _, _))
            | (&Object::Class(ref x, _, _), &Object::Class(ref y, _, _))
            | (&Object::BuiltIn(ref x, _), &Object::BuiltIn(ref y, _)) => x == y,
            (&Object::Nil, &Object::Nil) | (&Object::None, &Object::None) => true,
            (&Object::Int(ref x), &Object::Int(ref y)) => x == y,
            (&Object::Float(ref x), &Object::Float(ref y)) => x == y,
            (&Object::Return(ref x), &Object::Return(ref y)) => x == y,
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
            Object::Return(ref r) => write!(f, "return {}", r),
            Object::None => write!(f, "none"),
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
            Object::Function(ref s, _, _) => write!(f, "fn <{}> ", s),
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

            Object::Dict(ref hashmap) => {
                let mut fmt_string = String::new();
                fmt_string.push_str("{");
                for (i, (k, v)) in hashmap.iter().enumerate() {
                    fmt_string.push_str(format!("{} : {}", k, v).as_str());
                    if i < hashmap.len() - 1 {
                        fmt_string.push_str(", ");
                    }
                }
                fmt_string.push_str("}");
                write!(f, "{}", fmt_string)
            }

            Object::Nil => write!(f, "nil"),

            Object::Str(ref s) => write!(f, "{}", s),
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
            (&Object::Return(ref s), &Object::Return(ref o)) => (s.partial_cmp(o)),
            (&Object::Array(ref a), &Object::Array(ref o)) => (a.partial_cmp(o)),
            (&Object::Dict(ref d), &Object::Dict(ref o)) => (d.iter().partial_cmp(o.iter())),
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
            Object::Function(ref s, _, _) => write!(f, "fn <{}>", s),
            Object::BuiltIn(ref s, _) => write!(f, "fn <builtin{}>", s),
            Object::None => write!(f, "none"),
            Object::Return(ref r) => write!(f, "return {}", r),
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

            Object::Str(ref s) => write!(f, "{}", s.clone()),

            Object::Dict(ref hashmap) => {
                let mut fmt_string = String::new();
                fmt_string.push_str("{");
                for (i, (k, v)) in hashmap.iter().enumerate() {
                    fmt_string.push_str(format!("{} : {}", k, v).as_str());
                    if i < hashmap.len() - 1 {
                        fmt_string.push_str(", ");
                    }
                }
                fmt_string.push_str("}");
                write!(f, "{}", fmt_string)
            }
        }
    }
}
