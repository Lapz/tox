//! This module provides the types that are used throughout tox for the typeChecking

use ctx::CompileCtx;
use env::Entry;
use std::collections::HashMap;
use util::symbol::Symbol;

static mut UNIQUE_COUNT: u32 = 0;

#[derive(Clone, Copy, Debug, PartialEq, Default)]
pub struct Unique(pub u32);

impl Unique {
    pub fn new() -> Self {
        let value = unsafe { UNIQUE_COUNT };
        unsafe { UNIQUE_COUNT += 1 };
        Unique(value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Array(Box<Type>),
    Alias(Symbol, Box<Type>),
    Class(
        Symbol,
        HashMap<Symbol, Type>,
        HashMap<Symbol, Entry>,
        Unique,
    ),
    This {
        name: Symbol,
        fields: HashMap<Symbol, Type>,
        methods: HashMap<Symbol, Entry>,
    },
    Fun(Vec<Type>, Box<Type>),
    Dict(Box<Type>, Box<Type>), // Key, Value
    Int,
    Str,
    Bool,
    Nil,
    Float,
}

impl Type {
    pub fn is_int(&self) -> bool {
        match *self {
            Type::Int => true,
            _ => false,
        }
    }
    pub fn print(&self, ctx: &CompileCtx) -> String {
        match *self {
            Type::Alias(ref name, ref ty) => format!("Type alias {} = {}", name, ty.print(ctx)),
            Type::Class(ref name, ref methods, ref fields, _) => {
                let mut fmt_string = format!("Class {} {{", ctx.name(*name));

                for (i, field) in fields.iter().enumerate() {
                    if i + 1 == fields.len() {
                        fmt_string.push_str(&ctx.name(*field.0))
                    } else {
                        fmt_string.push_str(&format!("{},", ctx.name(*field.0)))
                    }
                }

                for (i, method) in methods.iter().enumerate() {
                    if i + 1 == fields.len() {
                        fmt_string.push_str(&format!("{}", ctx.name(*method.0)))
                    } else {
                        fmt_string.push_str(&format!(",{}", ctx.name(*method.0)))
                    }
                }

                fmt_string.push('}');

                fmt_string
            }

            Type::This {
                ref name,
                ref methods,
                ref fields,
            } => {
                let mut fmt_string = format!("This {} {{ ", ctx.name(*name));

                for (i, field) in fields.iter().enumerate() {
                    if i + 1 == fields.len() {
                        fmt_string.push_str(&format!("{}", &ctx.name(*field.0)))          
                    } else {
                        fmt_string.push_str(&format!("{},", ctx.name(*field.0)))
                    }
                }


                fmt_string.push(',');

                for (i, method) in methods.iter().enumerate() {
                    if i + 1 == fields.len() {
                        fmt_string.push_str(&format!("{}", ctx.name(*method.0)))
                    } else {
                        fmt_string.push_str(&format!(",{}", ctx.name(*method.0)))
                    }
                }

                fmt_string.push('}');

                fmt_string
            }
            Type::Fun(ref params, ref returns) => {
                let mut fmt_string = String::from("fn(");

                for (i, param) in params.iter().enumerate() {
                    if i + 1 == params.len() {
                        fmt_string.push_str(&format!("{}", param.print(ctx)))
                    } else {
                        fmt_string.push_str(&format!("{},", param.print(ctx)))
                    }
                }

                fmt_string.push_str(") -> ");

                fmt_string.push_str(&format!("{}", returns.print(ctx)));

                fmt_string
            }
            Type::Dict(ref key, ref value) => {
                format!("Dictionary<{},{}>", key.print(ctx), value.print(ctx))
            }
            Type::Array(ref a) => format!("Array of {} ", a.print(ctx)),
            Type::Int => format!("Int"),
            Type::Str => format!("Str"),
            Type::Bool => format!("Boolean"),
            Type::Nil => format!("Nil"),
            Type::Float => format!("Float"),
        }
    }
}
