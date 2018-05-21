//! This module provides the types that are used throughout tox for the typeChecking

use env::Entry;
use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};
use util::pos::Span;
use util::symbol::Symbol;

#[derive(Debug, Clone)]
pub enum Type {
    Array(Box<Type>),
    Alias(Symbol, Box<Type>),
    Class {
        name: Symbol,
        methods: HashMap<Symbol, Entry>,
        fields: HashMap<Symbol, Type>,
    },
    This {
        name: Symbol,
        fields: HashMap<Symbol, Type>,
        methods: HashMap<Symbol, Type>,
    },
    Fun(Vec<Type>, Box<Type>),
    Dict(Box<Type>, Box<Type>), // Key, Value
    Int,
    Str,
    Bool,
    Nil,
    Float,
}
