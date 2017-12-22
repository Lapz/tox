use types::Type;
use object::Object;
use symbol::{Symbol, SymbolFactory, Symbols};
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Entry {
    VarEntry(Type), // Vec of (Vec<MethodParam types>,Return Type)
    FunEntry { params: Vec<Type>, returns: Type },
}

#[derive(Debug, Clone)]
pub struct Env {
    pub types: Symbols<Type>,
    pub vars: Symbols<Entry>,
    pub objects: Symbols<Object>,
}

impl Env {
    pub fn new(strings: &Rc<SymbolFactory>) -> Self {
        let mut types = Symbols::new(Rc::clone(strings));
        let string_symbol = types.symbol("str");
        let int_symbol = types.symbol("int");
        let float_symbol = types.symbol("float");
        let nil_symbol = types.symbol("nil");
        let bool_symbol = types.symbol("bool");

        types.enter(int_symbol, Type::Int);
        types.enter(float_symbol, Type::Float);
        types.enter(bool_symbol, Type::Bool);
        types.enter(nil_symbol, Type::Nil);
        types.enter(string_symbol, Type::Str);

        Env {
            types,
            vars: Symbols::new(Rc::clone(strings)),
            objects:Symbols::new(Rc::clone(strings)),
        }
    }
    pub fn look_type(&mut self, symbol: Symbol) -> Option<&Type> {
        self.types.look(symbol)
    }

    pub fn look_var(&self, symbol: Symbol) -> Option<&Entry> {
        self.vars.look(symbol)
    }

    pub fn begin_scope(&mut self) {
        self.types.begin_scope();
        self.vars.begin_scope();
    }

    pub fn end_scope(&mut self) {
        self.types.end_scope();
        self.vars.end_scope();
    }

    pub fn add_type(&mut self, symbol: Symbol, data: Type) {
        self.types.enter(symbol, data)
    }

    pub fn add_var(&mut self, symbol: Symbol, data: Entry) {
        self.vars.enter(symbol, data)
    }
}
