use types::{BaseType, Type};
use object::Object;
use symbol::{Symbol, SymbolFactory, Table};
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub enum Entry {
    VarEntry(Type), // Vec of (Vec<MethodParam types>,Return Type)
    FunEntry { params: Vec<Type>, returns: Type },
}

static mut UNIQUE_COUNT: u64 = 0;

#[derive(Clone, Debug, PartialEq)]
pub struct Unique(u64);

impl Unique {
    pub fn new() -> Self {
        let value = unsafe { UNIQUE_COUNT };
        unsafe { UNIQUE_COUNT += 1 };
        Unique(value)
    }
}

#[derive(Debug, Clone)]
pub struct Env {
    pub types: Table<Symbol,Type>,
    pub vars: Table<Symbol,Entry>,
    pub objects: Table<Symbol,Object>,
    pub unique: Unique,
}

impl Env {
    pub fn new(strings: &Rc<SymbolFactory>) -> Self {
        let mut types = Table::new(Rc::clone(strings));
        let string_symbol = types.symbol("str");
        let int_symbol = types.symbol("int");
        let float_symbol = types.symbol("float");
        let nil_symbol = types.symbol("nil");
        let bool_symbol = types.symbol("bool");

        types.enter(int_symbol, Type::Simple(BaseType::Int));
        types.enter(float_symbol, Type::Simple(BaseType::Float));
        types.enter(bool_symbol, Type::Simple(BaseType::Bool));
        types.enter(nil_symbol, Type::Simple(BaseType::Nil));
        types.enter(string_symbol, Type::Simple(BaseType::Str));

        Env {
            types,
            vars: Table::new(Rc::clone(strings)),
            objects: Table::new(Rc::clone(strings)),
            unique: Unique::new(),
        }
    }
    pub fn look_type(&mut self, symbol: Symbol) -> Option<&Type> {
        self.types.look(symbol)
    }

    pub fn name(&self, symbol: Symbol) -> String {
        self.vars.name(symbol)
    }
    pub fn look_var(&self, symbol: Symbol) -> Option<&Entry> {
        self.vars.look(symbol)
    }

    pub fn unique_id(&mut self) -> Symbol {
        let next = Unique::new().0;
        self.vars.symbol(&next.to_string())
    }

    pub fn look_object(&self, symbol: Symbol) -> Option<&Object> {
        self.objects.look(symbol)
    }

    pub fn begin_scope(&mut self) {
        self.types.begin_scope();
        self.vars.begin_scope();
        self.objects.begin_scope();
    }

    pub fn end_scope(&mut self) {
        self.types.end_scope();
        self.vars.end_scope();
        self.objects.end_scope();
    }

    pub fn add_type(&mut self, symbol: Symbol, data: Type) {
        self.types.enter(symbol, data)
    }

    pub fn add_object(&mut self, symbol: Symbol, data: Object) {
        self.objects.enter(symbol, data)
    }

    pub fn add_var(&mut self, symbol: Symbol, data: Entry) {
        self.vars.enter(symbol, data)
    }

    pub fn assign_object(&mut self, symbol: Symbol, data: Object) {
        self.objects.replace(symbol, data);
    }
}
