use infer::env::VarEntry;
use infer::types::{Type, TypeCon};

use std::rc::Rc;
use util::emmiter::Reporter;
use util::pos::Span;
use util::symbol::{Symbol, SymbolFactory, Symbols};

#[derive(Debug)]
pub struct CompileCtx<'a> {
    symbols: Symbols<()>,
    types: Symbols<Type>,
    vars: Symbols<VarEntry>,
    reporter: &'a mut Reporter,
}

impl<'a> CompileCtx<'a> {
    pub fn new(strings: &Rc<SymbolFactory>, reporter: &'a mut Reporter) -> Self {
        let mut types = Symbols::new(Rc::clone(strings));
        let string_symbol = types.symbol("str");
        let int_symbol = types.symbol("int");
        let float_symbol = types.symbol("float");
        let nil_symbol = types.symbol("nil");
        let bool_symbol = types.symbol("bool");

        types.enter(int_symbol, Type::App(TypeCon::Int, vec![]));
        types.enter(float_symbol, Type::App(TypeCon::Float, vec![]));
        types.enter(bool_symbol, Type::App(TypeCon::Bool, vec![]));
        types.enter(nil_symbol, Type::Nil);
        types.enter(string_symbol, Type::App(TypeCon::Str, vec![]));

        let mut vars = Symbols::new(Rc::clone(strings));

        {
            let mut add_builtin = |name: &str, mut params: Vec<Type>, returns: Type| {
                let symbol = vars.symbol(name);
                params.push(returns);
                vars.enter(
                    symbol,
                    VarEntry::Fun {
                        ty: Type::Generic(vec![], Box::new(Type::App(TypeCon::Arrow, params))),
                    },
                );
            };

            add_builtin("clock", vec![], Type::App(TypeCon::Float, vec![]));
            add_builtin(
                "random",
                vec![
                    Type::App(TypeCon::Int, vec![]),
                    Type::App(TypeCon::Int, vec![]),
                ],
                Type::App(TypeCon::Int, vec![]),
            );
        }

        CompileCtx {
            symbols: Symbols::new(Rc::clone(strings)),
            types,
            vars,
            reporter,
        }
    }

    /// Report an error
    pub fn error<T: Into<String>>(&mut self, msg: T, span: Span) {
        self.reporter.error(msg, span)
    }

    pub fn warn<T: Into<String>>(&mut self, msg: T, span: Span) {
        self.reporter.warn(msg, span)
    }

    pub fn global_error(&mut self, msg: &str) {
        self.reporter.global_error(msg)
    }
    pub fn remove_error(&mut self) {
        self.reporter.remove_error();
    }

    /// Check for a type in the type Env
    pub fn look_type(&mut self, symbol: Symbol) -> Option<&Type> {
        self.types.look(symbol)
    }

    /// Find the corresponding name for a symbol
    pub fn name(&self, symbol: Symbol) -> String {
        self.vars.name(symbol)
    }

    pub fn symbols(&self) -> &Symbols<()> {
        &self.symbols
    }

    /// CHeck for a Type in the symbol Env
    pub fn look_var(&self, symbol: Symbol) -> Option<&VarEntry> {
        self.vars.look(symbol)
    }

    /// Begins a new scope
    pub fn begin_scope(&mut self) {
        self.types.begin_scope();
        self.vars.begin_scope();
    }

    /// Ends the old scope
    pub fn end_scope(&mut self) {
        self.types.end_scope();
        self.vars.end_scope();
    }

    pub fn add_type(&mut self, symbol: Symbol, data: Type) {
        self.types.enter(symbol, data)
    }

    pub fn add_var(&mut self, symbol: Symbol, data: VarEntry) {
        self.vars.enter(symbol, data)
    }
}
