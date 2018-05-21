use env::{Entry, VarEntry};
use std::rc::Rc;
use types::{Type, Unique};
use util::emmiter::Reporter;
use util::pos::Span;
use util::symbol::{Symbol, SymbolFactory, Symbols};
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

        types.enter(int_symbol, Type::Int);
        types.enter(float_symbol, Type::Float);
        types.enter(bool_symbol, Type::Bool);
        types.enter(nil_symbol, Type::Nil);
        types.enter(string_symbol, Type::Str);

        let mut vars = Symbols::new(Rc::clone(strings));

        {
            let mut add_builtin = |name: &str, params: Vec<Type>, returns: Type| {
                let symbol = vars.symbol(name);
                vars.enter(
                    symbol,
                    VarEntry::Fun {
                        ty: Type::Fun(params, Box::new(returns)),
                    },
                )
            };

            add_builtin("clock", vec![], Type::Float);
            add_builtin("hex", vec![Type::Int], Type::Str);
            add_builtin("oct", vec![Type::Int], Type::Str);
            add_builtin("random", vec![Type::Int, Type::Int], Type::Int);
            add_builtin("to_int", vec![Type::Str], Type::Int);
        }

        let mut add_builtin_class = |name: &str, methods: Vec<(&str, Entry)>| {
            let symbol = vars.symbol(name);

            use std::collections::HashMap;

            let mut methods_ty = HashMap::new();

            for method in methods {
                let name = vars.symbol(method.0);
                methods_ty.insert(name, method.1);
            }

            let entry = VarEntry::Var(Type::Class(
                symbol,
                methods_ty,
                HashMap::new(),
                Unique::new(),
            ));

            vars.enter(symbol, entry);
        };

        add_builtin_class(
            "io",
            vec![(
                "readline",
                Entry::Fun(Type::Fun(vec![], Box::new(Type::Str))),
            )],
        );

        CompileCtx {
            symbols: Symbols::new(Rc::clone(strings)),
            types: Symbols::new(Rc::clone(strings)),
            vars: Symbols::new(Rc::clone(strings)),
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

    /// Check for a type in the type Env
    pub fn look_type(&mut self, symbol: Symbol) -> Option<&Type> {
        self.types.look(symbol)
    }

    /// Find the corresponding name for a symbol
    pub fn name(&self, symbol: Symbol) -> String {
        self.vars.name(symbol)
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
