use env::{Entry, VarEntry};
use std::rc::Rc;
use types::Type;
use util::emmiter::Reporter;
use util::symbol::{SymbolFactory, Symbols};

struct CompileCtx {
    symbols: Symbols<()>,
    types: Symbols<Entry>,
    var_env: Symbols<VarEntry>,
    reporter: Reporter,
}

impl CompileCtx {
    pub fn new(strings: &Rc<SymbolFactory>, reporter: Reporter) -> Self {
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

            let entry = VarEntry::Var(Type::Class {
                name: symbol,
                methods: methods_ty,
                fields: HashMap::new(),
            });

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
            var_env: Symbols::new(Rc::clone(strings)),
            reporter,
        }
    }
}
