use infer::env::VarEntry;
use infer::types::{CSpan, Constructor, PatternVar, Type, TypeCon};

use infer::coverage::CoverageError;
use std::collections::HashMap;
use std::rc::Rc;
use syntax::ast::Pattern::Con;
use util::emmiter::Reporter;
use util::pos::Span;
use util::symbol::{Symbol, SymbolFactory, Symbols};

#[derive(Debug)]
pub struct CompileCtx<'a> {
    symbols: Symbols<()>,
    types: Symbols<Type>,
    vars: Symbols<VarEntry>,
    patterns: Symbols<PatternVar>,
    pattern_types: HashMap<PatternVar, Type>,
    constructors: HashMap<Type, Vec<Constructor>>,
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

        let mut constructors = HashMap::new();

        constructors.insert(
            Type::App(TypeCon::Int, vec![]),
            vec![Constructor::new(int_symbol, vec![], 0, CSpan::Infinity)],
        );

        constructors.insert(
            Type::App(TypeCon::Float, vec![]),
            vec![Constructor::new(float_symbol, vec![], 0, CSpan::Infinity)],
        );

        constructors.insert(
            Type::App(TypeCon::Bool, vec![]),
            vec![Constructor::new(bool_symbol, vec![], 0, CSpan::Range(2))],
        );

        constructors.insert(
            Type::Nil,
            vec![Constructor::new(nil_symbol, vec![], 0, CSpan::Range(1))],
        );

        constructors.insert(
            Type::App(TypeCon::Str, vec![]),
            vec![Constructor::new(string_symbol, vec![], 0, CSpan::Infinity)],
        );

        CompileCtx {
            symbols: Symbols::new(Rc::clone(strings)),
            types,
            vars,
            reporter,
            patterns: Symbols::new(Rc::clone(strings)),
            pattern_types: HashMap::new(),
            constructors,
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

    pub fn symbols_mut(&mut self) -> &mut Symbols<()> {
        &mut self.symbols
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

    pub fn add_pattern_var(&mut self, symbol: Symbol, data: PatternVar) {
        self.patterns.enter(symbol, data)
    }

    pub fn add_pattern_type(&mut self, uid: PatternVar, ty: Type) {
        self.pattern_types.insert(uid, ty);
    }

    pub fn add_constructor(&mut self, ty: Type, cons: Vec<Constructor>) {
        self.constructors.insert(ty, cons);
    }

    pub fn with_types(&mut self, types: HashMap<PatternVar, Type>) {
        self.pattern_types.extend(types.into_iter())
    }

    pub fn get_pattern_type(&self, uid: PatternVar) -> Result<&Type, CoverageError> {
        match self.pattern_types.get(&uid) {
            Some(ty) => Ok(ty),
            None => Err(CoverageError::NoTypeFound(uid)),
        }
    }

    pub fn get_constructors(&self, typ: &Type) -> Result<Vec<Constructor>, CoverageError> {
        match self.constructors.get(typ) {
            Some(cons) => {
                if cons.is_empty() {
                    Err(CoverageError::EmptyType(typ.clone()))
                } else {
                    Ok(cons.to_vec())
                }
            }
            None => Err(CoverageError::NoConstructorsFound(typ.clone())),
        }
    }
}
