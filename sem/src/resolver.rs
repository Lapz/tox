use std::collections::HashMap;
use syntax::ast::statement::Statement;
use syntax::ast::expr::{Expression, VariableUseHandle};
use util::pos::{Span, Spanned};
use util::emmiter::Reporter;
use util::symbol::Symbol;
use util::env::TypeEnv;

#[derive(Debug, PartialEq)]
pub enum State {
    Declared,
    Defined,
    Read,
}

#[derive(Debug)]
pub struct Resolver {
    scopes: Vec<HashMap<Symbol, State>>,
    current_function: FunctionType,
    current_class: ClassType,
    pub locals: HashMap<VariableUseHandle, usize>,
    pub reporter: Reporter,
}

pub type ResolveError<T> = Result<T, ()>;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FunctionType {
    Function,
    None,
    Method,
    Init,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ClassType {
    None,
    Class,
}

impl Resolver {
    pub fn new(reporter: Reporter) -> Self {
        Resolver {
            scopes: vec![],
            reporter,
            current_function: FunctionType::None,
            current_class: ClassType::None,
            locals: HashMap::new(),
        }
    }

    pub fn resolve(&mut self, statements: &[Spanned<Statement>], env: &TypeEnv) -> Result<(), ()> {
        let mut had_errors = false;

        for statement in statements {
            if let Err(_) = self.resolve_statement(statement, env) {
                had_errors = true;
            }
        }

        if had_errors {
            Err(())
        } else {
            Ok(())
        }
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new())
    }

    fn end_scope(&mut self, span: Span, env: &TypeEnv) {
        let scopes = self.scopes.pop().unwrap();

        for (symbol, state) in scopes.iter() {
            if state == &State::Defined {
                let msg = format!("Local variable '{}' is not used.", env.name(*symbol));
                self.warn(msg, span)
            }
        }
    }

    fn peek(&self) -> usize {
        self.scopes.len() - 1
    }

    fn not_resolved(&self, name: &Symbol) -> bool {
        !self.scopes.is_empty() && self.scopes[self.peek()].get(name) == Some(&State::Declared)
    }

    fn declare(&mut self, name: Symbol, span: Span, env: &TypeEnv) -> ResolveError<()> {
        if self.scopes.is_empty() {
            return Ok(());
        }

        let index = self.peek();

        if self.scopes[index].contains_key(&name) {
            let msg = format!(
                "Variable with name '{}', already declared in this scope.",
                env.name(name)
            );

            self.warn(msg, span);
            return Err(());
        }

        self.scopes[index].insert(name, State::Declared);

        Ok(())
    }

    pub fn insert(&mut self, name: Symbol, state: State) {
        let top = self.peek();
        self.scopes[top].insert(name, state);
    }

    pub fn define(&mut self, name: Symbol) {
        if self.scopes.is_empty() {
            return;
        };

        let index = self.peek();
        self.scopes[index].insert(name, State::Defined);
    }

    pub fn resolve_local(&mut self, name: &Symbol, handle: VariableUseHandle, is_read: bool) {
        let max_depth = self.scopes.len();

        for i in 0..max_depth {
            if self.scopes[max_depth - i - 1].contains_key(name) {
                if is_read {
                    if let Some(state) = self.scopes[max_depth - i - 1].get_mut(name) {
                        *state = State::Read
                    }
                }
                self.locals.insert(handle, i);
                return;
            }
        }

        self.locals.insert(handle, max_depth); // Globals
    }

    fn error<T: Into<String>>(&mut self, msg: T, span: Span) {
        self.reporter.error(msg, span)
    }

    fn warn<T: Into<String>>(&mut self, msg: T, span: Span) {
        self.reporter.warn(msg, span)
    }
}

impl Resolver {
    fn resolve_statement(
        &mut self,
        statement: &Spanned<Statement>,
        env: &TypeEnv,
    ) -> ResolveError<()> {
        match statement.value {
            Statement::Print(ref expr) | Statement::Expr(ref expr) => {
                self.resolve_expression(&expr, env)?;
                Ok(())
            }
            Statement::Block(ref statements) => {
                self.begin_scope();

                for statement in statements {
                    self.resolve_statement(statement, env)?;
                }

                self.end_scope(statement.span, env);

                Ok(())
            }

            Statement::TypeAlias { ref alias, .. } => {
                self.declare(alias.value, alias.span, env)?;
                self.define(alias.value);

                Ok(())
            }

            Statement::If {
                ref cond,
                ref then,
                ref otherwise,
            } => {
                self.resolve_expression(cond, env)?;

                self.resolve_statement(then, env)?;

                match *otherwise {
                    Some(ref expr) => self.resolve_statement(expr, env)?,
                    None => return Ok(()),
                };

                Ok(())
            }

            Statement::For {
                ref init,
                ref cond,
                ref incr,
                ref body,
            } => {
                if let Some(ref init) = *init {
                    self.resolve_statement(init, env)?;
                }

                if let Some(ref cond) = *cond {
                    self.resolve_expression(cond, env)?;
                }

                if let Some(ref incr) = *incr {
                    self.resolve_expression(incr, env)?;
                }

                self.resolve_statement(body, env)?;

                Ok(())
            }

            Statement::While { ref cond, ref body } => {
                self.resolve_expression(cond, env)?;
                self.resolve_statement(body, env)?;
                Ok(())
            }

            Statement::Break | Statement::Continue => Ok(()),

            Statement::Return(ref r) => {
                if self.current_function == FunctionType::None {
                    self.error("Cannot return from top-level code", statement.span);
                    return Err(());
                }
                self.resolve_expression(r, env)
            }

            Statement::Var {
                ref ident,
                ref expr,
                ..
            } => {
                self.declare(ident.value, ident.span, env)?;

                if let Some(ref expr) = *expr {
                    self.resolve_expression(expr, env)?
                }

                self.define(ident.value);
                Ok(())
            }

            Statement::Function {
                ref name,
                ref body,
                ref params,
                ..
            } => {
                self.declare(name.value, name.span, env)?;
                self.define(name.value);

                let enclosing_function = self.current_function;

                self.current_function = FunctionType::Function;

                self.begin_scope();

                for param in &params.value {
                    self.declare(param.value.name.value, param.span, env)?;
                    self.define(param.value.name.value)
                }

                self.resolve_statement(body, env)?;

                self.end_scope(body.span, env);

                self.current_function = enclosing_function;

                Ok(())
            }
            Statement::Class {
                ref name,
                ref body,
                ref superclass,
            } => {
                self.declare(name.value, name.span, env)?;
                self.define(name.value);

                let enclosing_class = self.current_class;
                let mut sklass = false;

                self.current_class = ClassType::Class;

                if let Some(ref sclass) = *superclass {
                    self.define(sclass.value);
                    self.begin_scope();
                    self.insert(Symbol(1), State::Read); // super
                    sklass = true;
                }

                self.begin_scope();

                self.insert(Symbol(0), State::Read); // this

                for field in &body.value.1 {
                    self.declare(field.value.name.value, field.value.name.span, env)?;
                    self.define(field.value.name.value);
                }

                for method in &body.value.0 {
                    self.current_function = FunctionType::Method;
                    self.resolve_statement(method, env)?;
                }

                self.current_function = FunctionType::None;

                if sklass {
                    self.end_scope(body.span, env);
                }

                self.end_scope(body.span, env);

                self.current_class = enclosing_class;

                Ok(())
            }
        }
    }
}

impl Resolver {
    fn resolve_expression(
        &mut self,
        expr: &Spanned<Expression>,
        env: &TypeEnv,
    ) -> ResolveError<()> {
        match expr.value {
            Expression::Array { ref items, .. } => {
                for item in items {
                    self.resolve_expression(item, env)?;
                }
                Ok(())
            }

            Expression::Assign {
                ref handle,
                ref name,
                ref value,
                ..
            } => {
                self.resolve_expression(value, env)?;
                self.resolve_local(&name.value, *handle, false);
                Ok(())
            }

            Expression::Binary {
                ref lhs, ref rhs, ..
            } => {
                self.resolve_expression(lhs, env)?;
                self.resolve_expression(rhs, env)?;
                Ok(())
            }

            Expression::Call {
                ref callee,
                ref args,
            } => {
                self.resolve_expression(callee, env)?;

                for argument in args {
                    self.resolve_expression(argument, env)?;
                }

                Ok(())
            }

            Expression::ClassInstance { ref props, .. } => {
                for prop in props.iter() {
                    self.declare(prop.value.symbol.value, prop.value.symbol.span, env)?;
                    self.define(prop.value.symbol.value);
                    self.resolve_expression(&prop.value.expr, env)?;
                }
                Ok(())
            }

            Expression::Get { ref object, .. } => {
                self.resolve_expression(object, env)?;
                Ok(())
            }

            Expression::Grouping { ref expr } => self.resolve_expression(expr, env),

            Expression::Index {
                ref index,
                ref target,
            } => {
                self.resolve_expression(index, env)?;
                self.resolve_expression(target, env)?;
                Ok(())
            }

            Expression::Literal(_) => Ok(()),

            Expression::Set {
                ref value,
                ref object,
                ..
            } => {
                self.resolve_expression(value, env)?;
                self.resolve_expression(object, env)?;
                Ok(())
            }

            Expression::Ternary {
                ref condition,
                ref then_branch,
                ref else_branch,
            } => {
                self.resolve_expression(condition, env)?;
                self.resolve_expression(else_branch, env)?;
                self.resolve_expression(then_branch, env)?;
                Ok(())
            }

            Expression::Unary { ref expr, .. } => {
                self.resolve_expression(expr, env)?;
                Ok(())
            }

            Expression::This(ref handle) => {
                if self.current_class == ClassType::None {
                    self.error("Cannot use 'this' outside of a class.", expr.span);
                    return Err(());
                }

                self.resolve_local(&Symbol(0), *handle, false);

                Ok(())
            }

            Expression::Var(ref v, ref handle) => {
                if self.not_resolved(&v.value) {
                    let msg = format!(
                        "Cannot read local variable '{}' in its own initializer.",
                        env.name(v.value)
                    );

                    self.error(msg, expr.span);
                    return Err(());
                }

                self.resolve_local(&v.value, *handle, true);
                Ok(())
            }
        }
    }
}

#[cfg(test)]
mod test {
    use syntax::ast::statement::Statement;
    use syntax::lexer::Lexer;
    use util::symbol::{SymbolFactory, Table};
    use syntax::parser::Parser;
    use resolver::Resolver;
    use util::pos::Spanned;
    use util::emmiter::Reporter;
    use util::env::TypeEnv;
    use std::rc::Rc;

    fn get_ast(
        input: &str,
        strings: Rc<SymbolFactory>,
        reporter: Reporter,
    ) -> Vec<Spanned<Statement>> {
        let tokens = Lexer::new(input, reporter.clone()).lex().unwrap();

        let mut symbols = Table::new(strings);
        Parser::new(tokens, reporter.clone(), &mut symbols)
            .parse()
            .unwrap()
    }

    #[test]
    fn global() {
        let input = "var a = 0; { fun f() { print(a);} }";
        let strings = Rc::new(SymbolFactory::new());
        let env = TypeEnv::new(&strings);
        let reporter = Reporter::new();
        assert!(
            Resolver::new(reporter.clone())
                .resolve(&get_ast(input, strings, reporter), &env)
                .is_ok()
        )
    }

    #[test]
    fn captured() {
        let input = "{var a = 0;fun f() {print(a);}}";
        let reporter = Reporter::new();
        let strings = Rc::new(SymbolFactory::new());
        let env = TypeEnv::new(&strings);
        assert!(
            Resolver::new(reporter.clone())
                .resolve(&get_ast(input, strings, reporter), &env)
                .is_ok()
        )
    }

    #[test]
    fn lexical_capture() {
        let input = "var a = 0;{fun f() {print(a);} var a = 1;}";
        let strings = Rc::new(SymbolFactory::new());
        let env = TypeEnv::new(&strings);
        let reporter = Reporter::new();
        assert!(
            Resolver::new(reporter.clone())
                .resolve(&get_ast(input, strings, reporter), &env)
                .is_ok()
        )
    }

    #[test]
    fn shadowing_erro() {
        let input = "var a = 0; { var a = a;}";
        let strings = Rc::new(SymbolFactory::new());
        let env = TypeEnv::new(&strings);
        let reporter = Reporter::new();
        assert!(
            Resolver::new(reporter.clone())
                .resolve(&get_ast(input, strings, reporter), &env)
                .is_err()
        )
    }

    #[test]
    fn local_redeclar() {
        let input = "{var a = 1;var a = 2;}";
        let strings = Rc::new(SymbolFactory::new());
        let env = TypeEnv::new(&strings);
        let reporter = Reporter::new();
        assert!(
            Resolver::new(reporter.clone())
                .resolve(&get_ast(input, strings, reporter), &env)
                .is_err()
        )
    }

    #[test]
    fn global_redeclar() {
        let input = "var a = 1;var a = 2;";
        let strings = Rc::new(SymbolFactory::new());
        let env = TypeEnv::new(&strings);
        let reporter = Reporter::new();
        assert!(
            Resolver::new(reporter.clone())
                .resolve(&get_ast(input, strings, reporter), &env)
                .is_ok()
        )
    }

    #[test]
    fn return_top_level() {
        let input = "return 10;";
        let strings = Rc::new(SymbolFactory::new());
        let env = TypeEnv::new(&strings);
        let reporter = Reporter::new();
        Resolver::new(reporter.clone())
            .resolve(&get_ast(input, strings, reporter.clone()), &env)
            .is_err();
        assert!(reporter.has_error())
    }

    #[test]
    #[should_panic]
    fn this_outside_class() {
        let input = "this.name;";
        let strings = Rc::new(SymbolFactory::new());
        let env = TypeEnv::new(&strings);
        let reporter = Reporter::new();

        Resolver::new(reporter.clone())
            .resolve(&get_ast(input, strings, reporter.clone()), &env)
            .is_err();

        assert!(reporter.has_error())
    }

}
