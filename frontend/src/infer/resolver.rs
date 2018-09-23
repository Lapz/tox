use ctx::CompileCtx;
use fnv::FnvHashMap;
use syntax::ast::expr::{Expression, VariableUseHandle};
use syntax::ast::statement::Statement;
use util::pos::{Span, Spanned};
use util::symbol::Symbol;

#[derive(Debug, PartialEq)]
pub enum State {
    Declared,
    Defined,
    Read,
}

#[derive(Debug)]
pub struct Resolver {
    scopes: Vec<FnvHashMap<Symbol, State>>,
    current_function: FunctionType,
    current_class: ClassType,
    locals: FnvHashMap<VariableUseHandle, usize>,
}

pub type ResolveError<T> = Result<T, ()>;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FunctionType {
    Function,
    None,
    Method,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ClassType {
    None,
    Class,
}

impl Resolver {
    pub fn new() -> Self {
        Resolver {
            scopes: vec![],
            current_function: FunctionType::None,
            current_class: ClassType::None,
            locals: FnvHashMap::default(),
        }
    }

    pub fn resolve(
        &mut self,
        statements: &[Spanned<Statement>],
        ctx: &mut CompileCtx,
    ) -> Result<(), ()> {
        let mut had_errors = false;

        for statement in statements {
            if self.resolve_statement(statement, ctx).is_err() {
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
        self.scopes.push(FnvHashMap::default())
    }

    fn end_scope(&mut self, span: Span, ctx: &mut CompileCtx) {
        let scopes = self.scopes.pop().unwrap();

        for (symbol, state) in &scopes {
            if state == &State::Defined {
                let msg = format!("Local variable '{}' is not used.", ctx.name(*symbol));
                ctx.warn(msg, span)
            }
        }
    }

    fn peek(&self) -> usize {
        self.scopes.len() - 1
    }

    fn not_resolved(&self, name: &Symbol) -> bool {
        !self.scopes.is_empty() && self.scopes[self.peek()].get(name) == Some(&State::Declared)
    }

    fn declare(&mut self, name: Symbol, span: Span, ctx: &mut CompileCtx) -> ResolveError<()> {
        if self.scopes.is_empty() {
            return Ok(());
        }

        let index = self.peek();

        if self.scopes[index].contains_key(&name) {
            let msg = format!(
                "Variable with name '{}', already declared in this scope.",
                ctx.name(name)
            );

            ctx.warn(msg, span);
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
}

impl Resolver {
    fn resolve_statement(
        &mut self,
        statement: &Spanned<Statement>,
        ctx: &mut CompileCtx,
    ) -> ResolveError<()> {
        match statement.value {
            Statement::Print(ref expr) | Statement::Expr(ref expr) => {
                self.resolve_expression(expr, ctx)?;
                Ok(())
            }
            Statement::Block(ref statements) => {
                self.begin_scope();

                for statement in statements {
                    self.resolve_statement(statement, ctx)?;
                }

                self.end_scope(statement.span, ctx);

                Ok(())
            }

            Statement::TypeAlias { ref alias, .. } => {
                self.declare(alias.value, alias.span, ctx)?;
                self.define(alias.value);

                Ok(())
            }

            Statement::If {
                ref cond,
                ref then,
                ref otherwise,
            } => {
                self.resolve_expression(cond, ctx)?;

                self.resolve_statement(then, ctx)?;

                match *otherwise {
                    Some(ref expr) => self.resolve_statement(expr, ctx)?,
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
                    self.resolve_statement(init, ctx)?;
                }

                if let Some(ref cond) = *cond {
                    self.resolve_expression(cond, ctx)?;
                }

                if let Some(ref incr) = *incr {
                    self.resolve_expression(incr, ctx)?;
                }

                self.resolve_statement(body, ctx)?;

                Ok(())
            }

            Statement::While { ref cond, ref body } => {
                self.resolve_expression(cond, ctx)?;
                self.resolve_statement(body, ctx)?;
                Ok(())
            }

            Statement::Break | Statement::Continue => Ok(()),

            Statement::Return(ref r) => {
                if self.current_function == FunctionType::None {
                    ctx.error("Cannot return from top-level code", statement.span);
                    return Err(());
                }
                self.resolve_expression(r, ctx)
            }

            Statement::Var {
                ref ident,
                ref expr,
                ..
            } => {
                self.declare(ident.value, ident.span, ctx)?;

                if let Some(ref expr) = *expr {
                    self.resolve_expression(expr, ctx)?
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
                self.declare(name.value, name.span, ctx)?;
                self.define(name.value);

                let enclosing_function = self.current_function;

                self.current_function = FunctionType::Function;

                self.begin_scope();

                for param in &params.value {
                    self.declare(param.value.name.value, param.span, ctx)?;
                    self.define(param.value.name.value)
                }

                self.resolve_statement(body, ctx)?;

                self.end_scope(body.span, ctx);

                self.current_function = enclosing_function;

                Ok(())
            }
            Statement::Class {
                ref name,
                ref body,
                ref superclass,
            } => {
                self.declare(name.value, name.span, ctx)?;
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
                    self.declare(field.value.name.value, field.value.name.span, ctx)?;
                    self.define(field.value.name.value);
                }

                for method in &body.value.0 {
                    self.current_function = FunctionType::Method;
                    self.resolve_statement(method, ctx)?;
                }

                self.current_function = FunctionType::None;

                if sklass {
                    self.end_scope(body.span, ctx);
                }

                self.end_scope(body.span, ctx);

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
        ctx: &mut CompileCtx,
    ) -> ResolveError<()> {
        match expr.value {
            Expression::Array { ref items, .. } => {
                for item in items {
                    self.resolve_expression(item, ctx)?;
                }
                Ok(())
            }

            Expression::Assign {
                ref handle,
                ref name,
                ref value,
                ..
            } => {
                self.resolve_expression(value, ctx)?;
                self.resolve_local(&name.value, *handle, false);
                Ok(())
            }

            Expression::Binary {
                ref lhs, ref rhs, ..
            } => {
                self.resolve_expression(lhs, ctx)?;
                self.resolve_expression(rhs, ctx)?;
                Ok(())
            }

            Expression::Call {
                ref callee,
                ref args,
            } => {
                self.resolve_expression(callee, ctx)?;

                for argument in args {
                    self.resolve_expression(argument, ctx)?;
                }

                Ok(())
            }

            Expression::ClassInstance { ref props, .. } => {
                for prop in props.iter() {
                    self.declare(prop.value.symbol.value, prop.value.symbol.span, ctx)?;
                    self.define(prop.value.symbol.value);
                    self.resolve_expression(&prop.value.expr, ctx)?;
                }
                Ok(())
            }

            Expression::Get { ref object, .. } => {
                self.resolve_expression(object, ctx)?;
                Ok(())
            }

            Expression::Grouping { ref expr } => self.resolve_expression(expr, ctx),

            Expression::Index {
                ref index,
                ref target,
            } => {
                self.resolve_expression(index, ctx)?;
                self.resolve_expression(target, ctx)?;
                Ok(())
            }

            Expression::Literal(_) => Ok(()),

            Expression::Set {
                ref value,
                ref object,
                ..
            } => {
                self.resolve_expression(value, ctx)?;
                self.resolve_expression(object, ctx)?;
                Ok(())
            }

            Expression::Ternary {
                ref condition,
                ref then_branch,
                ref else_branch,
            } => {
                self.resolve_expression(condition, ctx)?;
                self.resolve_expression(else_branch, ctx)?;
                self.resolve_expression(then_branch, ctx)?;
                Ok(())
            }

            Expression::Unary { ref expr, .. } => {
                self.resolve_expression(expr, ctx)?;
                Ok(())
            }

            Expression::This(ref handle) => {
                if self.current_class == ClassType::None {
                    ctx.error("Cannot use 'this' outside of a class.", expr.span);
                    return Err(());
                }

                self.resolve_local(&Symbol(0), *handle, false);

                Ok(())
            }

            Expression::Var(ref v, ref handle) => {
                if self.not_resolved(&v.value) {
                    let msg = format!(
                        "Cannot read local variable '{}' in its own initializer.",
                        ctx.name(v.value)
                    );

                    ctx.error(msg, expr.span);
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
    use super::Resolver;
    use ctx::CompileCtx;
    use std::rc::Rc;
    use syntax::ast::statement::Statement;
    use syntax::lexer::Lexer;
    use syntax::parser::Parser;
    use util::emmiter::Reporter;
    use util::pos::Spanned;
    use util::symbol::{SymbolFactory, Symbols};

    fn get_ast(
        input: &str,
        strings: Rc<SymbolFactory>,
        reporter: Reporter,
    ) -> Vec<Spanned<Statement>> {
        let tokens = Lexer::new(input, reporter.clone()).lex().unwrap();

        let mut symbols = Symbols::new(strings);
        Parser::new(tokens, reporter.clone(), &mut symbols)
            .parse()
            .unwrap()
    }

    #[test]
    fn global() {
        let input = "var a = 0; { fun f() { print(a);} }";
        let strings = Rc::new(SymbolFactory::new());
        let mut reporter = Reporter::new();
        let ast = get_ast(input, strings.clone(), reporter.clone());
        let mut ctx = CompileCtx::new(&strings, &mut reporter);

        assert!(Resolver::new().resolve(&ast, &mut ctx).is_ok())
    }

    #[test]
    fn captured() {
        let input = "{var a = 0;fun f() {print(a);}}";
        let mut reporter = Reporter::new();
        let strings = Rc::new(SymbolFactory::new());
        let ast = get_ast(input, strings.clone(), reporter.clone());
        let mut ctx = CompileCtx::new(&strings, &mut reporter);
        assert!(Resolver::new().resolve(&ast, &mut ctx).is_ok())
    }

    #[test]
    fn lexical_capture() {
        let input = "var a = 0;{fun f() {print(a);} var a = 1;}";
        let strings = Rc::new(SymbolFactory::new());
        let mut reporter = Reporter::new();
        let ast = get_ast(input, strings.clone(), reporter.clone());
        let mut ctx = CompileCtx::new(&strings, &mut reporter);

        assert!(Resolver::new().resolve(&ast, &mut ctx).is_ok())
    }

    #[test]
    fn shadowing_erro() {
        let input = "var a = 0; { var a = a;}";
        let strings = Rc::new(SymbolFactory::new());
        let mut reporter = Reporter::new();
        let ast = get_ast(input, strings.clone(), reporter.clone());
        let mut ctx = CompileCtx::new(&strings, &mut reporter);

        assert!(Resolver::new().resolve(&ast, &mut ctx).is_err())
    }

    #[test]
    fn local_redeclar() {
        let input = "{var a = 1;var a = 2;}";
        let strings = Rc::new(SymbolFactory::new());
        let mut reporter = Reporter::new();
        let ast = get_ast(input, strings.clone(), reporter.clone());
        let mut ctx = CompileCtx::new(&strings, &mut reporter);

        assert!(Resolver::new().resolve(&ast, &mut ctx).is_err())
    }

    #[test]
    fn global_redeclar() {
        let input = "var a = 1;var a = 2;";
        let strings = Rc::new(SymbolFactory::new());
        let mut reporter = Reporter::new();
        let ast = get_ast(input, strings.clone(), reporter.clone());
        let mut ctx = CompileCtx::new(&strings, &mut reporter);
        assert!(Resolver::new().resolve(&ast, &mut ctx).is_ok())
    }

    #[test]
    fn return_top_level() {
        let input = "return 10;";
        let strings = Rc::new(SymbolFactory::new());
        let mut reporter = Reporter::new();
        let ast = get_ast(input, strings.clone(), reporter.clone());
        let mut ctx = CompileCtx::new(&strings, &mut reporter);

        assert!(Resolver::new().resolve(&ast, &mut ctx).is_err())
    }

    #[test]

    fn this_outside_class() {
        let input = "this.name;";
        let strings = Rc::new(SymbolFactory::new());
        let mut reporter = Reporter::new();
        let ast = get_ast(input, strings.clone(), reporter.clone());
        let mut ctx = CompileCtx::new(&strings, &mut reporter);

        assert!(Resolver::new().resolve(&ast, &mut ctx).is_err())
    }

}
