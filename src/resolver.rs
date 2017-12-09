use std::collections::HashMap;
use ast::statement::*;
use ast::expr::*;
use std::fmt::{Display, Formatter};
use std::fmt;
use pos::{Postition, WithPos};

#[derive(Debug)]
pub struct Resolver<'a> {
    scopes: Vec<HashMap<Variable<'a>, bool>>,
    current_function: FunctionType,
    current_class: ClassType,
    locals: HashMap<VariableUseHandle, usize>,
}

#[derive(Debug)]
pub enum ResolverError {
    ReadInInit(String),
    AllReadyDecleared(String),
    Return(String),
    This(String),
    Init(String),
}

impl Display for ResolverError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            ResolverError::ReadInInit(ref s)
            | ResolverError::Init(ref s)
            | ResolverError::Return(ref s)
            | ResolverError::This(ref s)
            | ResolverError::AllReadyDecleared(ref s) => write!(f, "{}", s),
        }
    }
}

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

impl<'a> Default for Resolver<'a> {
    fn default() -> Self {
        Resolver {
            scopes: vec![],
            current_function: FunctionType::None,
            current_class: ClassType::None,
            locals: HashMap::new(),
        }
    }
}


impl<'a> Resolver<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn resolve(
        &mut self,
        statements: Vec<WithPos<Statement<'a>>>,
    ) -> Result<(), Vec<ResolverError>> {
        let mut errors = vec![];

        for statement in statements {
            match self.resolve_statement(&statement) {
                Ok(_) => (),
                Err(e) => errors.push(e),
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new())
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn peek(&self) -> usize {
        self.scopes.len() - 1
    }

    fn not_resolved(&self, name: &Variable<'a>) -> bool {
        !self.scopes.is_empty() && self.scopes[self.peek()].get(name) == Some(&false)
    }

    fn declare(&mut self, name: Variable<'a>, pos: Postition) -> Result<(), ResolverError> {
        if self.scopes.is_empty() {
            return Ok(());
        }

        let index = self.peek();

        if self.scopes[index].contains_key(&name) {
            let msg = format!(
                "Variable with name '{}', already declared in this scope.",
                name
            );
            return Err(ResolverError::AllReadyDecleared(self.error(&msg, pos)));
        }

        self.scopes[index].insert(name, false);

        Ok(())
    }

    pub fn insert(&mut self, name: Variable<'a>, state: bool) {
        let top = self.peek();
        self.scopes[top].insert(name, state);
    }

    pub fn define(&mut self, name: Variable<'a>) {
        if self.scopes.is_empty() {
            return;
        };

        let index = self.peek();
        self.scopes[index].insert(name, true);
    }

    pub fn resolve_local(&mut self, name: &Variable<'a>, handle: VariableUseHandle) {
        let max_depth = self.scopes.len();

        for i in 0..max_depth {
            if self.scopes[max_depth - i - 1].contains_key(name) {
                self.locals.insert(handle, i);
                return;
            }
        }

        self.locals.insert(handle, max_depth); // Globals
    }

    fn resolve_func(
        &mut self,
        body: &Expression<'a>,
        kind: FunctionType,
        pos: Postition,
    ) -> Result<(), ResolverError> {
        let enclosing_function = self.current_function;

        self.current_function = kind;

        match body {
            &Expression::Func {
                ref body,
                ref parameters,
            } => {
                self.begin_scope();

                for param in parameters {
                    self.declare(param.clone(), pos)?;
                    self.define(param.clone());
                }

                self.resolve_statement(body)?;

                self.end_scope();

                self.current_function = enclosing_function;

                Ok(())
            }

            _ => unreachable!(),
        }
    }

    fn error(&self, message: &str, pos: Postition) -> String {
        format!("{} on {}", message, pos)
    }
}

impl<'a> Resolver<'a> {
    fn resolve_statement(
        &mut self,
        statement: &WithPos<Statement<'a>>,
    ) -> Result<(), ResolverError> {
        match statement.node {
            Statement::Block(ref statements) => {
                self.begin_scope();

                for statement in statements {
                    self.resolve_statement(statement)?;
                }

                self.end_scope();

                Ok(())
            }

            Statement::ExpressionStmt(ref expr) => {
                self.resolve_expr(expr, statement.pos)?;
                Ok(())   
            },

            Statement::IfStmt {
                ref condition,
                ref then_branch,
                ref else_branch,
            } => {
                self.resolve_expr(condition, statement.pos)?;
                self.resolve_statement(then_branch)?;

                match *else_branch {
                    Some(ref expr) => self.resolve_statement(expr)?,
                    None => return Ok(()),
                };

                Ok(())
            }

            Statement::ForStmt {
                ref initializer,
                ref condition,
                ref increment,
                ref body,
            } => {
                self.resolve_statement(initializer)?;
                self.resolve_expr(condition, statement.pos)?;
                self.resolve_expr(increment, statement.pos)?;
                self.resolve_statement(body)?;
                Ok(())
            }

            Statement::WhileStmt {
                ref condition,
                ref body,
            } => {
                self.resolve_expr(condition, statement.pos)?;
                self.resolve_statement(body)?;
                Ok(())
            }

            Statement::DoStmt {
                ref condition,
                ref body,
            } => {
                self.resolve_expr(condition, statement.pos)?;
                self.resolve_statement(body)?;
                Ok(())
            }

            Statement::Break | Statement::Continue => Ok(()),

            Statement::Return(ref r) => {
                if self.current_function == FunctionType::None {
                    return Err(ResolverError::Return(self.error(
                        "Cannot return from top-level code",
                        statement.pos,
                    )));
                }

                match *r {
                    None => Ok(()),
                    Some(ref value) => {
                        if self.current_function == FunctionType::Init {
                            return Err(ResolverError::Init(self.error(
                                "Cannot return a value from an initializer",
                                statement.pos,
                            )));
                        }

                        self.resolve_expr(value, statement.pos)
                    }
                }
            }

            Statement::Var(ref variable, ref expression) => {
                self.declare(variable.clone(), statement.pos)?;

                match *expression {
                    Expression::Literal(Literal::Nil) => (),
                    _ => self.resolve_expr(expression, statement.pos)?,
                }

                self.define(variable.to_owned());
                Ok(())
            }

            Statement::Function { ref name, ref body } => {
                self.declare(name.clone(), statement.pos)?;
                self.define(name.clone());

                self.resolve_func(body, FunctionType::Function, statement.pos)?;
                Ok(())
            }

            Statement::Class {
                ref name,
                ref methods,
            } => {
                self.declare(name.clone(), statement.pos)?;
                self.define(name.clone());

                let enclosing_class = self.current_class;

                self.current_class = ClassType::Class;

                self.begin_scope();

                self.insert(Variable("this"), true);

                for method in methods {
                    let mut declaration = FunctionType::Method;

                    match method {
                        &WithPos { ref node, ref pos } => match node {
                            &Statement::Function { ref name, ref body } => {
                                if name.0 == "init" {
                                    declaration = FunctionType::Init;
                                }

                                self.resolve_func(body, declaration, *pos)?;
                            }
                            _ => unreachable!(),
                        },
                    };
                }

                self.current_class = enclosing_class;

                self.end_scope();

                Ok(())
            }
        }
    }
}
impl<'a> Resolver<'a> {
    fn resolve_expr(&mut self, expr: &Expression<'a>, pos: Postition) -> Result<(), ResolverError> {
        match *expr {
            Expression::Array { ref items } => {
                for ref item in items {
                    self.resolve_expr(item, pos)?;
                }
                Ok(())
            }

            Expression::Assign {
                ref handle,
                ref name,
                ref value,
                ..
            } => {
                self.resolve_expr(value, pos)?;
                self.resolve_local(name, *handle);
                Ok(())
            }

            Expression::Binary {
                ref left_expr,
                ref right_expr,
                ..
            } => {
                self.resolve_expr(left_expr, pos)?;
                self.resolve_expr(right_expr, pos)?;
                Ok(())
            }

            Expression::Call {
                ref callee,
                ref arguments,
            } => {
                self.resolve_expr(callee, pos)?;

                for ref argument in arguments {
                    self.resolve_expr(argument, pos)?;
                }

                Ok(())
            }

            Expression::Dict { ref items } => {
                for &(ref key, ref value) in items {
                    self.resolve_expr(key, pos)?;
                    self.resolve_expr(value, pos)?;
                }

                Ok(())
            }

            Expression::Func {
                ref parameters,
                ref body,
            } => {
                let enclosing_function = self.current_function;

                self.current_function = FunctionType::Function;

                self.begin_scope();


                for parameter in parameters {
                    self.declare(parameter.clone(), pos)?;
                    self.define(parameter.clone());
                }

                self.resolve_statement(body)?;

                self.end_scope();

                self.current_function = enclosing_function;
                Ok(())
            }

            Expression::Get { ref object, .. } => {
                self.resolve_expr(object, pos)?;
                Ok(())
            }

            Expression::Grouping { ref expr } => self.resolve_expr(expr, pos),

            Expression::IndexExpr {
                ref index,
                ref target,
            } => {
                self.resolve_expr(index, pos)?;
                self.resolve_expr(target, pos)?;
                Ok(())
            }

            Expression::Literal(_) => Ok(()),

            Expression::Logical {
                ref left,
                ref right,
                ..
            } => {
                self.resolve_expr(left, pos)?;
                self.resolve_expr(right, pos)?;
                Ok(())
            }

            Expression::Set {
                ref value,
                ref object,
                ..
            } => {
                self.resolve_expr(value, pos)?;
                self.resolve_expr(object, pos)?;
                Ok(())
            }

            Expression::Ternary {
                ref condition,
                ref then_branch,
                ref else_branch,
            } => {
                self.resolve_expr(condition, pos)?;
                self.resolve_expr(else_branch, pos)?;
                self.resolve_expr(then_branch, pos)?;
                Ok(())
            }

            Expression::Unary { ref expr, .. } => {
                self.resolve_expr(expr, pos)?;
                Ok(())
            }

            Expression::This(ref handle) => {
                if self.current_class == ClassType::None {
                    return Err(ResolverError::This(self.error(
                        "Cannot use 'this' outside of a class.",
                        pos,
                    )));
                }

                self.resolve_local(&Variable("this"), *handle);

                Ok(())
            }

            Expression::Var(ref v, ref handle) => {
                if self.not_resolved(v) {
                    let msg = format!("Cannot read local variable '{}' in its own initializer.", v);
                    return Err(ResolverError::ReadInInit(self.error(&msg, pos)));
                }

                self.resolve_local(v, *handle);
                Ok(())
            }
        }
    }
}


#[cfg(test)]
mod test {
    use ast::statement::Statement;
    use lexer::Lexer;
    use parser::Parser;
    use resolver::Resolver;
    use pos::WithPos;

    fn get_ast(input:&str) -> Vec<WithPos<Statement>> {
        let tokens = Lexer::new(input).lex().unwrap();
        Parser::new(tokens).parse().unwrap()
    }
    
    #[test]
    fn global() {
        let input = "var a = 0;{fun f() {print a;}}";
        assert!(Resolver::new().resolve(get_ast(input)).is_ok())
    }

    #[test]
    fn captured() {
        let input = "{var a = 0;fun f() {print a;}}";
        assert!(Resolver::new().resolve(get_ast(input)).is_ok())
    }

    #[test]
    fn lexical_capture() {
        let input = "var a = 0;{fun f() {print a;} var a = 1;}";
        assert!(Resolver::new().resolve(get_ast(input)).is_ok())
    }

    #[test]
    #[should_panic]
    fn shadowing_error() {
        let input = "var a = 0;{var a = a;}";
        Resolver::new().resolve(get_ast(input)).unwrap()
    }

    #[test]
    #[should_panic]
    fn local_redeclar_error() {
        let input = "{var a = 1;var a = 2;}";
        Resolver::new().resolve(get_ast(input)).unwrap()
    }

     #[test]
    fn global_redeclar() {
        let input = "var a = 1;var a = 2;";
         assert!(Resolver::new().resolve(get_ast(input)).is_ok())
    }

    #[test]
    #[should_panic]
    fn return_top_level() {
        let input = "return 10;";
        Resolver::new().resolve(get_ast(input)).unwrap()
    }

    #[test]
    #[should_panic]
    fn this_outside_class() {
        let input = "this.name;";
        Resolver::new().resolve(get_ast(input)).unwrap()
    }
    
}