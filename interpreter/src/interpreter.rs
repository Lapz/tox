use super::object::Object;
use fnv::FnvHashMap;
use interpreter::env::Environment;
use std::cell::RefCell;
use std::rc::Rc;
use syntax::ast::*;
use util::pos::{Span, Spanned, EMPTYSPAN};
use util::symbol::Symbol;
use util::symbol::Symbols;

#[derive(Debug)]
pub enum ErrorCode {
    Break,
    Continue,
    IndexOutOfBound,
    InvalidIndexType,
    NotAnInstance,
    UndefinedProperty(Symbol),
    CantParseAsInt(Vec<u8>),
    UndefinedSymbol(Symbol),
    Return(Box<Object>),
}

#[derive(Debug)]
pub struct RuntimeError {
    pub code: ErrorCode,
    pub span: Option<Span>,
}

impl RuntimeError {
    pub fn new(code: ErrorCode, span: Span) -> Self {
        Self {
            code,
            span: Some(span),
        }
    }

    pub fn new_without_span(code: ErrorCode) -> Self {
        Self { code, span: None }
    }
}

impl ErrorCode {
    pub fn reason(&self, symbols: &Symbols<()>) -> String {
        match *self {
            ErrorCode::UndefinedSymbol(ref symbol) => {
                format!("Undefined variable '{}' ", symbols.name(*symbol))
            }

            ErrorCode::UndefinedProperty(ref symbol) => {
                format!("Undefined property '{}' ", symbols.name(*symbol))
            }

            ErrorCode::IndexOutOfBound => {
                format!("An invalid index was used when trying to access an item from an array")
            }

            ErrorCode::InvalidIndexType => format!("The type of an index should be an integer"),
            ErrorCode::CantParseAsInt(ref bytes) => format!(
                "Cannot parse `{}` as an int",
                ::std::str::from_utf8(bytes).unwrap()
            ),

            ErrorCode::NotAnInstance => format!("A class instance was expected"),
            _ => format!(""),
        }
    }
}

pub(crate) fn evaluate_function(
    function: &Function,
    env: &mut Environment,
) -> Result<Object, RuntimeError> {
    let mut func_params = Vec::with_capacity(function.params.value.len());

    for param in function.params.value.iter() {
        func_params.push(param.value.name.value);
    }

    env.define(
        function.name.value,
        Object::Function(
            function.name.value,
            func_params.clone(),
            Box::new(function.body.clone()),
            Box::new(env.clone()),
        ),
    );

    Ok(Object::Function(
        function.name.value,
        func_params,
        Box::new(function.body.clone()),
        Box::new(env.clone()),
    ))
}

pub(crate) fn evaluate_class(class: &Class, env: &mut Environment) -> Result<Object, RuntimeError> {
    use std::mem;

    env.define(class.name.value, Object::Nil);
    let mut class_methods: FnvHashMap<Symbol, Object> = FnvHashMap::default();
    let mut sklass = None;

    let mut s = false;

    let old_env = env.clone();

    if let Some(ref super_class_name) = class.superclass {
        let new_env = Environment::new_with_outer(env);
        mem::replace(env, new_env);

        s = true;

        let superclass = env.get(&super_class_name)?;

        sklass = Some(Box::new(env.get(&super_class_name)?));
        env.define(Symbol(1), superclass);
    }

    for method in class.methods.iter() {
        let mut func_params = Vec::with_capacity(method.value.params.value.len());

        for param in method.value.params.value.iter() {
            func_params.push(param.value.name.value);
        }

        class_methods.insert(
            method.value.name.value,
            Object::Function(
                method.value.name.value,
                func_params,
                Box::new(method.value.body.clone()),
                Box::new(env.clone()),
            ),
        );
    }

    if s {
        mem::replace(env, old_env);
    }

    env.define(
        class.name.value,
        Object::Class(class.name.value, sklass, class_methods),
    );
    Ok(Object::None)
}
pub(crate) fn evaluate_statement(
    statement: &Spanned<Statement>,
    env: &mut Environment,
) -> Result<Object, RuntimeError> {
    match statement.value {
        Statement::Block(ref statements) => {
            let mut environment = Environment::new_with_outer(env);

            let mut result = Object::None;

            for statement in statements {
                let object = evaluate_statement(statement, &mut environment);

                match object {
                    Ok(Object::Return(_)) => return object,
                    Ok(val) => result = val,
                    Err(_) => return object,
                }
            }

            Ok(result)
        }

        Statement::Break => Err(RuntimeError::new(ErrorCode::Break, statement.span)),
        Statement::Continue => Err(RuntimeError::new(ErrorCode::Continue, statement.span)),
        Statement::While { ref body, ref cond } => {
            while evaluate_expression(cond, env)?.is_truthy() {
                match evaluate_statement(body, env) {
                    Ok(value) => match value {
                        Object::Return(_) => return Ok(value),
                        _ => (),
                    },
                    Err(e) => match e.code {
                        ErrorCode::Break => break,
                        ErrorCode::Continue => continue,
                        _ => return Err(e),
                    },
                };
            }

            Ok(Object::None)
        }

        Statement::Expr(ref expr) => evaluate_expression(expr, env),

        Statement::For {
            ref init,
            ref cond,
            ref incr,
            ref body,
        } => {
            if init.is_none() && cond.is_none() && incr.is_none() {
                loop {
                    match evaluate_statement(body, env) {
                        Ok(value) => match value {
                            Object::Return(_) => return Ok(value),
                            _ => value,
                        },
                        Err(e) => match e.code {
                            ErrorCode::Break => break,
                            ErrorCode::Continue => continue,
                            _ => return Err(e),
                        },
                    };
                }
                return Ok(Object::None);
            }

            if let Some(ref init) = *init {
                evaluate_statement(init, env)?;
            }

            if let Some(ref cond) = *cond {
                while evaluate_expression(cond, env)?.is_truthy() {
                    match evaluate_statement(body, env) {
                        Ok(value) => match value {
                            Object::Return(_) => return Ok(value),
                            _ => value,
                        },
                        Err(e) => match e.code {
                            ErrorCode::Break => break,
                            ErrorCode::Continue => continue,
                            _ => return Err(e),
                        },
                    };

                    if let Some(ref inc) = *incr {
                        evaluate_expression(inc, env)?;
                    }
                }
            }

            Ok(Object::None)
        }

        Statement::Print(ref expr) => {
            use std::io;
            use std::io::prelude::*;
            let value = evaluate_expression(expr, env)?;

            println!("{}", value.as_string());
            let _ = io::stdout().flush();

            Ok(Object::None)
        }

        Statement::Return(ref expr) => {
            Ok(Object::Return(Box::new(evaluate_expression(expr, env)?)))
        }

        Statement::If {
            ref cond,
            ref otherwise,
            ref then,
        } => {
            if evaluate_expression(cond, env)?.is_truthy() {
                evaluate_statement(then, env)
            } else if let Some(ref else_statement) = *otherwise {
                evaluate_statement(else_statement, env)
            } else {
                Ok(Object::None)
            }
        }

        Statement::VarDeclaration {
            ref ident,
            ref expr,
            ..
        } => {
            if let Some(ref expr) = *expr {
                let value = evaluate_expression(expr, env)?;
                env.define(ident.value, value);
            } else {
                env.define(ident.value, Object::Nil);
            }

            Ok(Object::Nil)
        }
    }
}

fn evaluate_expression(
    expression: &Spanned<Expression>,
    env: &mut Environment,
) -> Result<Object, RuntimeError> {
    match expression.value {
        Expression::Array { ref items, .. } => {
            let mut values = Vec::with_capacity(items.len());
            for item in items {
                values.push(evaluate_expression(item, env)?)
            }
            Ok(Object::Array(values))
        }

        Expression::Assign {
            ref name,
            ref kind,
            ref value,
        } => {
            let mut value = evaluate_expression(value, env)?;

            match kind.value {
                AssignOperator::Equal => (),

                AssignOperator::PlusEqual => {
                    let current = env.get(name)?;

                    match (current, value) {
                        (Object::Int(x), Object::Int(y)) => value = Object::Int(x + y),
                        (Object::Float(x), Object::Float(y)) => value = Object::Float(x + y),
                        _ => unreachable!(),
                    }
                }

                AssignOperator::MinusEqual => {
                    let current = env.get(name)?;

                    match (current, value) {
                        (Object::Int(x), Object::Int(y)) => value = Object::Int(x - y),
                        (Object::Float(x), Object::Float(y)) => value = Object::Float(x - y),
                        _ => unreachable!(),
                    }
                }

                AssignOperator::SlashEqual => {
                    let current = env.get(name)?;

                    match (current, value) {
                        (Object::Int(x), Object::Int(y)) => value = Object::Int(x / y),
                        (Object::Float(x), Object::Float(y)) => value = Object::Float(x / y),
                        _ => unreachable!(),
                    }
                }

                AssignOperator::StarEqual => {
                    let current = env.get(name)?;

                    match (current, value) {
                        (Object::Int(x), Object::Int(y)) => value = Object::Int(x * y),
                        (Object::Float(x), Object::Float(y)) => value = Object::Float(x * y),
                        _ => unreachable!(),
                    }
                }
            }

            env.assign(name, value.clone())?;

            Ok(value)
        }

        Expression::Binary {
            ref lhs,
            ref op,
            ref rhs,
        } => {
            let left = evaluate_expression(lhs, env)?;
            let right = evaluate_expression(rhs, env)?;

            match op.value {
                Op::BangEqual => Ok(Object::Bool(left != right)),
                Op::EqualEqual => Ok(Object::Bool(left == right)),
                Op::LessThan => Ok(Object::Bool(left < right)),
                Op::LessThanEqual => Ok(Object::Bool(left <= right)),
                Op::GreaterThan => Ok(Object::Bool(left > right)),
                Op::GreaterThanEqual => Ok(Object::Bool(left >= right)),
                Op::Plus => add(left, right),
                Op::Minus => minus(left, right),
                Op::Star => times(left, right),
                Op::Slash => divide(left, right),
                Op::Modulo => modulo(left, right),
                Op::Exponential => expon(left, right),
                Op::And => {
                    if !left.is_truthy() {
                        Ok(left)
                    } else {
                        Ok(right)
                    }
                }
                Op::Or => {
                    if left.is_truthy() {
                        Ok(left)
                    } else {
                        Ok(right)
                    }
                }
            }
        }

        Expression::Call {
            ref callee,
            ref args,
        } => {
            let callee = evaluate_expression(callee, env)?;

            let mut obj_arguments = Vec::with_capacity(args.len());

            for arg in args {
                obj_arguments.push(evaluate_expression(arg, env)?);
            }

            callee.call(&obj_arguments)
        }
        Expression::ClassInstance {
            ref symbol,
            ref props,
        } => match env.get(symbol)? {
            Object::Class(_, ref superclass, ref methods) => {
                let mut instance_props: FnvHashMap<Symbol, Object> = FnvHashMap::default();
                let mut s_class_methods = None;

                if let Some(ref sklass) = *superclass {
                    match **sklass {
                        Object::Class(_, _, ref methods_) => {
                            s_class_methods = Some(methods_.clone());
                        }
                        _ => unimplemented!(),
                    }
                }

                for prop in props.iter() {
                    let value = evaluate_expression(&prop.value.expr, env)?;
                    instance_props.insert(prop.value.symbol.value, value);
                }

                env.define(
                    symbol.value,
                    Object::Instance {
                        methods: methods.clone(),
                        fields: Rc::new(RefCell::new(instance_props.clone())),
                        sclassmethods: s_class_methods.clone(),
                    },
                );

                Ok(Object::Instance {
                    methods: methods.clone(),
                    fields: Rc::new(RefCell::new(instance_props)),
                    sclassmethods: s_class_methods,
                })
            }

            _ => unimplemented!(),
        },

        Expression::Closure(ref function) => evaluate_function(&function.value, env),

        Expression::Literal(ref lit) => evaluate_literal(lit),
        Expression::Get {
            ref object,
            ref property,
            ..
        } => {
            let object = evaluate_expression(object, env)?;

            match object {
                instance @ Object::Instance { .. } => instance.get_property(property, env),
                class @ Object::Class(_, _, _) => class.get_property(property, env),
                ref e => {
                    println!("{:?}", e);
                    Err(RuntimeError::new(ErrorCode::NotAnInstance, expression.span))
                }
            }
        }
        Expression::Grouping { ref expr } => evaluate_expression(expr, env),
        Expression::SubScript {
            ref target,
            ref index,
        } => {
            let target = evaluate_expression(target, env)?;
            let index = evaluate_expression(index, env)?;

            match target {
                Object::Array(r) => {
                    let index = match index {
                        Object::Int(i) => i,
                        _ => unreachable!(),
                    };

                    if index >= (r.len() as i64) || index < 0 {
                        return Err(RuntimeError::new(
                            ErrorCode::IndexOutOfBound,
                            expression.span,
                        ));
                    }

                    Ok(r[index as usize].to_owned())
                }
                _ => unimplemented!(),
            }
        }

        Expression::Set {
            ref object,
            ref name,
            ref value,
            ..
        } => {
            let object = evaluate_expression(object, env)?;
            let value = evaluate_expression(value, env)?;

            match object {
                mut instance @ Object::Instance { .. } => {
                    instance.set(name.value, &value);
                }
                _ => return Err(RuntimeError::new(ErrorCode::NotAnInstance, expression.span)),
            }

            Ok(value)
        }

        Expression::Var(ref symbol) => env.get(symbol),

        Expression::Ternary {
            ref condition,
            ref then_branch,
            ref else_branch,
        } => {
            let cond = evaluate_expression(condition, env)?;

            if cond.is_truthy() {
                evaluate_expression(then_branch, env)
            } else {
                evaluate_expression(else_branch, env)
            }
        }

        Expression::This => env.get(&Spanned::new(Symbol(0), EMPTYSPAN)),

        Expression::Unary { ref op, ref expr } => {
            let right = evaluate_expression(expr, env)?;

            match op.value {
                UnaryOp::Minus => match right {
                    Object::Float(f) => Ok(Object::Float(-f)),
                    Object::Int(i) => Ok(Object::Int(-i)),
                    _ => unreachable!(),
                },
                UnaryOp::Bang => Ok(!right),
            }
        }
    }
}

fn add(lhs: Object, rhs: Object) -> Result<Object, RuntimeError> {
    match (lhs, rhs) {
        (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l + r)),
        (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l + r)),
        (Object::Str(mut l), Object::Str(r)) => {
            l.extend(r.iter());

            Ok(Object::Str(l))
        }
        _ => unreachable!(),
    }
}

fn times(lhs: Object, rhs: Object) -> Result<Object, RuntimeError> {
    match (lhs, rhs) {
        (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l * r)),
        (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l * r)),
        (lhs,rhs) => unreachable!("{:?} {:?}",lhs,rhs),
    }
}

#[inline]
fn modulo(lhs: Object, rhs: Object) -> Result<Object, RuntimeError> {
    match (lhs, rhs) {
        (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l % r)),
        (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l % r)),
        _ => unreachable!(),
    }
}

#[inline]
fn expon(lhs: Object, rhs: Object) -> Result<Object, RuntimeError> {
    match (lhs, rhs) {
        (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l.powf(r))),
        (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l.pow(r as u32))),
        _ => unreachable!(),
    }
}
#[inline]
fn minus(lhs: Object, rhs: Object) -> Result<Object, RuntimeError> {
    match (lhs, rhs) {
        (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l - r)),
        (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l - r)),
        _ => unreachable!(),
    }
}
#[inline]
fn divide(lhs: Object, rhs: Object) -> Result<Object, RuntimeError> {
    match (lhs, rhs) {
        (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l / r)),
        (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l / r)),
        _ => unreachable!(),
    }
}

fn evaluate_literal(expression: &Literal) -> Result<Object, RuntimeError> {
    match *expression {
        Literal::Float(i) => Ok(Object::Float(i)),
        Literal::Int(i) => Ok(Object::Int(i)),
        Literal::Str(ref s) => Ok(Object::Str(s.clone().as_bytes().to_vec())),
        Literal::Nil => Ok(Object::Nil),
        Literal::True(ref b) | Literal::False(ref b) => Ok(Object::Bool(*b)),
    }
}

pub mod env {

    use super::{ErrorCode, RuntimeError};
    use builtins::BuiltIn;
    use object::Object;
    use util::symbol::Symbol;

    use std::rc::Rc;

    use fnv::FnvHashMap;
    use std::cell::RefCell;
    use util::pos::Spanned;
    use util::symbol::Symbols;

    #[derive(Debug, Clone, Default)]
    /// A Lox enviroment
    /// which contains a map of a Symbol and value and the scope in which a
    /// Symbol was declared
    pub struct Environment {
        actual: Rc<RefCell<EnvironmentImpl>>,
    }

    impl PartialEq for Environment {
        fn eq(&self, other: &Environment) -> bool {
            self.actual.borrow().values == other.actual.borrow().values
        }
    }

    impl Environment {
        pub fn new() -> Self {
            Self::default()
        }

        pub fn new_with_outer(outer: &Environment) -> Environment {
            Environment {
                actual: Rc::new(RefCell::new(EnvironmentImpl::with_values(outer))),
            }
        }

        pub fn fill_env(&mut self, symbols: &mut Symbols<()>) {
            let built_in = BuiltIn::new().get_built_ins(symbols);

            for (variable, run_time_value) in built_in {
                self.define(variable, run_time_value);
            }
        }

        /// Defines a Symbol by inserting the name and
        /// value into the environmentImpl
        pub fn define(&self, name: Symbol, value: Object) {
            self.actual.borrow_mut().values.insert(name, value);
        }

        pub fn remove(&self, name: &Symbol) {
            self.actual.borrow_mut().values.remove(name);
        }

        /// Takes a Symbol and a value.
        /// Checks if the value exsits and inserts into the actual values if not,
        /// searches the outer scope and then calls itselfs.
        /// Returns a runtime error if the not found
        pub fn assign(&self, name: &Spanned<Symbol>, value: Object) -> Result<(), RuntimeError> {
            let mut actual = self.actual.borrow_mut();

            if actual.values.contains_key(&name.value) {
                *actual.values.get_mut(&name.value).unwrap() = value;
                Ok(())
            } else {
                match actual.outer {
                    Some(ref outer) => outer.assign(name, value),
                    None => Err(RuntimeError::new(
                        ErrorCode::UndefinedSymbol(name.value),
                        name.span,
                    )),
                }
            }
        }

        /// Takes a Symbol and a value.
        /// Checks if the value exists and if not,
        /// searches the outer scope and then calls itselfs.
        /// Returns a runtime error if the not found
        pub fn get(&self, name: &Spanned<Symbol>) -> Result<Object, RuntimeError> {
            let actual = self.actual.borrow();

            if let Some(value) = actual.values.get(&name.value) {
                return Ok(value.clone());
            } else {
                match actual.outer {
                    Some(ref outer) => outer.get(name),
                    None => Err(RuntimeError::new(
                        ErrorCode::UndefinedSymbol(name.value),
                        name.span,
                    )),
                }
            }
        }

        pub fn dump(&self) {
            let actual = self.actual.borrow();
            println!("-----------ENVIRONMENT-------");
            for (name, value) in &actual.values {
                println!("name:{}, value:{}", name, value)
            }

            if let Some(ref outer) = actual.outer {
                outer.dump()
            }
        }
    }

    /// The actual struct that contains the values.
    /// It is wrapped within a Rc<RefCell<_>>
    #[derive(Debug, Default)]
    pub(crate) struct EnvironmentImpl {
        outer: Option<Environment>,
        values: FnvHashMap<Symbol, Object>,
    }

    impl EnvironmentImpl {
        fn with_values(outer: &Environment) -> Self {
            EnvironmentImpl {
                outer: Some(outer.clone()),
                values: FnvHashMap::default(),
            }
        }
    }

}
