use ast::expr::*;
use ast::statement::Statement;

use std::str;
use symbol::Symbols;

impl AssignOperator {
    fn pprint(&self) -> &'static str {
        match *self {
            AssignOperator::Equal => "=",
            AssignOperator::MinusEqual => "-=",
            AssignOperator::PlusEqual => "+=",
            AssignOperator::SlashEqual => "/=",
            AssignOperator::StarEqual => "*=",
        }
    }
}

impl Ty {
    fn pprint(&self, symbols: &mut Symbols<()>) -> String {
        match *self {
            Ty::Simple(ref s) => symbols.name(s.value),
            _ => unimplemented!(),
        }
    }
}

impl UnaryOp {
    fn pprint(&self) -> &'static str {
        match *self {
            UnaryOp::Bang => "!",
            UnaryOp::Minus => "-",
        }
    }
}

impl Literal {
    fn pprint(&self) -> String {
        match *self {
            Literal::Int(ref i) => i.to_string(),
            Literal::Float(ref f) => f.to_string(),
            Literal::Str(ref s) => str::from_utf8(s).unwrap().to_string(),
            Literal::False(_) => "false".into(),
            Literal::True(_) => "true".into(),
            Literal::Nil => "nil".into(),
        }
    }
}

impl Op {
    fn pprint(&self) -> &'static str {
        match *self {
            Op::BangEqual => "!=",
            Op::EqualEqual => "==",
            Op::LessThan => "<",
            Op::LessThanEqual => "<=",
            Op::GreaterThan => ">",
            Op::GreaterThanEqual => ">=",
            Op::Plus => "+",
            Op::Minus => "-",
            Op::Star => "*",
            Op::Slash => "/",
            Op::Modulo => "%",
            Op::Exponential => "^",
            Op::And => "and",
            Op::Or => "or",
        }
    }
}

impl Statement {
    pub fn pprint(&self, symbols: &mut Symbols<()>) -> String {
        let mut pprinted = String::new();

        self.pprint_into(&mut pprinted, symbols);

        pprinted
    }

    fn pprint_into(&self, pprint_string: &mut String, symbols: &mut Symbols<()>) {
        match *self {
            Statement::Expr(ref expr) | Statement::Print(ref expr) => {
                expr.value.pprint_into(pprint_string, symbols);
            }

            Statement::TypeAlias { ref alias, ref ty } => {
                pprint_string.push_str("(type ");
                pprint_string.push_str(&symbols.name(alias.value));
                pprint_string.push_str("=");
                pprint_string.push_str(&ty.value.pprint(symbols));

                pprint_string.push_str(" )");
            }
            Statement::Var {
                ref ident,
                ref ty,
                ref expr,
            } => {
                pprint_string.push_str("(var ");
                pprint_string.push_str(&symbols.name(ident.value));

                pprint_string.push_str(" => ");

                if let Some(ref var_ty) = *ty {
                    pprint_string.push_str(":");
                    pprint_string.push_str(&var_ty.value.pprint(symbols));
                }

                if let Some(ref expr) = *expr {
                    expr.value.pprint_into(pprint_string, symbols);
                }

                pprint_string.push_str(" ) )");
            }

            Statement::Block(ref statements) => {
                pprint_string.push_str("(block ");
                for statement in statements {
                    statement.value.pprint_into(pprint_string, symbols);
                }
                pprint_string.push_str(" )");
            }

            Statement::Class {
                ref name,
                ref superclass,
                ref body,
            } => {
                pprint_string.push_str("(class ");
                pprint_string.push_str(&symbols.name(name.value));

                if let Some(ref sclass) = *superclass {
                    pprint_string.push_str("< ");
                    pprint_string.push_str(&symbols.name(sclass.value));
                }

                if !body.value.0.is_empty() {
                    pprint_string.push_str(" (methods => ");

                    for method in &body.value.0 {
                        method.value.pprint_into(pprint_string, symbols);
                    }

                    pprint_string.push_str(") ");
                }

                if body.value.1.is_empty() {
                    pprint_string.push_str(" (props => ");

                    for property in &body.value.1 {
                        pprint_string.push_str("( ");
                        pprint_string.push_str(&symbols.name(property.value.name.value));
                        pprint_string.push_str(":");
                        pprint_string.push_str(&property.value.ty.value.pprint(symbols));
                        pprint_string.push_str(")");
                    }
                    pprint_string.push_str(") ");
                }

                pprint_string.push_str(")");
            }

            Statement::If {
                ref cond,
                ref otherwise,
                ref then,
            } => {
                pprint_string.push_str("(if ");

                cond.value.pprint_into(pprint_string, symbols);

                then.value.pprint_into(pprint_string, symbols);

                if let Some(ref else_) = *otherwise {
                    else_.value.pprint_into(pprint_string, symbols);
                }

                pprint_string.push_str(" )");
            }

            Statement::For {
                ref incr,
                ref cond,
                ref init,
                ref body,
            } => {
                pprint_string.push_str("(for ");

                if let Some(ref incr) = *incr {
                    incr.value.pprint_into(pprint_string, symbols);
                }

                if let Some(ref cond) = *cond {
                    cond.value.pprint_into(pprint_string, symbols);
                }

                if let Some(ref init) = *init {
                    init.value.pprint_into(pprint_string, symbols);
                }

                body.value.pprint_into(pprint_string, symbols);

                pprint_string.push_str(" )");
            }

            Statement::Function {
                ref name,
                ref body,
                ref params,
                ref returns,
            } => {
                pprint_string.push_str("(fun ");
                pprint_string.push_str(&symbols.name(name.value));

                pprint_string.push_str(" params (");

                for param in &params.value {
                    pprint_string.push_str(&symbols.name(param.value.name.value));
                    pprint_string.push_str(":");
                    pprint_string.push_str(&param.value.ty.value.pprint(symbols));
                    pprint_string.push_str(" ");
                }

                pprint_string.push_str(")");

                pprint_string.push_str("body (");

                body.value.pprint_into(pprint_string, symbols);

                pprint_string.push_str(")");

                pprint_string.push_str("-> ");

                if let Some(ref ty) = *returns {
                    ty.value.pprint(symbols);
                }

                pprint_string.push_str(" )");
            }

            Statement::While { ref body, ref cond } => {
                pprint_string.push_str("(while ");

                cond.value.pprint_into(pprint_string, symbols);
                body.value.pprint_into(pprint_string, symbols);

                pprint_string.push_str(" )");
            }

            Statement::Break => {
                pprint_string.push_str("( break )");
            }

            Statement::Continue => {
                pprint_string.push_str("( continue )");
            }

            Statement::Return(ref returns) => {
                pprint_string.push_str("(return ");

                returns.value.pprint_into(pprint_string, symbols);

                pprint_string.push_str(" )");
            }
        }
    }
}

impl Expression {
    fn pprint_into(&self, pprint_string: &mut String, symbols: &mut Symbols<()>) {
        match *self {
            Expression::Array { ref items, .. } => {
                pprint_string.push_str("(array");
                for item in items {
                    item.value.pprint_into(pprint_string, symbols);
                }
                pprint_string.push_str(" )");
            }

            Expression::Assign {
                ref name,
                ref value,
                ref kind,
                ..
            } => {
                pprint_string.push_str("(");
                pprint_string.push_str(&symbols.name(name.value));
                pprint_string.push_str(kind.value.pprint());
                value.value.pprint_into(pprint_string, symbols);
                pprint_string.push_str(" )");
            }
            Expression::Binary {
                ref lhs,
                ref rhs,
                ref op,
            } => {
                pprint_string.push_str("(");
                pprint_string.push_str(op.value.pprint());
                lhs.value.pprint_into(pprint_string, symbols);
                rhs.value.pprint_into(pprint_string, symbols);
                pprint_string.push_str(" )");
            }

            Expression::Call {
                ref callee,
                ref args,
            } => {
                pprint_string.push_str("(call ");
                callee.value.pprint_into(pprint_string, symbols);

                for argument in args {
                    argument.value.pprint_into(pprint_string, symbols);
                }

                pprint_string.push_str(" )");
            }

            Expression::ClassInstance {
                ref props,
                ref symbol,
            } => {
                pprint_string.push_str("( ");
                pprint_string.push_str(&symbols.name(symbol.value));
                pprint_string.push_str(" ");

                pprint_string.push_str("(props (");

                for prop in props.iter() {
                    pprint_string.push_str(&symbols.name(prop.value.symbol.value));
                    pprint_string.push_str(" =>");
                    prop.value.expr.value.pprint_into(pprint_string, symbols);
                }

                pprint_string.push_str(") ) )");
            }

            Expression::Get {
                ref object,
                ref property,
                ..
            } => {
                pprint_string.push_str("( get (");
                pprint_string.push_str(&symbols.name(property.value));
                object.value.pprint_into(pprint_string, symbols);
                pprint_string.push_str("))");
            }

            Expression::Grouping { ref expr } => {
                expr.value.pprint_into(pprint_string, symbols);
            }

            Expression::Index {
                ref index,
                ref target,
            } => {
                index.value.pprint_into(pprint_string, symbols);
                target.value.pprint_into(pprint_string, symbols);
            }

            Expression::Literal(ref literal) => {
                pprint_string.push_str(" ");
                pprint_string.push_str(&literal.pprint());
                pprint_string.push_str(" ");
            }

            Expression::Set {
                ref object,
                ref name,
                ref value,
                ..
            } => {
                pprint_string.push_str("( set ");
                pprint_string.push_str(&symbols.name(name.value));
                object.value.pprint_into(pprint_string, symbols);
                value.value.pprint_into(pprint_string, symbols);
                pprint_string.push_str(")");
            }

            Expression::Ternary {
                ref condition,
                ref then_branch,
                ref else_branch,
            } => {
                pprint_string.push_str("(");
                condition.value.pprint_into(pprint_string, symbols);
                pprint_string.push_str("?");
                then_branch.value.pprint_into(pprint_string, symbols);
                pprint_string.push_str(":");
                else_branch.value.pprint_into(pprint_string, symbols);
                pprint_string.push_str(" )");
            }

            Expression::Unary { ref expr, ref op } => {
                pprint_string.push_str(op.value.pprint());
                expr.value.pprint_into(pprint_string, symbols);
            }

            Expression::This(_) => {
                pprint_string.push_str("this");
            }

            Expression::Var(ref v, ..) => {
                pprint_string.push_str(" ");
                pprint_string.push_str(&symbols.name(v.value));
            }
        }
    }
}

#[cfg(test)]
mod test {
    use lexer::Lexer;
    use parser::Parser;
    use symbol::{SymbolFactory, Symbols};
    use util::emmiter::Reporter;

    #[test]
    fn it_works() {
        let input = "var a =0;";
        let reporter = Reporter::new();
        use std::rc::Rc;
        let tokens = Lexer::new(input, reporter.clone()).lex().unwrap();
        let strings = Rc::new(SymbolFactory::new());
        let mut symbols = Symbols::new(strings);
        let ast = Parser::new(tokens, reporter.clone(), &mut symbols)
            .parse()
            .unwrap();

        for statement in ast {
            statement.value.pprint(&mut symbols);
        }
    }
}
