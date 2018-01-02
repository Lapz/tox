use ast::expr::*;
use ast::statement::Statement;

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

impl ExpressionTy {
    fn pprint(&self, symbols: &mut Symbols<()>) -> String {
        match *self {
            ExpressionTy::Simple(s) => symbols.name(s),
            _ => unimplemented!(),
        }
    }
}
impl LogicOperator {
    fn pprint(&self) -> &'static str {
        match *self {
            LogicOperator::Or => "or",
            LogicOperator::And => "and",
        }
    }
}

impl UnaryOperator {
    fn pprint(&self) -> &'static str {
        match *self {
            UnaryOperator::Bang => "!",
            UnaryOperator::Minus => "-",
        }
    }
}

impl Literal {
    fn pprint(&self) -> String {
        match *self {
            Literal::Int(ref i) => i.to_string(),
            Literal::Float(ref f) => f.to_string(),
            Literal::Str(ref s) => s.to_string(),
            Literal::False(_) => "false".into(),
            Literal::True(_) => "true".into(),
            Literal::Nil => "nil".into(),
        }
    }
}

impl Operator {
    fn pprint(&self) -> &'static str {
        match *self {
            Operator::BangEqual => "!=",
            Operator::EqualEqual => "==",
            Operator::LessThan => "<",
            Operator::LessThanEqual => "<=",
            Operator::GreaterThan => ">",
            Operator::GreaterThanEqual => ">=",
            Operator::Plus => "+",
            Operator::Minus => "-",
            Operator::Star => "*",
            Operator::Slash => "/",
            Operator::Modulo => "%",
            Operator::Exponential => "^",
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
            Statement::ExpressionStmt(ref expr) => {
                expr.node.pprint_into(pprint_string, symbols);
            }

            Statement::Print(ref expr) => {
                expr.node.pprint_into(pprint_string, symbols);
            }
            Statement::TypeAlias { ref alias, ref ty } => {
                pprint_string.push_str("(type ");
                pprint_string.push_str(&symbols.name(*alias));
                pprint_string.push_str("=");
                pprint_string.push_str(&ty.pprint(symbols));

                pprint_string.push_str(" )");
            }
            Statement::Var(ref name, ref expr, ref ty) => {
                pprint_string.push_str("(var ");
                pprint_string.push_str(&symbols.name(*name));

                if let &Some(ref var_ty) = ty {
                    pprint_string.push_str(":");
                    pprint_string.push_str(&var_ty.pprint(symbols));
                }

                expr.node.pprint_into(pprint_string, symbols);

                pprint_string.push_str(" )");
            }

            Statement::Block(ref statements) => {
                pprint_string.push_str("(block ");
                for statement in statements {
                    statement.node.pprint_into(pprint_string, symbols);
                }
                pprint_string.push_str(" )");
            }

            Statement::Class {
                ref name,
                ref methods,
                ref properties,
            } => {
                pprint_string.push_str("(class ");
                pprint_string.push_str(&symbols.name(*name));

                for method in methods {
                    method.node.pprint_into(pprint_string, symbols);
                }

                pprint_string.push_str("(properties:");

                for property in properties {
                    pprint_string.push_str(" ");
                    pprint_string.push_str(&symbols.name(property.0));
                    pprint_string.push_str(": ");
                    pprint_string.push_str(&property.1.pprint(symbols));
                    pprint_string.push_str(",");
                }

                pprint_string.push_str(")");

                pprint_string.push_str(" )");
            }

            Statement::IfStmt {
                ref condition,
                ref then_branch,
                ref else_branch,
            } => {
                pprint_string.push_str("(if ");

                condition.node.pprint_into(pprint_string, symbols);

                then_branch.node.pprint_into(pprint_string, symbols);

                if let &Some(ref else_) = else_branch {
                    else_.node.pprint_into(pprint_string, symbols);
                }

                pprint_string.push_str(" )");
            }

            Statement::ForStmt {
                ref initializer,
                ref condition,
                ref increment,
                ref body,
            } => {
                pprint_string.push_str("(for ");

                if let &Some(ref init) = initializer {
                    init.node.pprint_into(pprint_string, symbols);
                }

                if let &Some(ref cond) = condition {
                    cond.node.pprint_into(pprint_string, symbols);
                }

                if let &Some(ref inc) = increment {
                    inc.node.pprint_into(pprint_string, symbols);
                }

                body.node.pprint_into(pprint_string, symbols);

                pprint_string.push_str(" )");
            }

            Statement::Function { ref name, ref body } => {
                pprint_string.push_str("(fun ");
                pprint_string.push_str(&symbols.name(*name));

                body.node.pprint_into(pprint_string, symbols);

                pprint_string.push_str(" )");
            }

            Statement::WhileStmt {
                ref body,
                ref condition,
            } => {
                pprint_string.push_str("(while ");

                condition.node.pprint_into(pprint_string, symbols);
                body.node.pprint_into(pprint_string, symbols);

                pprint_string.push_str(" )");
            }

            Statement::DoStmt {
                ref body,
                ref condition,
            } => {
                pprint_string.push_str("(do ");

                body.node.pprint_into(pprint_string, symbols);

                pprint_string.push_str("(while ");

                condition.node.pprint_into(pprint_string, symbols);

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

                if let &Some(ref value) = returns {
                    value.node.pprint_into(pprint_string, symbols);
                }

                pprint_string.push_str(" )");
            }
        }
    }
}

impl Expression {
    fn pprint_into(&self, pprint_string: &mut String, symbols: &mut Symbols<()>) {
        match *self {
            Expression::Array { ref items } => {
                pprint_string.push_str("(array");
                for ref item in items {
                    item.node.pprint_into(pprint_string, symbols);
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
                pprint_string.push_str(&symbols.name(*name));
                pprint_string.push_str(kind.pprint());
                value.node.pprint_into(pprint_string, symbols);
                pprint_string.push_str(" )");
            }
            Expression::Binary {
                ref left_expr,
                ref right_expr,
                ref operator,
            } => {
                pprint_string.push_str("(");
                pprint_string.push_str(operator.pprint());
                left_expr.node.pprint_into(pprint_string, symbols);
                right_expr.node.pprint_into(pprint_string, symbols);
                pprint_string.push_str(" )");
            }

            Expression::Call {
                ref callee,
                ref arguments,
            } => {
                pprint_string.push_str("(call ");
                callee.node.pprint_into(pprint_string, symbols);

                for argument in arguments {
                    argument.node.pprint_into(pprint_string, symbols);
                }

                pprint_string.push_str(" )");
            }

            Expression::ClassInstance {
                ref properties,
                ref name,
            } => {
                pprint_string.push_str("(instance ");
                pprint_string.push_str(&symbols.name(*name));
                pprint_string.push_str(" ");

                for &(ref key, ref value) in properties {
                    pprint_string.push_str(&symbols.name(*key));
                    pprint_string.push_str(".");
                    value.node.pprint_into(pprint_string, symbols);
                }

                pprint_string.push_str(" )");
            }

            Expression::Dict { ref items } => {
                pprint_string.push_str("(dict ");

                for &(ref key, ref value) in items {
                    key.node.pprint_into(pprint_string, symbols);

                    pprint_string.push_str(":");

                    value.node.pprint_into(pprint_string, symbols);
                }

                pprint_string.push_str(" )");
            }

            Expression::Func {
                ref parameters,
                ref body,
                ..
            } => {
                pprint_string.push_str("(");

                for item in parameters {
                    pprint_string.push_str(&symbols.name(item.0));
                    pprint_string.push_str(":");
                    pprint_string.push_str(&item.1.pprint(symbols));
                    pprint_string.push_str(" ");
                }

                body.node.pprint_into(pprint_string, symbols);

                pprint_string.push_str(")");
            }

            Expression::Get {
                ref object,
                ref property,
                ..
            } => {
                pprint_string.push_str("( get (");
                pprint_string.push_str(&symbols.name(*property));
                object.node.pprint_into(pprint_string, symbols);
                pprint_string.push_str("))");
            }

            Expression::Grouping { ref expr } => {
                expr.node.pprint_into(pprint_string, symbols);
            }

            Expression::IndexExpr {
                ref index,
                ref target,
            } => {
                index.node.pprint_into(pprint_string, symbols);
                target.node.pprint_into(pprint_string, symbols);
            }

            Expression::Literal(ref literal) => {
                pprint_string.push_str(" ");
                pprint_string.push_str(&literal.pprint());
                pprint_string.push_str(" ");
            }

            Expression::Logical {
                ref left,
                ref right,
                ref operator,
            } => {
                pprint_string.push_str("(");

                pprint_string.push_str(operator.pprint());

                left.node.pprint_into(pprint_string, symbols);

                right.node.pprint_into(pprint_string, symbols);
                pprint_string.push_str(")");
            }

            Expression::Set {
                ref object,
                ref name,
                ref value,
                ..
            } => {
                pprint_string.push_str("( set ");
                pprint_string.push_str(&symbols.name(*name));
                object.node.pprint_into(pprint_string, symbols);
                value.node.pprint_into(pprint_string, symbols);
                pprint_string.push_str(") )");
            }

            Expression::Ternary {
                ref condition,
                ref then_branch,
                ref else_branch,
            } => {
                pprint_string.push_str("(");
                condition.node.pprint_into(pprint_string, symbols);
                pprint_string.push_str("?");
                then_branch.node.pprint_into(pprint_string, symbols);
                pprint_string.push_str(":");
                else_branch.node.pprint_into(pprint_string, symbols);
                pprint_string.push_str(" )");
            }

            Expression::Unary {
                ref expr,
                ref operator,
            } => {
                pprint_string.push_str(operator.pprint());
                expr.node.pprint_into(pprint_string, symbols);
            }

            Expression::This(_) => {
                pprint_string.push_str("this");
            }

            Expression::Var(ref v, ..) => {
                pprint_string.push_str(" ");
                pprint_string.push_str(&symbols.name(*v));
            }
        }
    }
}

#[cfg(test)]
mod test {
    use lexer::Lexer;
    use symbol::{SymbolFactory, Symbols};
    use parser::Parser;

    #[test]
    fn it_works() {
        let input = "var a =0;";
        use std::rc::Rc;
        let tokens = Lexer::new(input).lex().unwrap();
        let strings = Rc::new(SymbolFactory::new());
        let mut symbols = Symbols::new(strings);
        let ast = Parser::new(tokens, &mut symbols).parse().unwrap();

        for statement in ast {
            statement.node.pprint(&mut symbols);
        }
    }
}
