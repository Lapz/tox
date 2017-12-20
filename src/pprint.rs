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
    fn pprint(&self) -> &str {
        match *self {
            Literal::Int(ref i) => &i.to_string(),
            Literal::False(ref f) => &f.to_string(),
            Literal::Str(ref s) => s.as_str(),
            Literal::False(_) => "false",
            Literal::True(_) => "true",
            Literal::Nil => "nil",
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
 fn pprint(&self, symbols: Symbols<()>) -> String {
        let mut pprinted = String::new();

        self.pprint_into(&mut pprinted, symbols);

        pprinted
    }
}

impl Expression {
    fn pprint(&self, symbols: Symbols<()>) -> String {
        let mut pprinted = String::new();

        self.pprint_into(&mut pprinted, symbols);

        pprinted
    }

    fn pprint_into(&self, pprint_string: &mut String, symbols: Symbols<()>) {
        match *self {
            Expression::Array { ref items } => {
                pprint_string.push_str("(array");
                for ref item in items {
                    item.node.pprint_into(pprint_string, symbols);
                }
                pprint_string.push_str(")");
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
                pprint_string.push_str(")");
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
                pprint_string.push_str(")");
            }

            Expression::Call {
                ref callee,
                ref arguments,
            } => {
                pprint_string.push_str("(fn");
                callee.node.pprint_into(pprint_string, symbols);
                pprint_string.push_str(")");
            }

            Expression::Dict { ref items } => {
                pprint_string.push_str("(dict ");

                for &(ref key, ref value) in items {
                    key.node.pprint_into(pprint_string, symbols);

                    pprint_string.push_str(":");

                    value.node.pprint_into(pprint_string, symbols);
                }

                pprint_string.push_str(")");
            }

            Expression::Func {
                ref parameters,
                ref body,
                ..
            } => {
                pprint_string.push_str("( fn (");

                for item in parameters {
                    pprint_string.push_str(&symbols.name(item.0));
                    pprint_string.push_str(":");
                    pprint_string.push_str(&symbols.name(item.1));
                }

                pprint_string.push_str(") )");
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
                pprint_string.push_str(literal.pprint());
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
                pprint_string.push_str(")");
            }

            Expression::Unary { ref expr, ref operator} => {
                pprint_string.push_str(operator.pprint());
                expr.node.pprint_into(pprint_string, symbols);
            }

            Expression::This(ref handle) => {
                pprint_string.push_str("this");
            }

            Expression::Var(ref v, ref handle) => {
                pprint_string.push_str(&symbols.name(*v));
            }

            _ => unimplemented!(),
        }
    }
}
