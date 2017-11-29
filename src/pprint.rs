use ast::expr::*;
// Credit to mariosangiorgio for the pprint implementation
pub trait PrettyPrint {
    fn pprint_into(&self, pprint_printed: &mut String) -> ();
    fn pprint(&self) -> String {
        let mut pprinted = String::new();

        self.pprint_into(&mut pprinted);

        pprinted
    }
}

impl <'a> PrettyPrint for Logical<'a> {
    fn pprint_into(&self, pprint_printed: &mut String) -> () {
        pprint_printed.push_str("(");

        self.operator.pprint_into(pprint_printed);


        self.left.pprint_into(pprint_printed);

        self.right.pprint_into(pprint_printed);
        pprint_printed.push_str(")");
    }
}

impl <'a> PrettyPrint for Unary<'a> {
    fn pprint_into(&self, pprint_printed: &mut String) -> () {
        pprint_printed.push_str("(");
        self.operator.pprint_into(pprint_printed);

        self.expr.pprint_into(pprint_printed);
        pprint_printed.push_str(")");
    }
}

impl <'a> PrettyPrint for Binary<'a> {
    fn pprint_into(&self, pprint_printed: &mut String) -> () {
        pprint_printed.push_str("(");
        self.operator.pprint_into(pprint_printed);

        self.left_expr.pprint_into(pprint_printed);

        self.right_expr.pprint_into(pprint_printed);
        pprint_printed.push_str(")");
    }
}

impl <'a> PrettyPrint for Ternary<'a> {
    fn pprint_into(&self, pprint_printed: &mut String) -> () {
        pprint_printed.push_str("(");
        self.condition.pprint_into(pprint_printed);

        self.then_branch.pprint_into(pprint_printed);

        self.else_branch.pprint_into(pprint_printed);
        pprint_printed.push_str(")");
    }
}

impl  PrettyPrint for LogicOperator {
    fn pprint_into(&self, pprint_printed: &mut String) -> () {
        match *self {
            LogicOperator::And => pprint_printed.push_str("and "),
            LogicOperator::Or => pprint_printed.push_str("or "),
        }
    }
}
impl  PrettyPrint for Operator {
    fn pprint_into(&self, pprint_printed: &mut String) -> () {
        match *self {
            Operator::Plus => pprint_printed.push_str("+ "),
            Operator::Slash => pprint_printed.push_str("/ "),
            Operator::Star => pprint_printed.push_str("*"),
            Operator::EqualEqual => pprint_printed.push_str("== "),
            Operator::BangEqual => pprint_printed.push_str("!= "),
            Operator::GreaterThanEqual => pprint_printed.push_str(">= "),
            Operator::LessThanEqual => pprint_printed.push_str("<= "),
            Operator::GreaterThan => pprint_printed.push_str("> "),
            Operator::LessThan => pprint_printed.push_str("< "),
            Operator::Minus => pprint_printed.push_str("- "),
            Operator::Modulo => pprint_printed.push_str("% "),
            Operator::Exponential => pprint_printed.push_str("^ "),
        }
    }
}


impl <'a> PrettyPrint for Call<'a> {
    fn pprint_into(&self, pprint_printed: &mut String) -> () {
        pprint_printed.push_str("( fn");
        self.callee.pprint_into(pprint_printed);
        pprint_printed.push_str(")");
    }
}
impl  PrettyPrint for UnaryOperator {
    fn pprint_into(&self, pprint_printed: &mut String) -> () {
        match *self {
            UnaryOperator::Bang => pprint_printed.push_str("! "),
            UnaryOperator::Minus => pprint_printed.push_str("- "),
        }
    }
}

impl <'a> PrettyPrint for Grouping<'a> {
    fn pprint_into(&self, pprint_printed: &mut String) -> () {
        pprint_printed.push_str("( group");
        self.expr.pprint_into(pprint_printed);
        pprint_printed.push_str(")");
    }
}

impl <'a> PrettyPrint for Func<'a> {
    fn pprint_into(&self, pprint_printed: &mut String) -> () {
        pprint_printed.push_str("( fn (");


        for item in &self.parameters {
            item.pprint_into(pprint_printed);
        }

        pprint_printed.push_str(") )");
    }
}


impl <'a> PrettyPrint for Array<'a> {
    fn pprint_into(&self, pprint_printed: &mut String) -> () {
        pprint_printed.push_str("(array");

        for item in &self.items {
            item.pprint_into(pprint_printed);
        }

        pprint_printed.push_str(")");
    }
}

impl <'a> PrettyPrint for Dictionary<'a> {
    fn pprint_into(&self, pprint_printed: &mut String) -> () {
        pprint_printed.push_str("(dict ");

        for &(ref key, ref value) in &self.items {
            key.pprint_into(pprint_printed);

            pprint_printed.push_str(":");

            value.pprint_into(pprint_printed);
        }

        pprint_printed.push_str(")");
    }
}

impl <'a> PrettyPrint for Variable<'a> {
    fn pprint_into(&self, pprint_printed: &mut String) -> () {
        pprint_printed.push_str("(");
        pprint_printed.push_str(&self.0);
        pprint_printed.push_str(")");
    }
}

impl <'a> PrettyPrint for Assign<'a> {
    fn pprint_into(&self, pprint_printed: &mut String) -> () {
        pprint_printed.push_str("(");
        pprint_printed.push_str(&self.name.0);
        pprint_printed.push_str("=");
        self.value.pprint_into(pprint_printed);
        pprint_printed.push_str(")");
    }
}

impl <'a> PrettyPrint for IndexExpr<'a> {
    fn pprint_into(&self, pprint_printed: &mut String) -> () {
        pprint_printed.push_str("(");
        self.target.pprint_into(pprint_printed);
        pprint_printed.push_str("[");
        self.index.pprint_into(pprint_printed);
        pprint_printed.push_str("]");
        pprint_printed.push_str(")");
    }
}

impl PrettyPrint for Literal {
    fn pprint_into(&self, pprint_printed: &mut String) -> () {
        match *self {
            Literal::Float(i) => {
                pprint_printed.push_str(" ");
                pprint_printed.push_str(&i.to_string());
                pprint_printed.push_str(" ");
            },

            Literal::Int(i) => {
                pprint_printed.push_str(" ");
                pprint_printed.push_str(&i.to_string());
                pprint_printed.push_str(" ");
            
            },
            Literal::Str(ref s) => {
                pprint_printed.push_str(" ");
                pprint_printed.push_str(s.as_str())
            },
            Literal::False(_) => {
                pprint_printed.push_str(" false");
            },
            Literal::True(_) => {
                pprint_printed.push_str(" true");
            },
            Literal::Nil => {
                pprint_printed.push_str(" nil");
            },
        }
    }
}

impl <'a> PrettyPrint for Expression<'a> {
    fn pprint_into(&self, pprinted: &mut String) -> () {
        match *self {
            Expression::Grouping(ref g) => g.pprint_into(pprinted),
            Expression::Dict(ref d) => d.pprint_into(pprinted),
            Expression::Unary(ref u) => u.pprint_into(pprinted),
            Expression::Literal(ref l) => l.pprint_into(pprinted),
            Expression::Binary(ref b) => b.pprint_into(pprinted),
            Expression::Var(ref v, _) => v.pprint_into(pprinted),
            Expression::Assign(ref a) => a.pprint_into(pprinted),
            Expression::Logical(ref l) => l.pprint_into(pprinted),
            Expression::Ternary(ref t) => t.pprint_into(pprinted),
            Expression::Array(ref l) => l.pprint_into(pprinted),
            Expression::IndexExpr(ref a) => a.pprint_into(pprinted),
            Expression::Call(ref c) => c.pprint_into(pprinted),
            Expression::Func(ref f) => f.pprint_into(pprinted),
            _ => unimplemented!(),
        }
    }
}