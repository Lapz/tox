/*
    match f {
        List::Cons(1,x) => x,
        List::Nil => 0,
        List::Cons(1,List::Cons(y,x)) => x+y
    };
*/
Match {
        cond: Var(Symbol(10)),
        patterns: [
            MatchArm {
                lhs: Expr(Constructor {
                    enum_name: Symbol(5),
                    variant: Symbol(6),
                    args: [Literal(Int(1)), Var(Symbol(11))],
                }),
                rhs: Expr(Var(Symbol(11))),
                is_all: false,
            },
            MatchArm {
                lhs: Expr(Constructor {
                    enum_name: Symbol(5),
                    variant: Symbol(8),
                    args: [],
                }),
                rhs: Expr(Literal(Int(0))),
                is_all: false,
            },
            MatchArm {
                lhs: Expr(Constructor {
                    enum_name: Symbol(5),
                    variant: Symbol(6),
                    args: [
                        Literal(Int(1)),
                        Constructor {
                            enum_name: Symbol(5),
                            variant: Symbol(6),
                            args: [Var(Symbol(12)), Var(Symbol(11))],
                        },
                    ],
                }),
                rhs: Expr(Var(Symbol(11))),
                is_all: false,
            },
        ],
    }