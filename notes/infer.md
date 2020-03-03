# Type Inference

During the type inference variable resolution is done as well as doing
type inference.

The type inference is based on the HIR.

```rust
    #[derive(Debug, Clone, Eq, PartialEq)]
    pub enum Stmt {
        Let {
            pat: PatId,
            initializer: Option<ExprId>,
        },
        Expr(ExprId),
    }
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum Expr {
        Array(Vec<ExprId>),
        Binary { lhs: ExprId, op: BinOp, rhs: ExprId },
        Block(Vec<StmtId>),
        Break,
        Call { callee: ExprId, args: Vec<ExprId> },
        Cast { expr: ExprId, ty: TypeId },
        Continue,
        If { cond: ExprId },
        Ident(NameId),
        Index { base: ExprId, index: ExprId },
        While { cond: ExprId, body: Vec<StmtId> },
        Literal(LiteralId),
        Paren(ExprId),
        Tuple(Vec<ExprId>),
        Unary { op: UnaryOp, expr: ExprId },
        Return(Option<ExprId>),
        Match { expr: ExprId, arms: Vec<MatchArm> },
    }


```

It should be done using query's.
