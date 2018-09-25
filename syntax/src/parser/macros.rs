/// Macro that is used to generate the code that parse a binary op
macro_rules! binary {
    ($_self:ident, $e:ident, $lhs:expr, $func:ident) => {
        while $_self.recognise($e) {
            let op = $_self.get_binary_op()?;

            let rhs = Box::new($_self.$func()?);

            $lhs = Spanned {
                span: $lhs.get_span().to(rhs.get_span()),
                value: Expression::Binary {
                    lhs: Box::new($lhs),
                    op,
                    rhs,
                },
            }
        }
    };

    ($_self:ident, $expr:expr, $lhs:expr, $func:ident) => {
        while $_self.matched($expr) {
            let op = $_self.get_binary_op()?;

            let rhs = Box::new($_self.$func()?);

            $lhs = Spanned {
                span: $lhs.get_span().to(rhs.get_span()),
                value: Expression::Binary {
                    lhs: Box::new($lhs),
                    op,
                    rhs,
                },
            }
        }
    };
}
/// Macro that expands to a match that takes a `TokenType` and turns it into a Operator
macro_rules! get_op {
    ($_self:ident,{ $($p:ident => $t:ident),*}) => {
        {
            match $_self.advance() {
                 $(Some(Spanned{
                    value: Token {
                        token:TokenType::$p,
                    },
                    ref span,
                }) => {
                    Ok(Spanned {
                    span: *span,
                    value: Op::$t,
                    })
                },)+

                Some(Spanned {
                value: Token { ref token },
                ref span,
            }) => {
                let msg = format!(
                    "Expected one of '!' '+' '-' '/''*' '=' '<' '>' '<=' '=>' but instead found {}",
                    token
                );

                $_self.error(msg, *span);

                Err(())
            }


            None => {
                $_self.reporter.global_error("Unexpected EOF");
                    Err(())
                }
            }
        }

    }
}

// The unary version of the get_binary_op macro
macro_rules! get_unary_op {
    ($_self:ident,{ $($p:ident => $t:ident),*}) => {
        {
            match $_self.advance() {
                 $(Some(Spanned{
                    value: Token {
                        token:TokenType::$p,
                    },
                    ref span,
                }) => {
                    Ok(Spanned {
                    span: *span,
                    value: UnaryOp::$t,
                    })
                },)+

                Some(Spanned {
                value: Token { ref token },
                ref span,
            }) => {
                let msg = format!(
                    "Expected one  '!' or '-' but instead found {}",
                    token
                );

                $_self.error(msg, *span);

                Err(())
            }


            None => {
                $_self.reporter.global_error("Unexpected EOF");
                    Err(())
                }
            }
        }

    }
}

macro_rules! get_assign_op {
    ($_self:ident,{ $($p:ident => $t:ident),*}) => {
        {
            match $_self.advance() {
                 $(Some(Spanned{
                    value: Token {
                        token:TokenType::$p,
                    },
                    ref span,
                }) => {
                    Ok(Spanned {
                    span: *span,
                    value: AssignOperator::$t,
                    })
                },)+

                Some(Spanned {
                value: Token { ref token },
                ref span,
            }) => {
                let msg = format!(
                    "Expected one  '+=','-=','*=','/='  but instead found {}",
                    token
                );

                $_self.error(msg, *span);

                Err(())
            }


            None => {
                $_self.reporter.global_error("Unexpected EOF");
                    Err(())
                }
            }
        }

    }
}
