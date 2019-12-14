use crate::ast;
use crate::children;

impl ast::BinExpr {
    pub(crate) fn lhs(&self) -> Option<ast::BinExpr> {
        children(self).nth(0)
    }

    pub(crate) fn rhs(&self) -> Option<ast::BinExpr> {
        children(self).nth(1)
    }

    pub(crate) fn op_kind(&self) -> Option<BinOp> {
        self.syntax()
            .children_with_tokens()
            .filter_map(|it| it.into_token())
            .find_map(|c| {
                let op = match c.kind() {
                    T![-] => BinOp::Minus,
                    T![+] => BinOp::Plus,
                    T![*] => BinOp::Mult,
                    T![/] => BinOp::Div,
                    T![&&] => BinOp::And,
                    T![||] => BinOp::Or,
                    T![<] => BinOp::LessThan,
                    T![>] => BinOp::GreaterThan,
                    T![==] => BinOp::EqualEqual,
                    T![!] => BinOp::Excl,
                    T![!=] => BinOp::NotEqual,
                    T![<=] => BinOp::LessThanEqual,
                    T![>=] => BinOp::GreaterThanEqual,
                    T![+=] => BinOp::PlusEqual,
                    T![-=] => BinOp::MinusEqual,
                    T![*=] => BinOp::MultEqual,
                    T![/=] => BinOp::DivEqual,
                    _ => return None,
                };

                Some(op)
            })
    }
}

pub enum BinOp {
    Plus,
    Minus,
    Mult,
    Div,
    And,
    Or,
    LessThan,
    GreaterThan,
    EqualEqual,
    Excl,
    NotEqual,
    LessThanEqual,
    GreaterThanEqual,
    PlusEqual,
    MinusEqual,
    MultEqual,
    DivEqual,
}
