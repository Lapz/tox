#[macro_export]

macro_rules! T {
    (break) => {
        $crate::token::SyntaxKind::BREAK
    };
    (continue) => {
        $crate::token::SyntaxKind::CONTINUE
    };
    (do) => {
        $crate::token::SyntaxKind::DO
    };
    (return) => {
        $crate::token::SyntaxKind::RETURN
    };
    (print) => {
        $crate::token::SyntaxKind::PRINT
    };
    (class) => {
        $crate::token::SyntaxKind::CLASS
    };
    (true) => {
        $crate::token::SyntaxKind::TRUE
    };
    (false) => {
        $crate::token::SyntaxKind::FALSE
    };
    (extends) => {
        $crate::token::SyntaxKind::EXTENDS
    };
    (extern) => {
        $crate::token::SyntaxKind::EXPORT
    };
    (match) => {
        $crate::token::SyntaxKind::MATCH
    };
    (else) => {
        $crate::token::SyntaxKind::ELSE
    };
    (if) => {
        $crate::token::SyntaxKind::IF
    };
    (let) => {
        $crate::token::SyntaxKind::LET
    };

    ("{") => {
        $crate::token::SyntaxKind::L_CURLY_
    };
    ("}") => {
        $crate::token::SyntaxKind::R_CURLY
    };
    ("[") => {
        $crate::token::SyntaxKind::L_SQUARE
    };
    ("]") => {
        $crate::token::SyntaxKind::R_SQUARE
    };
    ("(") => {
        $crate::token::SyntaxKind::L_PAREN
    };
    (")") => {
        $crate::token::SyntaxKind::R_PAREN
    };

    (=) => {
        $crate::token::SyntaxKind::ASSIGN
    };
    (:) => {
        $crate::token::SyntaxKind::COLON
    };
    (.) => {
        $crate::token::SyntaxKind::DOT
    };
    (;) => {
        $crate::token::SyntaxKind::SEMICOLON
    };
    (+=) => {
        $crate::token::SyntaxKind::PLUS_ASSIGN
    };
    (!) => {
        $crate::token::SyntaxKind::BANG
    };
    ("//") => {
        $crate::token::SyntaxKind::COMMENT
    };

    (+) => {
        $crate::token::SyntaxKind::PLUS
    };
    (-) => {
        $crate::token::SyntaxKind::MINUS
    };
    (*) => {
        $crate::token::SyntaxKind::STAR
    };
    (/) => {
        $crate::token::SyntaxKind::SLASH
    };

    (&&) => {
        $crate::token::SyntaxKind::AND
    };
    (==) => {
        $crate::token::SyntaxKind::EQUAL_EQUAL
    };
    (=>) => {
        $crate::token::SyntaxKind::FAT_ARROW
    };
    (<) => {
        $crate::token::SyntaxKind::LESS
    };
    (<=) => {
        $crate::token::SyntaxKind::LESS_EQUAL
    };
    (>) => {
        $crate::token::SyntaxKind::GREATER
    };
    (>=) => {
        $crate::token::SyntaxKind::GREATER_EQUAL
    };
    (!=) => {
        $crate::token::SyntaxKind::BANG_EQUAL
    };
    (||) => {
        $crate::token::SyntaxKind::OR
    };
}
