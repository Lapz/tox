#[macro_export]
macro_rules! T {
    (break) => {
        $crate::SyntaxKind::BREAK
    };
    (continue) => {
        $crate::SyntaxKind::CONTINUE
    };
    (do) => {
        $crate::SyntaxKind::DO
    };
    (return) => {
        $crate::SyntaxKind::RETURN
    };
    (print) => {
        $crate::SyntaxKind::PRINT
    };
    (class) => {
        $crate::SyntaxKind::CLASS
    };
    (true) => {
        $crate::SyntaxKind::TRUE
    };
    (false) => {
        $crate::SyntaxKind::FALSE
    };
    (extends) => {
        $crate::SyntaxKind::EXTENDS
    };
    (extern) => {
        $crate::SyntaxKind::EXTERN_KW
    };
    (match) => {
        $crate::SyntaxKind::MATCH
    };
    (else) => {
        $crate::SyntaxKind::ELSE
    };
    (if) => {
        $crate::SyntaxKind::IF
    };
    (let) => {
        $crate::SyntaxKind::LET
    };

    ("{") => {
        $crate::SyntaxKind::L_CURLY
    };

    (,) => {
        $crate::SyntaxKind::COMMA
    };
    ("}") => {
        $crate::SyntaxKind::R_CURLY
    };
    ("[") => {
        $crate::SyntaxKind::L_SQUARE
    };
    ("]") => {
        $crate::SyntaxKind::R_SQUARE
    };
    ("(") => {
        $crate::SyntaxKind::L_PAREN
    };
    (")") => {
        $crate::SyntaxKind::R_PAREN
    };

    (=) => {
        $crate::SyntaxKind::ASSIGN
    };
    (:) => {
        $crate::SyntaxKind::COLON
    };
    (.) => {
        $crate::SyntaxKind::DOT
    };
    (;) => {
        $crate::SyntaxKind::SEMICOLON
    };
    (+=) => {
        $crate::SyntaxKind::PLUS_ASSIGN
    };
    (!) => {
        $crate::SyntaxKind::BANG
    };
    ("//") => {
        $crate::SyntaxKind::COMMENT
    };

    (+) => {
        $crate::SyntaxKind::PLUS
    };
    (-) => {
        $crate::SyntaxKind::MINUS
    };
    (*) => {
        $crate::SyntaxKind::STAR
    };
    (/) => {
        $crate::SyntaxKind::SLASH
    };

    (&&) => {
        $crate::SyntaxKind::AND
    };
    (==) => {
        $crate::SyntaxKind::EQUAL_EQUAL
    };
    (=>) => {
        $crate::SyntaxKind::FAT_ARROW
    };
    (<) => {
        $crate::SyntaxKind::LESS
    };
    (<=) => {
        $crate::SyntaxKind::LESS_EQUAL
    };
    (>) => {
        $crate::SyntaxKind::R_ANGLE
    };
    (>=) => {
        $crate::SyntaxKind::GREATER_EQUAL
    };
    (!=) => {
        $crate::SyntaxKind::BANG_EQUAL
    };
    (||) => {
        $crate::SyntaxKind::OR
    };
}
