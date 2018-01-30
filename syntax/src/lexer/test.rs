#[cfg(test)]
mod tests {
    use util::pos::Postition;
    use token::{Token, TokenType};
    use lexer::Lexer;

    fn add_token(line: i64, column: i64, absolute: usize, token: TokenType) -> Token {
        Token {
            pos: Postition {
                line,
                column,
                absolute,
            },
            token,
        }
    }

    #[test]
    fn string_parsing() {
        let input = "\"This is a string\"";

        let mut lexer = Lexer::new(input);

        let lexer_tokens = lexer.lex().unwrap();

        let expected_tokens = vec![
            add_token(1, 1, 0, TokenType::STRING(String::from("This is a string"))),
        ];

        assert_eq!(lexer_tokens, expected_tokens);
    }

    #[test]
    fn array_parsing() {
        let input = "[1,2,3]";

        let mut lexer = Lexer::new(input);

        let lexer_tokens = lexer.lex().unwrap();

        let expected_tokens = vec![
            add_token(1, 1, 0, TokenType::LBRACKET),
            add_token(1, 2, 1, TokenType::INT(1i64)),
            add_token(1, 3, 2, TokenType::COMMA),
            add_token(1, 4, 3, TokenType::INT(2i64)),
            add_token(1, 5, 4, TokenType::COMMA),
            add_token(1, 6, 5, TokenType::INT(3i64)),
            add_token(1, 7, 6, TokenType::RBRACKET),
        ];
        assert_eq!(lexer_tokens, expected_tokens);
    }

    #[test]
    fn empty_array_parsing() {
        let input = "[]";

        let mut lexer = Lexer::new(input);

        let lexer_tokens = lexer.lex().unwrap();

        let expected_tokens = vec![
            add_token(1, 1, 0, TokenType::LBRACKET),
            add_token(1, 2, 1, TokenType::RBRACKET),
        ];

        assert_eq!(lexer_tokens, expected_tokens);
    }

    #[test]
    fn unterminated_string_parsing() {
        let input = "\"";

        let mut lexer = Lexer::new(input);

        let lexer_tokens = lexer.lex();

        assert!(lexer_tokens.is_err())
    }

    #[test]
    fn alpha_numeric_id() {
        let input = "a9";

        let mut lexer = Lexer::new(input);

        let lexer_tokens = lexer.lex().unwrap();

        assert_eq!(
            lexer_tokens[0],
            add_token(1, 1, 0, TokenType::IDENTIFIER("a9"))
        );
    }

    #[test]
    fn test_integer_parsing() {
        let input = "10,32";

        let mut lexer = Lexer::new(input);

        let lexer_tokens = lexer.lex().unwrap();

        let expected_tokens = vec![
            add_token(1, 1, 0, TokenType::INT(10)),
            add_token(1, 3, 2, TokenType::COMMA),
            add_token(1, 4, 3, TokenType::INT(32)),
        ];

        assert_eq!(lexer_tokens, expected_tokens);
    }

    #[test]
    fn test_float_parsing() {
        let input = "10.1,32.2";

        let mut lexer = Lexer::new(input);

        let lexer_tokens = lexer.lex().unwrap();

        let expected_tokens = vec![
            add_token(1, 1, 0, TokenType::FLOAT(10.1)),
            add_token(1, 5, 4, TokenType::COMMA),
            add_token(1, 6, 5, TokenType::FLOAT(32.2)),
        ];

        assert_eq!(lexer_tokens, expected_tokens);
    }

    #[test]
    fn test_comments_are_discarded() {
        let input = "10.0 // this is a comment";

        let mut lexer = Lexer::new(input);

        let lexer_tokens = lexer.lex().unwrap();

        assert_eq!(lexer_tokens.len(), 1);
    }

    #[test]
    fn loop_keywords() {
        let input = "if,else,for,while,return,break,continue,do";

        let mut lexer = Lexer::new(input);

        let lexer_tokens = lexer.lex().unwrap();

        let expected_tokens = vec![
            add_token(1, 1, 0, TokenType::IF),
            add_token(1, 3, 2, TokenType::COMMA),
            add_token(1, 4, 3, TokenType::ELSE),
            add_token(1, 8, 7, TokenType::COMMA),
            add_token(1, 9, 8, TokenType::FOR),
            add_token(1, 12, 11, TokenType::COMMA),
            add_token(1, 13, 12, TokenType::WHILE),
            add_token(1, 18, 17, TokenType::COMMA),
            add_token(1, 19, 18, TokenType::RETURN),
            add_token(1, 25, 24, TokenType::COMMA),
            add_token(1, 26, 25, TokenType::BREAK),
            add_token(1, 31, 30, TokenType::COMMA),
            add_token(1, 32, 31, TokenType::CONTINUE),
            add_token(1, 40, 39, TokenType::COMMA),
            add_token(1, 41, 40, TokenType::DO),
        ];

        assert_eq!(lexer_tokens, expected_tokens);
    }

    #[test]
    fn value_keywords() {
        let input = "true,false,nil";

        let mut lexer = Lexer::new(input);

        let lexer_tokens = lexer.lex().unwrap();

        let expected_tokens = vec![
            add_token(1, 1, 0, TokenType::TRUE(true)),
            add_token(1, 5, 4, TokenType::COMMA),
            add_token(1, 6, 5, TokenType::FALSE(false)),
            add_token(1, 11, 10, TokenType::COMMA),
            add_token(1, 12, 11, TokenType::NIL),
        ];

        assert_eq!(lexer_tokens, expected_tokens);
    }

    #[test]
    fn statement_keywords() {
        let input = "class,this,fun,var,or,and";

        let mut lexer = Lexer::new(input);

        let lexer_tokens = lexer.lex().unwrap();

        let expected_tokens = vec![
            add_token(1, 1, 0, TokenType::CLASS),
            add_token(1, 6, 5, TokenType::COMMA),
            add_token(1, 7, 6, TokenType::THIS),
            add_token(1, 11, 10, TokenType::COMMA),
            add_token(1, 12, 11, TokenType::FUNCTION),
            add_token(1, 15, 14, TokenType::COMMA),
            add_token(1, 16, 15, TokenType::VAR),
            add_token(1, 19, 18, TokenType::COMMA),
            add_token(1, 20, 19, TokenType::OR),
            add_token(1, 22, 21, TokenType::COMMA),
            add_token(1, 23, 22, TokenType::AND),
        ];
        assert_eq!(lexer_tokens, expected_tokens);
    }

    #[test]
    fn symbols() {
        let input = "%^!/-*<> == != <= >= ->";

        let mut lexer = Lexer::new(input);

        let lexer_tokens = lexer.lex().unwrap();

        let expected_tokens = vec![
            add_token(1, 1, 0, TokenType::MODULO),
            add_token(1, 2, 1, TokenType::EXPONENTIAL),
            add_token(1, 3, 2, TokenType::BANG),
            add_token(1, 4, 3, TokenType::SLASH),
            add_token(1, 5, 4, TokenType::MINUS),
            add_token(1, 6, 5, TokenType::STAR),
            add_token(1, 7, 6, TokenType::LESSTHAN),
            add_token(1, 8, 7, TokenType::GREATERTHAN),
            add_token(1, 10, 9, TokenType::EQUALEQUAL),
            add_token(1, 13, 12, TokenType::BANGEQUAL),
            add_token(1, 16, 15, TokenType::LESSTHANEQUAL),
            add_token(1, 19, 18, TokenType::GREATERTHANEQUAL),
            add_token(1, 22, 21, TokenType::FRETURN),
        ];
        assert_eq!(lexer_tokens, expected_tokens);
    }

    #[test]
    fn block() {
        let input = "{
            5 < 10 > 5;
        }";

        let mut lexer = Lexer::new(input);

        let lexer_tokens = lexer.lex().unwrap();

        let expected_tokens = vec![
            add_token(1, 1, 0, TokenType::LBRACE),
            add_token(2, 13, 14, TokenType::INT(5)),
            add_token(2, 15, 16, TokenType::LESSTHAN),
            add_token(2, 17, 18, TokenType::INT(10)),
            add_token(2, 20, 21, TokenType::GREATERTHAN),
            add_token(2, 22, 23, TokenType::INT(5)),
            add_token(2, 23, 24, TokenType::SEMICOLON),
            add_token(3, 9, 34, TokenType::RBRACE),
        ];
        assert_eq!(lexer_tokens, expected_tokens);
    }

}
