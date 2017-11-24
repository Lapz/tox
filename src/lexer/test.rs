
#[cfg(test)]
mod tests {


    use token::{Token, TokenType,Postition};
    use lexer::Lexer;


    fn add_token(line: i64, column: i64, absolute:usize,token: TokenType) -> Token {
        Token {
            pos:Postition{
                line,
                column,
                absolute
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
            add_token(1, 1,0, TokenType::STRING(String::from("This is a string"))),
        ];

        assert_eq!(lexer_tokens, expected_tokens);
    }

    #[test]
    fn array_parsing() {
        let input = "[1,2,3]";

        let mut lexer = Lexer::new(input);

        let lexer_tokens = lexer.lex().unwrap();

        let expected_tokens = vec![
            add_token(1, 1,0,  TokenType::LBRACKET),
            add_token(1, 2,1, TokenType::INT(1i64)),
            add_token(1, 3,2, TokenType::COMMA),
            add_token(1, 4,3, TokenType::INT(2i64)),
            add_token(1, 5,4, TokenType::COMMA),
            add_token(1, 6,5, TokenType::INT(3i64)),
            add_token(1, 7,6, TokenType::RBRACKET),
        ];
        assert_eq!(lexer_tokens, expected_tokens);
    }

    #[test]
    fn empty_array_parsing() {
        let input = "[]";

        let mut lexer = Lexer::new(input);

        let lexer_tokens = lexer.lex().unwrap();

        let expected_tokens = vec![
            add_token(1, 1,0, TokenType::LBRACKET),
            add_token(1, 2,1,TokenType::RBRACKET),
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

        assert_eq!(lexer_tokens[0], add_token(1, 1,0,TokenType::IDENTIFIER("a9")));
    }


    #[test]
    fn test_integer_parsing() {
        let input = "10,32";

        let mut lexer = Lexer::new(input);


        let lexer_tokens = lexer.lex().unwrap();

        let expected_tokens = vec![
            add_token(1, 1,0,TokenType::INT(10)),
            add_token(1, 3,2, TokenType::COMMA),
            add_token(1, 4,3, TokenType::INT(32)),
           
        ];

        assert_eq!(lexer_tokens, expected_tokens);
    }

    #[test]
    fn test_float_parsing() {
        let input = "10.1,32.2";

        let mut lexer = Lexer::new(input);


        let lexer_tokens = lexer.lex().unwrap();

        let expected_tokens = vec![
            add_token(1, 1,0,TokenType::FLOAT(10.1)),
            add_token(1, 5,4, TokenType::COMMA),
            add_token(1, 6,5, TokenType::FLOAT(32.2)),
           
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

    fn loop_keywords( ){
        let input = "if,else,for,while,return,break,continue,do";

         let mut lexer = Lexer::new(input);


        let lexer_tokens = lexer.lex().unwrap();

        let expected_tokens = vec![
            add_token(1, 1,0, TokenType::IF),
            add_token(1, 3,2, TokenType::COMMA),
            add_token(1, 4,3, TokenType::ELSE),
            add_token(1, 8,7, TokenType::COMMA),
            add_token(1, 9,8, TokenType::FOR),
            add_token(1, 12,11, TokenType::COMMA),
            add_token(1, 13,12, TokenType::WHILE),
            add_token(1, 18,17, TokenType::COMMA),
            add_token(1, 19,18, TokenType::RETURN),
            add_token(1, 25,24,TokenType::COMMA),
            add_token(1, 2 6,25, TokenType::BREAK),
            add_token(1, 30,31, TokenType::COMMA),
            add_token(1, 28,27, TokenType::CONTINUE),
            add_token(1, 29,30, TokenType::COMMA),
            add_token(1, 1,39, TokenType::DO),

        ];

        assert_eq!(lexer_tokens,expected_tokens);


    }

    // #[test]
    // fn keywords() {
    //     let input = "pprint,class,super,this,fun,var,true,false,or,and,nil";

    //     let mut lexer = Lexer::new(input);


    //     let lexer_tokens = lexer.lex().unwrap();

    //     let expected_tokens = vec![
    //         add_token(1, 6,0, TokenType::PPRINT),
    //         add_token(1, 7,7 ,TokenType::COMMA),
    //         add_token(1, 13,0, TokenType::CLASS),
    //         add_token(1, 14,0, TokenType::COMMA),
    //         add_token(1, 19,0, TokenType::SUPER),
    //         add_token(1, 20,0, TokenType::COMMA),
    //         add_token(1, 24,0, TokenType::THIS),
    //         add_token(1, 25,0, TokenType::COMMA),
    //         add_token(1, 28,0, TokenType::FUNCTION),
    //         add_token(1, 29,0, TokenType::COMMA),
    //         add_token(1, 32,0, TokenType::VAR),
    //         add_token(1, 33,0, TokenType::COMMA),
    //         add_token(1, 76, TokenType::COMMA),
    //         add_token(1, 80, TokenType::TRUE(true)),
    //         add_token(1, 81, TokenType::COMMA),
    //         add_token(1, 86, TokenType::FALSE(false)),
    //         add_token(1, 87, TokenType::COMMA),
    //         add_token(1, 89, TokenType::OR),
    //         add_token(1, 90, TokenType::COMMA),
    //         add_token(1, 93, TokenType::AND),
    //         add_token(1, 94, TokenType::COMMA),
    //         add_token(1, 97, TokenType::NIL),
    //     ];
    //     assert_eq!(lexer_tokens, expected_tokens);
    // }

    // #[test]
    // fn var_declaration_array() {
    //     let input = "var five = [1,2,3];";

    //     let mut lexer = Lexer::new(input);

    //     let lexer_tokens = lexer.lex().unwrap();

    //     let expected_tokens = vec![
    //         add_token(1, 4, TokenType::VAR),
    //         add_token(1, 9, TokenType::IDENTIFIER("five".to_owned())),
    //         add_token(1, 11, TokenType::ASSIGN),
    //         add_token(1, 13, TokenType::LBRACKET),
    //         add_token(1, 14, TokenType::INT(1f64)),
    //         add_token(1, 15, TokenType::COMMA),
    //         add_token(1, 16, TokenType::INT(2f64)),
    //         add_token(1, 17, TokenType::COMMA),
    //         add_token(1, 18, TokenType::INT(3f64)),
    //         add_token(1, 19, TokenType::RBRACKET),
    //         add_token(1, 20, TokenType::SEMICOLON),
    //     ];
    //     assert_eq!(lexer_tokens, expected_tokens);
    // }

    // #[test]
    // fn var_declaration_string() {
    //     let input = "var five = \"hello\";";


    //     let mut lexer = Lexer::new(input);


    //     let lexer_tokens = lexer.lex().unwrap();

    //     let expected_tokens = vec![
    //         add_token(1, 4, TokenType::VAR),
    //         add_token(1, 9, TokenType::IDENTIFIER("five".to_owned())),
    //         add_token(1, 11, TokenType::ASSIGN),
    //         add_token(1, 18, TokenType::STRING("hello".to_owned())),
    //         add_token(1, 19, TokenType::SEMICOLON),
    //     ];
    //     assert_eq!(lexer_tokens, expected_tokens);
    // }

    // #[test]
    // fn var_declaration_int() {
    //     let input = "var five = 5;";


    //     let mut lexer = Lexer::new(input);


    //     let lexer_tokens = lexer.lex().unwrap();

    //     let expected_tokens = vec![
    //         add_token(1, 4, TokenType::VAR),
    //         add_token(1, 9, TokenType::IDENTIFIER("five".to_owned())),
    //         add_token(1, 11, TokenType::ASSIGN),
    //         add_token(1, 13, TokenType::INT(5f64)),
    //         add_token(1, 14, TokenType::SEMICOLON),
    //     ];
    //     assert_eq!(lexer_tokens, expected_tokens);
    // }

    // #[test]
    // fn fun_declaration_int() {
    //     let input = "fun(x, y) {
    //         x + y;
    //     };";


    //     let mut lexer = Lexer::new(input);


    //     let lexer_tokens = lexer.lex().unwrap();

    //     let expected_tokens = vec![
    //         add_token(1, 4, TokenType::FUNCTION),
    //         add_token(1, 5, TokenType::LPAREN),
    //         add_token(1, 6, TokenType::IDENTIFIER("x".to_owned())),
    //         add_token(1, 7, TokenType::COMMA),
    //         add_token(1, 9, TokenType::IDENTIFIER("y".to_owned())),
    //         add_token(1, 10, TokenType::RPAREN),
    //         add_token(1, 12, TokenType::LBRACE),
    //         add_token(2, 14, TokenType::IDENTIFIER("x".to_owned())),
    //         add_token(2, 16, TokenType::PLUS),
    //         add_token(2, 18, TokenType::IDENTIFIER("y".to_owned())),
    //         add_token(2, 19, TokenType::SEMICOLON),
    //         add_token(3, 10, TokenType::RBRACE),
    //         add_token(3, 11, TokenType::SEMICOLON),
    //     ];
    //     assert_eq!(lexer_tokens, expected_tokens);
    // }

    // #[test]
    // fn call_expression_int() {
    //     let input = "sum([10,2]);";

    //     let mut lexer = Lexer::new(input);


    //     let lexer_tokens = lexer.lex().unwrap();

    //     let expected_tokens = vec![
    //         add_token(1, 4, TokenType::IDENTIFIER("sum".to_owned())),
    //         add_token(1, 5, TokenType::LPAREN),
    //         add_token(1, 6, TokenType::LBRACKET),
    //         add_token(1, 8, TokenType::INT(10f64)),
    //         add_token(1, 9, TokenType::COMMA),
    //         add_token(1, 10, TokenType::INT(2f64)),
    //         add_token(1, 11, TokenType::RBRACKET),
    //         add_token(1, 12, TokenType::RPAREN),
    //         add_token(1, 13, TokenType::SEMICOLON),
    //     ];
    //     assert_eq!(lexer_tokens, expected_tokens);
    // }

    // #[test]
    // fn symbols() {
    //     let input = "%^!/-*<> == != <= >=";

    //     let mut lexer = Lexer::new(input);

    //     let lexer_tokens = lexer.lex().unwrap();

    //     let expected_tokens = vec![
    //         add_token(1, 2, TokenType::MODULO),
    //         add_token(1, 3, TokenType::EXPONENTIAL),
    //         add_token(1, 4, TokenType::BANG),
    //         add_token(1, 5, TokenType::SLASH),
    //         add_token(1, 6, TokenType::MINUS),
    //         add_token(1, 7, TokenType::STAR),
    //         add_token(1, 8, TokenType::LESSTHAN),
    //         add_token(1, 9, TokenType::GREATERTHAN),
    //         add_token(1, 12, TokenType::EQUALTO),
    //         add_token(1, 15, TokenType::BANGEQUAL),
    //         add_token(1, 18, TokenType::LESSTHANEQUAL),
    //         add_token(1, 21, TokenType::GREATERTHANEQUAL),
    //     ];
    //     assert_eq!(lexer_tokens, expected_tokens);
    // }

    

    // #[test]
    // fn block() {
    //     let input = "{
    //                         5 < 10 > 5;
    //                        }";

    //     let mut lexer = Lexer::new(input);


    //     let lexer_tokens = lexer.lex().unwrap();

    //     let expected_tokens = vec![
    //         add_token(1, 2, TokenType::LBRACE),
    //         add_token(2, 30, TokenType::INT(5f64)),
    //         add_token(2, 32, TokenType::LESSTHAN),
    //         add_token(2, 35, TokenType::INT(10f64)),
    //         add_token(2, 37, TokenType::GREATERTHAN),
    //         add_token(2, 39, TokenType::INT(5f64)),
    //         add_token(2, 40, TokenType::SEMICOLON),
    //         add_token(3, 29, TokenType::RBRACE),
    //     ];
    //     assert_eq!(lexer_tokens, expected_tokens);
    // }

}
