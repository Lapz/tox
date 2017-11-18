// mod test;
//

use token::{Token, TokenType};
use std::str::Chars;
use std::iter::Peekable;

use std::fmt::{Display, Formatter};
use std::fmt;
#[derive(Debug)]
pub enum LexerError {
    UnclosedString(String),
    UnclosedBlockComment(String),
    InvalidFloat(String),
    EOF,
    EscapeCode, // Add the char
}

impl Display for LexerError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            LexerError::UnclosedString(ref e) => write!(f, "unclosed string {}", e),
            LexerError::EOF => write!(f, "Unexpected EOf"),
            LexerError::EscapeCode => write!(f, "Unexpected escape code"),
            LexerError::UnclosedBlockComment(ref e) => write!(f, "unclosed block comment {}", e),
            LexerError::InvalidFloat(ref e) => write!(f, "Inavlid Float {}", e),
        }
    }
}



#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    // A lexer instance
    line: i64,
    column: i64,
    input: &'a str,
    chars: Peekable<Chars<'a>>,
    lookahead:Option<char>
}


impl<'a> Lexer<'a> {
    /// Returns a new Lexer
    pub fn new(input: &'a str) -> Lexer {

        let mut chars = input.chars().peekable();
        Lexer {
            lookahead: chars.next(),
            chars: chars,
            input: input,
            line: 1,
            column: 1,
        }
    }


    fn peek(&mut self) -> Option<&char> {
        self.chars.peek()
    }


    fn token_with_info(&mut self, token: TokenType) -> Token {
        Token {
            token,
            line: self.line,
            column: self.column,
        }
    }



    fn advance(&mut self) -> Option<char> {
        self.column += 1;
        self.chars.next()
    }


    fn peek_ch_is_letter(&mut self) -> bool {
        // Checks if character is a letter
        match self.peek() {
            Some(&ch) => self.ch_is_letter(ch),
            None => false,
        }
    }



    fn skip_whitespace(&mut self) -> () {
        // Skips through white space

        while let Some(&c) = self.peek() {
            if c == '\n' {
                self.line += 1;
                self.column = 1;
            } else if !c.is_whitespace() {
                break;
            }
            self.advance();
        }
    }


    fn number(&mut self, first: char) -> Result<TokenType, LexerError> {
        // Parses the number and returns the f64 value

        let mut number = String::new();
        number.push(first);

        while let Some(&ch) = self.peek() {
            if !ch.is_numeric() {
                break;
            }
            number.push(self.advance().unwrap())
        }

        match self.peek() {
            Some(&'.') => {
                self.advance();
                while let Some(ch) = self.peek() {
                    if !ch.is_numeric() {
                        break;
                    }

                    number.push(self.advance().unwrap());
                }

                Ok(TokenType::FLOAT(number.parse().unwrap()))
            }

            Some(ch) => {
                if ch.is_alphabetic() {
                    return Err(LexerError::InvalidFloat(String::from("asdad")));
                }

                Ok(TokenType::INT(number.parse().unwrap()))
            }

            None => Err(LexerError::EOF),
        }
    }

    // var m = 10;
    fn identifier(&mut self, first: char) -> String {
        let mut ident = String::new();



        ident.push(first);

        while self.peek_ch_is_letter() {
            ident.push(self.advance().unwrap())
        }


        ident
    }

    fn escape_code(&mut self) -> Result<char, LexerError> {
        match self.advance() {
            Some('\'') => Ok('\''),
            Some('"') => Ok('"'),
            Some('\\') => Ok('\\'),
            Some('/') => Ok('/'),
            Some('n') => Ok('\n'),
            Some('r') => Ok('\r'),
            Some('t') => Ok('\t'),
            None => Err(LexerError::EOF),
            Some(ch) => Err(LexerError::EscapeCode),
        }
    }


    fn string(&mut self) -> Result<TokenType, LexerError> {
        let mut string = String::new();

        while let Some(ch) = self.advance() {
            match ch {
                '"' => {
                    self.line += 1;
                    return Ok(TokenType::STRING(string));
                }

                '\\' => string.push(self.escape_code()?),

                ch => string.push(ch),
            }
        }

        unimplemented!()
    }





    fn ch_is_letter(&mut self, ch: char) -> bool {
        ch.is_alphanumeric() || ch == '_'
    }

    /// Gets the next token from the Lexer
    fn next_token(&mut self) -> Result<TokenType, LexerError> {
        self.skip_whitespace();

        match self.advance() {
            Some('(') => Ok(TokenType::LPAREN),

            Some(')') => Ok(TokenType::RPAREN),

            Some('{') => Ok(TokenType::LBRACE),

            Some('}') => Ok(TokenType::RBRACE),

            Some('[') => Ok(TokenType::LBRACKET),

            Some(']') => Ok(TokenType::RBRACKET),

            Some(',') => Ok(TokenType::COMMA),

            Some('.') => Ok(TokenType::DOT),

            Some(':') => Ok(TokenType::COLON),

            Some('-') => match self.peek() {
                Some(&'=') => {
                    self.advance();

                    Ok(TokenType::MINUSASSIGN)
                }

                _ => Ok(TokenType::MINUS),
            },

            Some('+') => match self.peek() {
                Some(&'=') => {
                    self.advance();

                    Ok(TokenType::PLUSASSIGN)
                }

                _ => Ok(TokenType::PLUS),
            },

            Some(';') => Ok(TokenType::SEMICOLON),

            Some('*') => match self.peek() {
                Some(&'=') => {
                    self.advance();

                    Ok(TokenType::STARASSIGN)
                }

                _ => Ok(TokenType::STAR),
            },

            Some('?') => Ok(TokenType::QUESTION),

            Some('^') => Ok(TokenType::EXPONENTIAL),
            Some('%') => Ok(TokenType::MODULO),

            Some('"') => self.string(),

            Some('/') => match self.peek() {
                Some(&'/') => {
                    while self.peek() != Some(&'\n') && self.peek() != None {
                        self.advance();
                    }
                    self.line += 1;
                    self.column = 0;
                    Ok(TokenType::COMMENT)
                }

                Some(&'*') => {
                    self.advance();

                    while self.peek() != Some(&'*') && self.peek() != None {
                        if self.peek() == Some(&'\n') {
                            self.line += 1;
                            self.column = 0;
                        }
                        self.advance();
                    }


                    self.advance();

                    match self.peek() {
                        Some(&'/') => {
                            self.advance();
                            Ok(TokenType::COMMENT)
                        }

                        _ => {
                            let error = format!("Unclosed block comment on line {}", self.line);
                            Err(LexerError::UnclosedBlockComment(error))
                        }
                    }
                }
                _ => Ok(TokenType::SLASH),
            },

            Some('!') => match self.peek() {
                Some(&'=') => {
                    self.advance();

                    Ok(TokenType::BANGEQUAL)
                }

                _ => Ok(TokenType::BANG),
            },
            Some('=') => match self.peek() {
                Some(&'=') => {
                    self.advance();

                    Ok(TokenType::EQUALEQUAL)
                }

                _ => Ok(TokenType::ASSIGN),
            },

            Some('<') => match self.peek() {
                Some(&'=') => {
                    self.advance();
                    Ok(TokenType::LESSTHANEQUAL)
                }
                _ => Ok(TokenType::LESSTHAN),
            },

            Some('>') => match self.peek() {
                Some(&'=') => {
                    self.advance();
                    Ok(TokenType::GREATERTHANEQUAL)
                }
                _ => Ok(TokenType::GREATERTHAN),
            },

            Some(ch) => if ch.is_numeric() {
                self.number(ch)
            } else if self.ch_is_letter(ch) {
                let literal = self.identifier(ch);

                Ok(self.look_up_identifier(&literal))
            } else {
                Ok(TokenType::ILLEGAL)
            },

            None => Ok(TokenType::EOF),
        }
    }

    fn look_up_identifier(&self, identifier: &str) -> TokenType {
        match identifier {
            "pprint" => TokenType::PPRINT,
            // Class
            "class" => TokenType::CLASS,
            "super" => TokenType::SUPER,
            "this" => TokenType::THIS,
            // Functions and vars
            "fun" => TokenType::FUNCTION,
            "var" => TokenType::VAR,
            // Control Flow
            "if" => TokenType::IF,
            "else" => TokenType::ELSE,
            "for" => TokenType::FOR,
            "while" => TokenType::WHILE,
            "return" => TokenType::RETURN,
            "break" => TokenType::BREAK,
            "continue" => TokenType::CONTINUE,
            "do" => TokenType::DO,
            // Booleans
            "true" => TokenType::TRUE(true),
            "false" => TokenType::FALSE(false),
            "or" => TokenType::OR,
            "and" => TokenType::AND,
            "nil" => TokenType::NIL,
            _ => TokenType::IDENTIFIER(String::from(identifier)),
        }
    }
}
