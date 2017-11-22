
use token::{Postition, Token, TokenType};
use std::str::Chars;

use std::fmt::{Display, Formatter};
use std::fmt;
#[derive(Debug)]
pub enum LexerError {
    UnclosedString(String),
    UnclosedBlockComment(String),
    InvalidFloat(String),
    EOF,
    EscapeCode, // Add the char
    Unexpected(char),
}

impl Display for LexerError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            LexerError::UnclosedString(ref e) => write!(f, "unclosed string {}", e),
            LexerError::EOF => write!(f, "Unexpected EOf"),
            LexerError::EscapeCode => write!(f, "Unexpected escape code"),
            LexerError::UnclosedBlockComment(ref e) => write!(f, "unclosed block comment {}", e),
            LexerError::InvalidFloat(ref e) => write!(f, "Inavlid Float {}", e),
            LexerError::Unexpected(ref c) => write!(f, "Unexpected char {}",c),
        }
    }
}



#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    // A lexer instance
    input: &'a str,
    chars: CharPosition<'a>,
    lookahead: Option<(Postition, char)>,
    end: Postition,
}

#[derive(Debug, Clone)]
pub struct CharPosition<'a> {
    pos: Postition,
    chars: Chars<'a>,
}

impl<'a> CharPosition<'a> {
    fn new(input: &'a str) -> Self {
        CharPosition {
            pos: Postition {
                line: 1,
                column: 1,
                absolute: 0,
            },
            chars: input.chars(),
        }
    }
}
impl<'a> Iterator for CharPosition<'a> {
    type Item = (Postition, char);

    fn next(&mut self) -> Option<(Postition, char)> {
        self.chars.next().map(|ch| {
            let pos = self.pos;
            self.pos = self.pos.shift(ch);
            (pos, ch)
        })
    }
}


impl<'a> Lexer<'a> {
    /// Returns a new Lexer
    pub fn new(input: &'a str) -> Lexer {
        let mut chars = CharPosition::new(input);
        let end = chars.pos;
        Lexer {
            input: input,
            lookahead: chars.next(),
            chars: chars,
            end: end,
        }
    }

    fn advance(&mut self) -> Option<(Postition, char)> {
        match self.lookahead {
            Some((pos, ch)) => {
                self.end = self.end.shift(ch);
                self.lookahead = self.chars.next();
                Some((pos, ch))
            }

            None => None,
        }
    }

    fn slice(&self, start: Postition, end: Postition) -> &'a str {
        &self.input[start.absolute..end.absolute]
    }

    fn take_while<F>(&mut self, start: Postition, mut terminate: F) -> (Postition, &'a str)
    where
        F: FnMut(char) -> bool,
    {
        while let Some((end, ch)) = self.lookahead {
            if !terminate(ch) {
                return (end, self.slice(start, end));
            }
            self.advance();
            
        }

        (self.end, self.slice(start, self.end))
    }

    fn peek<F>(&mut self, mut check: F) -> bool
    where
        F: FnMut(char) -> bool,
    {
        self.lookahead.map_or(false, |(_, ch)| check(ch))
    }

    fn line_comment(&mut self, start: Postition) -> Token<'a> {
        let (_, _) = self.take_while(start, |ch| ch != '\n');

        Token {
            token:TokenType::COMMENT,
            pos:start
        }
    }

    fn block_comment(&mut self, start: Postition) -> Result<Token<'a>, LexerError> {
        self.advance(); // Eats the '*'
        loop {
            self.advance(); // Eats the '*'

            match self.lookahead {
                Some((_, '/')) => {
                    self.advance();
                    return Ok(Token{token:TokenType::COMMENT,pos:start});
                }
                Some((_, _)) => continue,

                None => return Err(LexerError::UnclosedBlockComment(String::from("Unclosed"))),
            }
        }
    }


    fn string_literal(&mut self, start: Postition) -> Result<Token<'a>, LexerError> {
        let mut string = String::new();

        while let Some((_, ch)) = self.advance() {
            match ch {
                '"' => {
                    return Ok(Token {
                        token: TokenType::STRING(string),
                        pos: start,
                    });
                }

                ch => string.push(ch),
            }
        }

        Err(LexerError::UnclosedString(String::from("adaf")))
    }

    fn number(&mut self, start: Postition) -> Result<Token<'a>, LexerError> {
        let (_, int) = self.take_while(start, |c| c.is_numeric());

        let (_, token) = match self.lookahead {
            Some((_, '.')) => {
                self.advance();

                let (_, float) = self.take_while(start, |c| c.is_numeric());

                match self.lookahead {
                    Some((_, ch)) if ch.is_alphabetic() => {
                        return Err(LexerError::Unexpected(ch)); // Change
                    }

                    _ => (start, TokenType::FLOAT(float.parse().unwrap())),
                }
            }

            Some((_, ch)) if ch.is_alphabetic() => return Err(LexerError::Unexpected(ch)), // Change the slicened start
            None | Some(_) => {
                if let Ok(val) = int.parse() {
                    (start, TokenType::INT(val))
                } else {
                    return Err(LexerError::EOF); // change
                }
            }
        };

        Ok(Token {
            token: token,
            pos: start,
        })
    }

    fn identifier(&mut self, start: Postition) -> Token<'a> {
        let (_, ident) = self.take_while(start, is_letter_ch);
        Token {
            token: look_up_identifier(ident),
            pos: start,
        }
    }
}


impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, LexerError>;

    fn next(&mut self) -> Option<Result<Token<'a>, LexerError>> {
        while let Some((start, ch)) = self.advance() {
            return match ch {
                '.' => Some(Ok(token_with_info(TokenType::DOT, start))),
                '?' => Some(Ok(token_with_info(TokenType::QUESTION, start))),
                ';' => Some(Ok(token_with_info(TokenType::SEMICOLON, start))),
                '{' => Some(Ok(token_with_info(TokenType::RBRACE, start))),
                '}' => Some(Ok(token_with_info(TokenType::LBRACE, start))),
                '[' => Some(Ok(token_with_info(TokenType::LBRACKET, start))),
                ']' => Some(Ok(token_with_info(TokenType::RBRACKET, start))),
                '(' => Some(Ok(token_with_info(TokenType::LPAREN, start))),
                ')' => Some(Ok(token_with_info(TokenType::RPAREN, start))),
                ',' => Some(Ok(token_with_info(TokenType::COMMA, start))),
                ':' => Some(Ok(token_with_info(TokenType::COLON, start))),
                '^' => Some(Ok(token_with_info(TokenType::EXPONENTIAL, start))),
                '%' => Some(Ok(token_with_info(TokenType::MODULO, start))),
                '"' => Some(self.string_literal(start)),

                '=' => if self.peek(|ch| ch == '=') {
                    self.advance();
                    Some(Ok(token_with_info(TokenType::EQUALEQUAL, start)))
                } else {
                    self.advance();
                    Some(Ok(token_with_info(TokenType::ASSIGN, start)))
                },

                '+' => if self.peek(|ch| ch == '=') {
                    self.advance();
                    Some(Ok(token_with_info(TokenType::PLUSASSIGN, start)))
                } else {
                    self.advance();
                    Some(Ok(token_with_info(TokenType::PLUS, start)))
                },

                '-' => if self.peek(|ch| ch == '=') {
                    self.advance();
                    Some(Ok(token_with_info(TokenType::MINUSASSIGN, start)))
                } else {
                    Some(Ok(token_with_info(TokenType::MINUS, start)))
                },

                '*' => if self.peek(|ch| ch == '=') {
                    self.advance();
                    Some(Ok(token_with_info(TokenType::STARASSIGN, start)))
                } else {
                    Some(Ok(token_with_info(TokenType::STAR, start)))
                },

                '/' => if self.peek(|ch| ch == '=') {
                    self.advance();
                    Some(Ok(token_with_info(TokenType::SLASHASSIGN, start)))
                } else if self.peek(|ch| ch == '/') {
                    self.advance();
                    Some(Ok(self.line_comment(start)))
                }else if self.peek(|ch| ch == '*') {
                    Some(self.block_comment(start))
                }
                else {
                    Some(Ok(token_with_info(TokenType::SLASH, start)))
                },

                '!' => if self.peek(|ch| ch == '=') {
                    self.advance();
                    Some(Ok(token_with_info(TokenType::BANGEQUAL, start)))
                } else {
                    Some(Ok(token_with_info(TokenType::BANG, start)))
                },

                '>' => if self.peek(|ch| ch == '=') {
                    self.advance();
                    Some(Ok(token_with_info(TokenType::GREATERTHANEQUAL, start)))
                } else {
                    Some(Ok(token_with_info(TokenType::GREATERTHAN, start)))
                },
                '<' => if self.peek(|ch| ch == '=') {
                    self.advance();
                    Some(Ok(token_with_info(TokenType::LESSTHANEQUAL, start)))
                } else {
                    Some(Ok(token_with_info(TokenType::LESSTHAN, start)))
                },
                
                ch if ch.is_numeric() => Some(self.number(start)),
                ch if is_letter_ch(ch) => Some(Ok(self.identifier(start))), 
                ch if ch.is_whitespace() => continue,
                ch => Some(Err(LexerError::Unexpected(ch)))
            };
        }

        Some(Ok(Token {
            token: TokenType::EOF,
            pos: self.end,
        }))
    }
}


fn token_with_info(token: TokenType, pos: Postition) -> Token {
    Token { token, pos }
}

fn is_letter_ch(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_'
}



fn is_digit(ch: char) -> bool {
    ch.is_digit(10)
}






#[inline]
fn look_up_identifier(id: &str) -> TokenType {
    match id {
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
        _ => TokenType::IDENTIFIER(id),
    }
}
