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
    input: &'a str,
    chars: CharPosition<'a>,
    lookahead: Option<(Postition, char)>,
    end: Postition,
}

#[derive(Debug, Copy, Clone)]
pub struct Postition {
    line: i64,
    column: i64,
    absolute: usize,
}

impl Postition {
    fn shift(mut self, ch: char) -> Self {
        if ch == '\n' {
            self.line += 1;
            self.column = 1;
        } else if ch == '\t' {
            self.column += 4;
        } else {
            self.column += 1;
        }

        self.absolute += ch.len_utf8();
        self
    }
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
            if terminate(ch) {
                return (end, self.slice(start, end));
            }
            self.advance();
        }

        (self.end, self.slice(start, self.end))
    }

    fn line_comment(&mut self, start: Postition) -> TokenType {
        let (end, comment) = self.take_while(start, |ch| ch != '\n');

        TokenType::COMMENT
    }

    fn block_comment(&mut self, start: Postition) -> Result<TokenType, LexerError> {
        self.advance(); // Eats the '*'

        loop {
            self.advance(); // Eats the '*'

            match self.lookahead {
                Some((end, '/')) => {
                    self.advance();
                    return Ok(TokenType::COMMENT);
                }
                Some((_, _)) => continue,

                None => return Err(LexerError::UnclosedBlockComment(String::from("Unclosed"))),
            }
        }
    }

    fn comparison(&mut self, start: Postition) -> TokenType {
        let (end, op) = self.take_while(start, |c| is_operator_char(c));

        match op {
            "=" => TokenType::ASSIGN,
            ">" => TokenType::LESSTHAN,
            "<" => TokenType::LESSTHAN,
            "!" => TokenType::BANG,
            "==" => TokenType::EQUALEQUAL,
            "=>" => TokenType::GREATERTHANEQUAL,
            "<=" => TokenType::LESSTHANEQUAL,
            "!=" => TokenType::BANGEQUAL,
            _ => unreachable!(),
        }
    }

    fn punctuation(&mut self, start: Postition) -> TokenType {
        let (end, punc) = self.take_while(start, |c| is_punctuation_char(c));

        match punc {
            "(" => TokenType::LPAREN,
            ")" => TokenType::RPAREN,
            "{" => TokenType::LBRACE,
            "}" => TokenType::RBRACE,
            "[" => TokenType::LBRACKET,
            "]" => TokenType::RBRACKET,
            ":" => TokenType::COLON,
            ";" => TokenType::SEMICOLON,
            "," => TokenType::COMMA,
            "?" => TokenType::QUESTION,
            "." => TokenType::DOT,
            _ => unreachable!(),
        }
    }

    fn string_literal(&mut self, start: Postition) -> Result<TokenType,LexerError> {
        let mut string = String::new();

        while let Some((next,ch)) = self.advance() {
            match ch {
                '"' => {
                    let end = next.shift(ch);

                    return Ok(TokenType::STRING(string))
            
                },

                ch => string.push(ch),
            }

        }

        Err(LexerError::UnclosedString(String::from("adaf")))

    }

    fn number(&mut self, start: Postition) -> TokenType {
        unimplemented!()
    }

    fn identifier(&mut self, start: Postition) -> TokenType {
        let (end, ident) = self.take_while(start, |c| ch_is_letter(c));
        look_up_identifier(ident)
    }
}

impl <'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>,LexerError>;

    fn next(&mut self) -> Option<Result<Token<'a>,LexerError>> {
        while let Some((start,ch)) = self.advance() {
            match ch {
                ',' => Some(Ok(token_with_info(TokenType::COMMA))),
                '{' => Some(Ok(token_with_info(TokenType::RBRACE))),
                '}'=> Some(Ok(token_with_info(TokenType::LBRACE))),
                '['=> Some(Ok(token_with_info(TokenType::LBRACKET))),
                ']'=> Some(Ok(token_with_info(TokenType::RBRACKET))),
                '('=> Some(Ok(token_with_info(TokenType::LPAREN))),
                ')'=> Some(Ok(token_with_info(TokenType::RPAREN))),
                '"'=> ,
                '.'=> Some(Ok(token_with_info(TokenType::DOT))),
                '?'=> Some(Ok(token_with_info(TokenType::))),
                ';'=> Some(Ok(token_with_info(TokenType::LBRACE))),
                ':'=> Some(Ok(token_with_info(TokenType::LBRACE))),
                _ => unimplemented!()
            }
        }
    }

}

fn ch_is_letter(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_'
}


fn is_operator_char(ch: char) -> bool {
    ch == '<' || ch == '>' || ch == '=' || ch == '!'
}


fn is_punctuation_char(ch: char) -> bool {
    ch == '[' || ch == ']' || ch == '(' || ch == ')' || ch == '{' || ch == '}' || ch == ','
        || ch == '?' || ch == '.' || ch == ':'
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



// fn peek(&mut self) -> Option<&char> {
//     self.chars.peek()
// }


// fn token_with_info(&mut self, token: TokenType) -> Token {
//     Token {
//         token,
//         line: self.line,
//         column: self.column,
//     }
// }



// fn advance(&mut self) -> Option<char> {
//     self.column += 1;
//     self.chars.next()
// }


// fn peek_ch_is_letter(&mut self) -> bool {
//     // Checks if character is a letter
//     match self.peek() {
//         Some(&ch) => self.ch_is_letter(ch),
//         None => false,
//     }
// }



// fn skip_whitespace(&mut self) -> () {
//     // Skips through white space

//     while let Some(&c) = self.peek() {
//         if c == '\n' {
//             self.line += 1;
//             self.column = 1;
//         } else if !c.is_whitespace() {
//             break;
//         }
//         self.advance();
//     }
// }


// fn number(&mut self, first: char) -> Result<TokenType, LexerError> {
//     // Parses the number and returns the f64 value

//     let mut number = String::new();
//     number.push(first);

//     while let Some(&ch) = self.peek() {
//         if !ch.is_numeric() {
//             break;
//         }
//         number.push(self.advance().unwrap())
//     }

//     match self.peek() {
//         Some(&'.') => {
//             self.advance();
//             while let Some(ch) = self.peek() {
//                 if !ch.is_numeric() {
//                     break;
//                 }

//                 number.push(self.advance().unwrap());
//             }

//             OkTokenType::FLOAT(number.parse().unwrap()))
//         }

//         Some(ch) => {
//             if ch.is_alphabetic() {
//                 return Err(LexerError::InvalidFloat(String::from("asdad")));
//             }

//             OkTokenType::INT(number.parse().unwrap()))
//         }

//         None => Err(LexerError::EOF),
//     }
// }

// // var m = 10;
// fn identifier(&mut self, first: char) -> String {
//     let mut ident = String::new();



//     ident.push(first);

//     while self.peek_ch_is_letter() {
//         ident.push(self.advance().unwrap())
//     }


//     ident
// }

// fn escape_code(&mut self) -> Result<char, LexerError> {
//     match self.advance() {
//         Some('\'') => Ok('\''),
//         Some('"') => Ok('"'),
//         Some('\\') => Ok('\\'),
//         Some('/') => Ok('/'),
//         Some('n') => Ok('\n'),
//         Some('r') => Ok('\r'),
//         Some('t') => Ok('\t'),
//         None => Err(LexerError::EOF),
//         Some(ch) => Err(LexerError::EscapeCode),
//     }
// }


// fn string(&mut self) -> Result<TokenType, LexerError> {
//     let mut string = String::new();

//     while let Some(ch) = self.advance() {
//         match ch {
//             '"' => {
//                 self.line += 1;
//                 return OkTokenType::STRING(string));
//             }

//             '\\' => string.push(self.escape_code()?),

//             ch => string.push(ch),
//         }
//     }

//     unimplemented!()
// }





// fn ch_is_letter(&mut self, ch: char) -> bool {
//     ch.is_alphanumeric() || ch == '_'
// }

// /// Gets the next token from the Lexer
// fn next_token(&mut self) -> Result<TokenType, LexerError> {
//     self.skip_whitespace();

//     match self.advance() {
//         Some('(') => OkTokenType::LPAREN),

//         Some(')') => OkTokenType::RPAREN),

//         Some('{') => OkTokenType::LBRACE),

//         Some('}') => OkTokenType::RBRACE),

//         Some('[') => OkTokenType::LBRACKET),

//         Some(']') => OkTokenType::RBRACKET),

//         Some(',') => OkTokenType::COMMA),

//         Some('.') => OkTokenType::DOT),

//         Some(':') => OkTokenType::COLON),

//         Some('-') => match self.peek() {
//             Some(&'=') => {
//                 self.advance();

//                 OkTokenType::MINUSASSIGN)
//             }

//             _ => OkTokenType::MINUS),
//         },

//         Some('+') => match self.peek() {
//             Some(&'=') => {
//                 self.advance();

//                 OkTokenType::PLUSASSIGN)
//             }

//             _ => OkTokenType::PLUS),
//         },

//         Some(';') => OkTokenType::SEMICOLON),

//         Some('*') => match self.peek() {
//             Some(&'=') => {
//                 self.advance();

//                 OkTokenType::STARASSIGN)
//             }

//             _ => OkTokenType::STAR),
//         },

//         Some('?') => OkTokenType::QUESTION),

//         Some('^') => OkTokenType::EXPONENTIAL),
//         Some('%') => OkTokenType::MODULO),

//         Some('"') => self.string(),

//         Some('/') => match self.peek() {
//             Some(&'/') => {
//                 while self.peek() != Some(&'\n') && self.peek() != None {
//                     self.advance();
//                 }
//                 self.line += 1;
//                 self.column = 0;
//                 OkTokenType::COMMENT)
//             }

//             Some(&'*') => {
//                 self.advance();

//                 while self.peek() != Some(&'*') && self.peek() != None {
//                     if self.peek() == Some(&'\n') {
//                         self.line += 1;
//                         self.column = 0;
//                     }
//                     self.advance();
//                 }


//                 self.advance();

//                 match self.peek() {
//                     Some(&'/') => {
//                         self.advance();
//                         OkTokenType::COMMENT)
//                     }

//                     _ => {
//                         let error = format!("Unclosed block comment on line {}", self.line);
//                         Err(LexerError::UnclosedBlockComment(error))
//                     }
//                 }
//             }
//             _ => OkTokenType::SLASH),
//         },

//         Some('!') => match self.peek() {
//             Some(&'=') => {
//                 self.advance();

//                 OkTokenType::BANGEQUAL)
//             }

//             _ => OkTokenType::BANG),
//         },
//         Some('=') => match self.peek() {
//             Some(&'=') => {
//                 self.advance();

//                 OkTokenType::EQUALEQUAL)
//             }

//             _ => OkTokenType::ASSIGN),
//         },

//         Some('<') => match self.peek() {
//             Some(&'=') => {
//                 self.advance();
//                 OkTokenType::LESSTHANEQUAL)
//             }
//             _ => OkTokenType::LESSTHAN),
//         },

//         Some('>') => match self.peek() {
//             Some(&'=') => {
//                 self.advance();
//                 OkTokenType::GREATERTHANEQUAL)
//             }
//             _ => OkTokenType::GREATERTHAN),
//         },

//         Some(ch) => if ch.is_numeric() {
//             self.number(ch)
//         } else if self.ch_is_letter(ch) {
//             let literal = self.identifier(ch);

//             Ok(self.look_up_identifier(&literal))
//         } else {
//             OkTokenType::ILLEGAL)
//         },

//         None => OkTokenType::EOF),
//     }
// }

// fn look_up_identifier(&self, identifier: &str) -> TokenType {
//     match identifier {
//         "pprint" => TokenType::PPRINT,
//         // Class
//         "class" => TokenType::CLASS,
//         "super" => TokenType::SUPER,
//         "this" => TokenType::THIS,
//         // Functions and vars
//         "fun" => TokenType::FUNCTION,
//         "var" => TokenType::VAR,
//         // Control Flow
//         "if" => TokenType::IF,
//         "else" => TokenType::ELSE,
//         "for" => TokenType::FOR,
//         "while" => TokenType::WHILE,
//         "return" => TokenType::RETURN,
//         "break" => TokenType::BREAK,
//         "continue" => TokenType::CONTINUE,
//         "do" => TokenType::DO,
//         // Booleans
//         "true" => TokenType::TRUE(true),
//         "false" => TokenType::FALSE(false),
//         "or" => TokenType::OR,
//         "and" => TokenType::AND,
//         "nil" => TokenType::NIL,
//         _ => TokenType::IDENTIFIER(String::from(identifier)),
//     }
// }
// }
