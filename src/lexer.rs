use token::{Token, TokenType};
use nom::{alphanumeric, digit};
use std::str::{self, FromStr};




#[derive(Debug)]
pub struct Lexer<'a> {
    pub input: &'a str,
    line:i64,
    column:i64,
}

impl<'a> Lexer<'a> {
    pub fn new(input:&'a str) -> Self {
        Lexer {
            input,
            line:1,
            column:1,
        }
    }

    fn token_with_info(&mut self,change:(TokenType,i64)) -> Token {
    self.line = self.line + change.1;
        Token {
            token:change.0,
            line:self.line,
            column:self.column
        }
    }

    method!(
        lex_identifier<Lexer<'a>>(&[u8]) -> (TokenType,i64),
        self,
        do_parse!(
            name:map_res!(call!(id),str::from_utf8) >>
            (look_up_identifier(name))
        )
    );
    method!(lex_string<Lexer<'a>>(&[u8]) -> (TokenType,i64), self, call!(string));

    method!(lex_number<Lexer<'a>>(&[u8]) ->(TokenType,i64),self, alt!(float | int));

    method!(lexer_assign<Lexer<'a>>(&[u8]) -> (TokenType,i64),self,alt!(assign | plus_assign| minus_assign | star_assign | slash_assign));
    method!(lex_operators<Lexer<'a>>(&[u8]) -> (TokenType,i64),self,alt!(plus | minus | bang | star | slash | modulo | exp));
    method!(lex_punctuations<Lexer<'a>>(&[u8])-> (TokenType,i64),self,alt!(dot | question | colon | comma | comment | semicolon | lparen | rparen | lbracket 
            | rbracket| lbrace | rbrace));
    method!(lex_comparison<Lexer<'a>>(&[u8]) -> (TokenType,i64),self,alt!(less_than | greater_than | equal_equal | bang_equal | less_than_equal | greater_than_equal));
    method!(lex_illegal<Lexer<'a>>(&[u8]) -> (TokenType,i64),self,do_parse!(take!(1) >> (TokenType::ILLEGAL,1)));
    method!(lex_token<Lexer<'a>>(&[u8]) -> Token,mut self,do_parse!(token:alt_complete!(call_m!(self.lex_operators) | 
    call_m!(self.lexer_assign) |call_m!(self.lex_comparison)|call_m!(self.lex_punctuations) |call_m!(self.lex_string) |call_m!(self.lex_number)|call_m!(self.lex_identifier) |
    call_m!(self.lex_illegal)) >> (self.token_with_info(token)))) ;
    method!(pub lex_tokens<Lexer<'a>>(&[u8]) -> Vec<Token>,mut self, ws!(many0!(call_m!(self.lex_token))));

}

named!(id,take_while!(is_alphanumeric_or_underscore));


fn is_alphanumeric_or_underscore(c: u8) -> bool {
    (c as char).is_alphanumeric() || (c as char) == '_'
}

fn look_up_identifier(id: &str) -> (TokenType,i64) {
    match id {
        "pprint" => (TokenType::PPRINT,6),
        // Class
        "class" => (TokenType::CLASS,5),
        "super" => (TokenType::SUPER,5),
        "this" => (TokenType::THIS,4),
        // Functions and vars
        "fun" => (TokenType::FUNCTION,3),
        "var" => (TokenType::VAR,3),
        // Control Flow
        "if" => (TokenType::IF,2),
        "else" => (TokenType::ELSE,4),
        "for" => (TokenType::FOR,3),
        "while" => (TokenType::WHILE,5),
        "return" => (TokenType::RETURN,5),
        "break" => (TokenType::BREAK,5),
        "continue" => (TokenType::CONTINUE,8),
        "do" => (TokenType::DO,2),
        // Booleans
        "true" => (TokenType::TRUE(true),4),
        "false" => (TokenType::FALSE(false),5),
        "or" => (TokenType::OR,2),
        "and" => (TokenType::AND,3),
        "nil" => (TokenType::NIL,3),
        _ => (TokenType::IDENTIFIER(String::from(id)),id.len() as i64),
    }
}


named!(
    int<(TokenType,i64)>,
    do_parse!(
        int:map_res!(map_res!(recognize!(digit),str::from_utf8), |s| i64::from_str_radix(s,10)) >>
        (TokenType::INT(int),int.to_string().chars().count() as i64)
));

named!(
    float<(TokenType,i64)>,
    do_parse!(
        ft:map_res!(map_res!(recognize!(
                alt_complete!(
                    delimited!(digit,tag!("."),opt!(complete!(digit))) |
                    delimited!(opt!(digit), tag!("."), digit))
            ),
            str::from_utf8), FromStr::from_str)
        >> (TokenType::FLOAT(ft),  ft.to_string().chars().count() as i64)
    )
);



// ASSIGNMENT

named!(
    assign<(TokenType,i64)>,
    do_parse!(tag!("=") >> (TokenType::ASSIGN,1))
);
named!(
    plus_assign<(TokenType,i64)>,
    do_parse!(tag!("+=") >> (TokenType::PLUSASSIGN,2))
);
named!(
    minus_assign<(TokenType,i64)>,
    do_parse!(tag!("-=") >> (TokenType::MINUSASSIGN,2))
);
named!(
    star_assign<(TokenType,i64)>,
    do_parse!(tag!("*=") >> (TokenType::STARASSIGN,2))
);
named!(
    slash_assign<(TokenType,i64)>,
    do_parse!(tag!("/=") >> (TokenType::SLASHASSIGN,2))
);


// Operator
named!(plus<(TokenType,i64)>, do_parse!(tag!("+") >> (TokenType::PLUS,1)));
named!(minus<(TokenType,i64)>, do_parse!(tag!("-") >> (TokenType::MINUS,1)));
named!(bang<(TokenType,i64)>, do_parse!(tag!("!") >> (TokenType::BANG,1)));
named!(star<(TokenType,i64)>, do_parse!(tag!("*") >> (TokenType::STAR,1)));
named!(slash<(TokenType,i64)>, do_parse!(tag!("/") >> (TokenType::SLASH,1)));
named!(
    modulo<(TokenType,i64)>,
    do_parse!(tag!("%") >> (TokenType::MODULO,1))
);
named!(
    exp<(TokenType,i64)>,
    do_parse!(tag!("^") >> (TokenType::EXPONENTIAL,1))
);



// PUNCTUCATIon

named!(dot<(TokenType,i64)>, do_parse!(tag!(".") >> (TokenType::DOT,1)));
named!(
    question<(TokenType,i64)>,
    do_parse!(tag!("?") >> (TokenType::QUESTION,1))
);
named!(colon<(TokenType,i64)>, do_parse!(tag!(":") >> (TokenType::COLON,1)));
named!(comma<(TokenType,i64)>, do_parse!(tag!(",") >> (TokenType::COMMA,1)));
named!(
    comment<(TokenType,i64)>,
    do_parse!(tag!("//") >> (TokenType::COMMENT,2))
);
named!(
    semicolon<(TokenType,i64)>,
    do_parse!(tag!(";") >> (TokenType::SEMICOLON,1))
);
named!(
    lparen<(TokenType,i64)>,
    do_parse!(tag!("(") >> (TokenType::LPAREN,1))
);
named!(
    rparen<(TokenType,i64)>,
    do_parse!(tag!(")") >> (TokenType::RPAREN,1))
);
named!(
    lbracket<(TokenType,i64)>,
    do_parse!(tag!("[") >> (TokenType::LBRACKET,1))
);
named!(
    rbracket<(TokenType,i64)>,
    do_parse!(tag!("]") >> (TokenType::RBRACKET,1))
);
named!(
    lbrace<(TokenType,i64)>,
    do_parse!(tag!("{") >> (TokenType::LBRACE,1))
);
named!(
    rbrace<(TokenType,i64)>,
    do_parse!(tag!("}") >> (TokenType::RBRACE,1))
);


// Comparison


named!(
    less_than<(TokenType,i64)>,
    do_parse!(tag!("<") >> (TokenType::LESSTHAN,1))
);
named!(
    greater_than<(TokenType,i64)>,
    do_parse!(tag!(">") >> (TokenType::GREATERTHAN,1))
);
named!(
    equal_equal<(TokenType,i64)>,
    do_parse!(tag!("==") >> (TokenType::EQUALEQUAL,1))
);
named!(
    bang_equal<(TokenType,i64)>,
    do_parse!(tag!("!=") >> (TokenType::BANGEQUAL,1))
);
named!(
    less_than_equal<(TokenType,i64)>,
    do_parse!(tag!("<=") >> (TokenType::LESSTHANEQUAL,2))
);
named!(
    greater_than_equal<(TokenType,i64)>,
    do_parse!(tag!("=>") >> (TokenType::GREATERTHANEQUAL,2))
);

named!(
    string<(TokenType,i64)>,
    do_parse!(
        string:delimited!(
            tag!("\""),
            map!(map_res!(escaped!(call!(alphanumeric), '\\', is_a!("\"n\\")), str::from_utf8),String::from),
            tag!("\"")) >>
         (TokenType::STRING(string.clone()), string.len() as i64)
        )
);
