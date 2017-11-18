use token::{Token, TokenType};
use nom::{alphanumeric, digit, IResult};
use std::str::{self, FromStr};




#[derive(Debug)]
pub struct Lexer<'a> {
    pub input: &'a str,
    line: i64,
    column: i64,
}



impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            input,
            line: 1,
            column: 1,
        }
    }

    
// (TokenType, column,line);

    fn token_with_info(&mut self, change: (TokenType, i64, i64)) -> Token {
        match change.0 {
            TokenType::TAB => {
                self.column += 4;
            }

            TokenType::NLINE => {
                self.line += 1;
                self.column =1;
            }

            _ => (),
        }

        self.line = self.line + change.2;
        self.column = self.column + change.1;
        Token {
            token: change.0,
            line: self.line,
            column: self.column,
        }
    }

    method!(
        lex_identifier<Lexer<'a>>(&[u8]) -> (TokenType,i64,i64),
        self,
        do_parse!(
            name:map_res!(call!(id),str::from_utf8) >>
            (look_up_identifier(name))
        )
    );

    fn filter(&self, mut i:Vec<Token>) -> Vec<Token> {
        i.retain(|t| t.token != TokenType::COMMENT || t.token != TokenType::NLINE|| t.token != TokenType::SPACE || t.token != TokenType::TAB );

        i

    }

    

    
    method!(lex_string<Lexer<'a>>(&[u8]) -> (TokenType,i64,i64), self, call!(string));
    method!(lex_number<Lexer<'a>>(&[u8]) ->(TokenType,i64,i64),self, alt!(float | int));
    method!(lex_whitespace<Lexer<'a>>(&[u8]) ->(TokenType,i64,i64),self,alt!(new_line |space|tab));
    method!(lexer_assign<Lexer<'a>>(&[u8]) -> (TokenType,i64,i64),self,alt!(assign | plus_assign| minus_assign | star_assign | slash_assign));
    method!(lex_operators<Lexer<'a>>(&[u8]) -> (TokenType,i64,i64),self,alt!(plus | minus | bang | star | slash | modulo | exp));
    method!(lex_punctuations<Lexer<'a>>(&[u8])-> (TokenType,i64,i64),self,alt!(dot | question | colon | comma | comment | semicolon | lparen | rparen | lbracket 
            | rbracket| lbrace | rbrace));
    method!(lex_comparison<Lexer<'a>>(&[u8]) -> (TokenType,i64,i64),self,alt!(less_than | greater_than | equal_equal | bang_equal | less_than_equal | greater_than_equal));
    method!(lex_illegal<Lexer<'a>>(&[u8]) -> (TokenType,i64,i64),self,do_parse!(take!(1) >> (TokenType::ILLEGAL,1,0)));
    method!(lex_token<Lexer<'a>>(&[u8]) -> Token,mut self,do_parse!(token:alt_complete!(call_m!(self.lex_whitespace)|call_m!(self.lex_operators) | 
    call_m!(self.lexer_assign) |call_m!(self.lex_comparison)|call_m!(self.lex_punctuations) |call_m!(self.lex_string) |call_m!(self.lex_number)|call_m!(self.lex_identifier) |
    call_m!(self.lex_illegal)) >> (self.token_with_info(token)))) ;
    method!(pub parse<Lexer<'a>>(&[u8]) -> Vec<Token>,mut self,do_parse!( tokens:many0!(call_m!(self.lex_token))  >> (self.filter(tokens))  ));
}

named!(id, take_while!(is_alphanumeric_or_underscore));


named!(
    new_line<(TokenType, i64, i64)>,
    do_parse!(tag!("\n") >> (TokenType::NLINE, 0, 1))
);
named!(space<(TokenType, i64, i64)>,
  do_parse!(tag!(" ") >> (TokenType::SPACE, 1, 0))
);

named!(
    tab<(TokenType, i64, i64)>,
    do_parse!(char!('\t') >> (TokenType::TAB, 4, 0))
);

fn is_alphanumeric_or_underscore(c: u8) -> bool {
    (c as char).is_alphanumeric() || (c as char) == '_'
}

fn look_up_identifier(id: &str) -> (TokenType, i64, i64) {
    match id {
        "pprint" => (TokenType::PPRINT, 6, 0),
        // Class
        "class" => (TokenType::CLASS, 5, 0),
        "super" => (TokenType::SUPER, 5, 0),
        "this" => (TokenType::THIS, 4, 0),
        // Functions and vars
        "fun" => (TokenType::FUNCTION, 3, 0),
        "var" => (TokenType::VAR, 3, 0),
        // Control Flow
        "if" => (TokenType::IF, 2, 0),
        "else" => (TokenType::ELSE, 4, 0),
        "for" => (TokenType::FOR, 3, 0),
        "while" => (TokenType::WHILE, 5, 0),
        "return" => (TokenType::RETURN, 5, 0),
        "break" => (TokenType::BREAK, 5, 0),
        "continue" => (TokenType::CONTINUE, 8, 0),
        "do" => (TokenType::DO, 2, 0),
        // Booleans
        "true" => (TokenType::TRUE(true), 4, 0),
        "false" => (TokenType::FALSE(false), 5, 0),
        "or" => (TokenType::OR, 2, 0),
        "and" => (TokenType::AND, 3, 0),
        "nil" => (TokenType::NIL, 3, 0),
        _ => (TokenType::IDENTIFIER(String::from(id)), id.len() as i64, 0),
    }
}


named!(
    int<(TokenType, i64, i64)>,
    do_parse!(
        int:map_res!(map_res!(recognize!(digit),str::from_utf8), |s| i64::from_str_radix(s,10)) >>
        (TokenType::INT(int),int.to_string().chars().count() as i64,0)
)
);

named!(
    float<(TokenType, i64, i64)>,
    do_parse!(
        ft:map_res!(map_res!(recognize!(
                alt_complete!(
                    delimited!(digit,tag!("."),opt!(complete!(digit))) |
                    delimited!(opt!(digit), tag!("."), digit))
            ),
            str::from_utf8), FromStr::from_str)
        >> (TokenType::FLOAT(ft),  ft.to_string().chars().count() as i64,0)
    )
);



// ASSIGNMENT

named!(
    assign<(TokenType, i64, i64)>,
    do_parse!(tag!("=") >> (TokenType::ASSIGN, 1, 0))
);
named!(
    plus_assign<(TokenType, i64, i64)>,
    do_parse!(tag!("+=") >> (TokenType::PLUSASSIGN, 2, 0))
);
named!(
    minus_assign<(TokenType, i64, i64)>,
    do_parse!(tag!("-=") >> (TokenType::MINUSASSIGN, 2, 0))
);
named!(
    star_assign<(TokenType, i64, i64)>,
    do_parse!(tag!("*=") >> (TokenType::STARASSIGN, 2, 0))
);
named!(
    slash_assign<(TokenType, i64, i64)>,
    do_parse!(tag!("/=") >> (TokenType::SLASHASSIGN, 2, 0))
);


// Operator
named!(
    plus<(TokenType, i64, i64)>,
    do_parse!(tag!("+") >> (TokenType::PLUS, 1, 0))
);
named!(
    minus<(TokenType, i64, i64)>,
    do_parse!(tag!("-") >> (TokenType::MINUS, 1, 0))
);
named!(
    bang<(TokenType, i64, i64)>,
    do_parse!(tag!("!") >> (TokenType::BANG, 1, 0))
);
named!(
    star<(TokenType, i64, i64)>,
    do_parse!(tag!("*") >> (TokenType::STAR, 1, 0))
);
named!(
    slash<(TokenType, i64, i64)>,
    do_parse!(tag!("/") >> (TokenType::SLASH, 1, 0))
);
named!(
    modulo<(TokenType, i64, i64)>,
    do_parse!(tag!("%") >> (TokenType::MODULO, 1, 0))
);
named!(
    exp<(TokenType, i64, i64)>,
    do_parse!(tag!("^") >> (TokenType::EXPONENTIAL, 1, 0))
);



// PUNCTUCATIon

named!(
    dot<(TokenType, i64, i64)>,
    do_parse!(tag!(".") >> (TokenType::DOT, 1, 0))
);
named!(
    question<(TokenType, i64, i64)>,
    do_parse!(tag!("?") >> (TokenType::QUESTION, 1, 0))
);
named!(
    colon<(TokenType, i64, i64)>,
    do_parse!(tag!(":") >> (TokenType::COLON, 1, 0))
);
named!(
    comma<(TokenType, i64, i64)>,
    do_parse!(tag!(",") >> (TokenType::COMMA, 1, 0))
);
named!(
    comment<(TokenType, i64, i64)>,
    do_parse!(tag!("//") >> (TokenType::COMMENT, 2, 0))
);
named!(
    semicolon<(TokenType, i64, i64)>,
    do_parse!(tag!(";") >> (TokenType::SEMICOLON, 1, 0))
);
named!(
    lparen<(TokenType, i64, i64)>,
    do_parse!(tag!("(") >> (TokenType::LPAREN, 1, 0))
);
named!(
    rparen<(TokenType, i64, i64)>,
    do_parse!(tag!(")") >> (TokenType::RPAREN, 1, 0))
);
named!(
    lbracket<(TokenType, i64, i64)>,
    do_parse!(tag!("[") >> (TokenType::LBRACKET, 1, 0))
);
named!(
    rbracket<(TokenType, i64, i64)>,
    do_parse!(tag!("]") >> (TokenType::RBRACKET, 1, 0))
);
named!(
    lbrace<(TokenType, i64, i64)>,
    do_parse!(tag!("{") >> (TokenType::LBRACE, 1, 0))
);
named!(
    rbrace<(TokenType, i64, i64)>,
    do_parse!(tag!("}") >> (TokenType::RBRACE, 1, 0))
);


// Comparison


named!(
    less_than<(TokenType, i64, i64)>,
    do_parse!(tag!("<") >> (TokenType::LESSTHAN, 1, 0))
);
named!(
    greater_than<(TokenType, i64, i64)>,
    do_parse!(tag!(">") >> (TokenType::GREATERTHAN, 1, 0))
);
named!(
    equal_equal<(TokenType, i64, i64)>,
    do_parse!(tag!("==") >> (TokenType::EQUALEQUAL, 1, 0))
);
named!(
    bang_equal<(TokenType, i64, i64)>,
    do_parse!(tag!("!=") >> (TokenType::BANGEQUAL, 1, 0))
);
named!(
    less_than_equal<(TokenType, i64, i64)>,
    do_parse!(tag!("<=") >> (TokenType::LESSTHANEQUAL, 2, 0))
);
named!(
    greater_than_equal<(TokenType, i64, i64)>,
    do_parse!(tag!("=>") >> (TokenType::GREATERTHANEQUAL, 2, 0))
);


named!(
    string<(TokenType,i64,i64)>,
    do_parse!(
        string:delimited!(
            tag!("\""),
            map!(map_res!(escaped!(call!(alphanumeric), '\\', is_a!("\"n\\")), str::from_utf8),String::from),
            tag!("\"")) >>
         (TokenType::STRING(string.clone()), string.len() as i64,0)
        )
);
