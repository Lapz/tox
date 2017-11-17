use token::{Token, TokenType};
use nom::{alphanumeric, digit, is_alphanumeric, IResult};
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

    fn token_with_info(&self,token:TokenType) -> Token {
        Token {
            token,
            line:self.line,
            column:self.column
        }
    }

    method!(
        lex_identifier<Lexer<'a>>(&[u8]) -> TokenType,
        self,
        do_parse!(
            name:map_res!(call!(id),str::from_utf8) >>
            (look_up_identifier(name))
        )
    );
    method!(lex_string<Lexer<'a>>(&[u8]) -> TokenType, self, call!(string));

    method!(lex_number<Lexer<'a>>(&[u8]) -> TokenType,self, alt!(float | int));

    method!(lexer_assign<Lexer<'a>>(&[u8]) -> TokenType,self,alt!(assign | minus_assign | star_assign | slash_assign));
    method!(lex_operators<Lexer<'a>>(&[u8]) -> TokenType,self,alt!(plus | minus | bang | star | slash | modulo | exp));
    method!(lex_punctuations<Lexer<'a>>(&[u8]) -> TokenType,self,alt!(dot | question | colon | comma | comment | semicolon | lparen | rparen | lbracket 
            | rbracket| lbrace | rbrace));
    method!(lex_comparison<Lexer<'a>>(&[u8]) -> TokenType,self,alt!(less_than | greater_than | equal_equal | bang_equal | less_than_equal | greater_than_equal));
    method!(lex_illegal<Lexer<'a>>(&[u8]) -> TokenType,self,do_parse!(take!(1) >> (TokenType::ILLEGAL)));
    method!(lex_token<Lexer<'a>>(&[u8]) -> Token,mut self,do_parse!(token:alt_complete!(call_m!(self.lex_operators) | 
    call_m!(self.lexer_assign) |call_m!(self.lex_punctuations) |call_m!(self.lex_string) |call_m!(self.lex_number)|call_m!(self.lex_identifier) |
    call_m!(self.lex_illegal)) >> (self.token_with_info(token)))) ;
    method!(pub lex_tokens<Lexer<'a>>(&[u8]) -> Vec<Token>,mut self, ws!(many0!(call_m!(self.lex_token))));

}

named!(id,take_while!(is_alphanumeric_or_underscore));


fn is_alphanumeric_or_underscore(c: u8) -> bool {
    (c as char).is_alphanumeric() || (c as char) == '_'
}

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
        _ => TokenType::IDENTIFIER(String::from(id)),
    }
}


named!(
    int<TokenType>,
    do_parse!(
        int:map_res!(map_res!(recognize!(digit),str::from_utf8),|s| i64::from_str_radix(s,10)) >> (TokenType::INT(int))
)
);

named!(
    float<TokenType>,
    do_parse!(
        ft:map_res!(map_res!(recognize!(
                alt_complete!(
                    delimited!(digit,tag!("."),opt!(complete!(digit))) |
                    delimited!(opt!(digit), tag!("."), digit))
            ),
            str::from_utf8), FromStr::from_str)
        >> (TokenType::FLOAT(ft))
    )
);



// ASSIGNMENT

named!(
    assign<TokenType>,
    do_parse!(tag!("=") >> (TokenType::ASSIGN))
);
named!(
    plus_assign<TokenType>,
    do_parse!(tag!("+=") >> (TokenType::PLUSASSIGN))
);
named!(
    minus_assign<TokenType>,
    do_parse!(tag!("-=") >> (TokenType::MINUSASSIGN))
);
named!(
    star_assign<TokenType>,
    do_parse!(tag!("*=") >> (TokenType::STARASSIGN))
);
named!(
    slash_assign<TokenType>,
    do_parse!(tag!("/=") >> (TokenType::SLASHASSIGN))
);


// Operator
named!(plus<TokenType>, do_parse!(tag!("+") >> (TokenType::PLUS)));
named!(minus<TokenType>, do_parse!(tag!("-") >> (TokenType::MINUS)));
named!(bang<TokenType>, do_parse!(tag!("!") >> (TokenType::BANG)));
named!(star<TokenType>, do_parse!(tag!("*") >> (TokenType::STAR)));
named!(slash<TokenType>, do_parse!(tag!("/") >> (TokenType::SLASH)));
named!(
    modulo<TokenType>,
    do_parse!(tag!("%") >> (TokenType::MODULO))
);
named!(
    exp<TokenType>,
    do_parse!(tag!("^") >> (TokenType::EXPONENTIAL))
);



// PUNCTUCATIon

named!(dot<TokenType>, do_parse!(tag!(".") >> (TokenType::DOT)));
named!(
    question<TokenType>,
    do_parse!(tag!("?") >> (TokenType::QUESTION))
);
named!(colon<TokenType>, do_parse!(tag!(":") >> (TokenType::COLON)));
named!(comma<TokenType>, do_parse!(tag!(",") >> (TokenType::COMMA)));
named!(
    comment<TokenType>,
    do_parse!(tag!("//") >> (TokenType::COMMENT))
);
named!(
    semicolon<TokenType>,
    do_parse!(tag!(";") >> (TokenType::SEMICOLON))
);
named!(
    lparen<TokenType>,
    do_parse!(tag!("(") >> (TokenType::LPAREN))
);
named!(
    rparen<TokenType>,
    do_parse!(tag!(")") >> (TokenType::RPAREN))
);
named!(
    lbracket<TokenType>,
    do_parse!(tag!("[") >> (TokenType::LBRACKET))
);
named!(
    rbracket<TokenType>,
    do_parse!(tag!("]") >> (TokenType::RBRACKET))
);
named!(
    lbrace<TokenType>,
    do_parse!(tag!("{") >> (TokenType::LBRACE))
);
named!(
    rbrace<TokenType>,
    do_parse!(tag!("}") >> (TokenType::RBRACE))
);


// Comparison


named!(
    less_than<TokenType>,
    do_parse!(tag!("<") >> (TokenType::LESSTHAN))
);
named!(
    greater_than<TokenType>,
    do_parse!(tag!(">") >> (TokenType::GREATERTHAN))
);
named!(
    equal_equal<TokenType>,
    do_parse!(tag!("==") >> (TokenType::EQUALEQUAL))
);
named!(
    bang_equal<TokenType>,
    do_parse!(tag!("!=") >> (TokenType::BANGEQUAL))
);
named!(
    less_than_equal<TokenType>,
    do_parse!(tag!("<=") >> (TokenType::LESSTHANEQUAL))
);
named!(
    greater_than_equal<TokenType>,
    do_parse!(tag!("=>") >> (TokenType::GREATERTHANEQUAL))
);






named!(
    string<TokenType>,
    do_parse!(
        string:delimited!(
            tag!("\""),
            map!(map_res!(escaped!(call!(alphanumeric), '\\', is_a!("\"n\\")), str::from_utf8),String::from),
            tag!("\"")) 
            >> (TokenType::STRING(string))
        )
);
