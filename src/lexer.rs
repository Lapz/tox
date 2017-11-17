use token::{Token, TokenType};
use nom::{alphanumeric,digit,is_alphanumeric,IResult};
use std::str::{self, FromStr};


named!(lex_identifier<TokenType>,
do_parse!(
    name:map_res!(call!(id),str::from_utf8) >>
    (look_up_identifier(name))
    )
);

named!(id,take_while!(is_alphanumeric_or_underscore));

fn is_alphanumeric_or_underscore(c:u8) -> bool {
    (c as char).is_alphanumeric() || (c as char) == '_'
}

fn look_up_identifier(id:&str) -> TokenType {
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



named!(string<TokenType>,do_parse!(
    string:delimited!(
    tag!("\""),
    map!(map_res!(escaped!(call!(alphanumeric), '\\', is_a!("\"n\\")), str::from_utf8),String::from),
    tag!("\"")
) >> (TokenType::STRING(string))

));

named!(lex_string<TokenType>,call!(string));

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

named!(lex_number<TokenType>,alt!(float|int));


named!(assign<TokenType>,do_parse!(tag!("=") >> (TokenType::ASSIGN)));
named!(plus_assign<TokenType>,do_parse!(tag!("+=") >> (TokenType::PLUSASSIGN)));
named!(minus_assign<TokenType>,do_parse!(tag!("-=") >> (TokenType::MINUSASSIGN)));
named!(star_assign<TokenType>,do_parse!(tag!("*=") >> (TokenType::STARASSIGN)));
named!(slash_assign<TokenType>,do_parse!(tag!("/=") >> (TokenType::SLASHASSIGN)));

named!(lexer_assign<TokenType>, alt!(
    assign|
    minus_assign |
    star_assign |
    slash_assign 
));

named!(plus<TokenType>,do_parse!(tag!("+") >> (TokenType::PLUS)));
named!(minus<TokenType>,do_parse!(tag!("-") >> (TokenType::MINUS)));
named!(bang<TokenType>,do_parse!(tag!("!") >> (TokenType::BANG)));
named!(star<TokenType>,do_parse!(tag!("*") >> (TokenType::STAR)));
named!(slash<TokenType>,do_parse!(tag!("/") >> (TokenType::SLASH)));
named!(modulo<TokenType>,do_parse!(tag!("%") >> (TokenType::MODULO)));
named!(exp<TokenType>,do_parse!(tag!("^") >> (TokenType::EXPONENTIAL)));


named!(lex_operators<TokenType>,alt!(
    plus|
    minus|
    bang|
    star|
    slash|
    modulo|
    exp
));

named!(dot<TokenType>,do_parse!(tag!(".") >> (TokenType::DOT)));
named!(question<TokenType>,do_parse!(tag!("?") >> (TokenType::QUESTION)));
named!(colon<TokenType>,do_parse!(tag!(":") >> (TokenType::COLON)));
named!(comma<TokenType>,do_parse!(tag!(",") >> (TokenType::COMMA)));
named!(comment<TokenType>,do_parse!(tag!("//") >> (TokenType::COMMENT)));
named!(semicolon<TokenType>,do_parse!(tag!(";") >> (TokenType::SEMICOLON)));
named!(lparen<TokenType>,do_parse!(tag!("(") >> (TokenType::LPAREN)));
named!(rparen<TokenType>,do_parse!(tag!(")") >> (TokenType::RPAREN)));
named!(lbracket<TokenType>,do_parse!(tag!("[") >> (TokenType::LBRACKET)));
named!(rbracket<TokenType>,do_parse!(tag!("]") >> (TokenType::RBRACKET)));
named!(lbrace<TokenType>,do_parse!(tag!("{") >> (TokenType::LBRACE)));
named!(rbrace<TokenType>,do_parse!(tag!("}") >> (TokenType::RBRACE)));

named!(lex_punctiation<TokenType>,alt!(
    dot|
    question|
    colon|
    comma|
    comment|
    semicolon |
    lparen |
    rparen |
    lbracket |
    rbracket |
    lbrace |
    rbrace 
));

named!(less_than<TokenType>,do_parse!(tag!("<") >> (TokenType::LESSTHAN)));
named!(greater_than<TokenType>,do_parse!(tag!(">") >> (TokenType::GREATERTHAN)));
named!(equal_equal<TokenType>,do_parse!(tag!("==") >> (TokenType::EQUALEQUAL)));
named!(bang_equal<TokenType>,do_parse!(tag!("!=") >> (TokenType::BANGEQUAL)));
named!(less_than_equal<TokenType>,do_parse!(tag!("<=") >> (TokenType::LESSTHANEQUAL)));
named!(greater_than_equal<TokenType>,do_parse!(tag!("=>") >> (TokenType::GREATERTHANEQUAL)));

named!(lex_comparison<TokenType>,alt!(
    less_than |
    greater_than |
    equal_equal |
    bang_equal |
    less_than_equal |
    greater_than_equal 
));

named!(lex_illegal<TokenType>,do_parse!(take!(1)>> (TokenType::ILLEGAL)));

named!(lex_token<TokenType>, do_parse!(token:alt_complete!(
    lex_operators |
    lexer_assign|
    lex_punctiation |
    lex_string |
    lex_number|
    lex_identifier |
    lex_illegal
) >>postion

));

named!(lex_tokens<Vec<TokenType>>,
    ws!(many0!(lex_token))

);

pub fn int_test() {
    let test = "var five = [1,2,3.5];"
.as_bytes();
    println!("{:?}", lex_tokens(&test[..]));
}
