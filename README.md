## Grammar
Statements:
  
    program      → declaration* EOF ;
    declaration  → varDecl
                | statement ;
    varDecl      → "var" IDENTIFIER ( "=", "+=" , "*=" , "-=", expression )? ";" ;
    statement    → exprStmt
                | forStmt
                | ifStmt
                | printStmt
                | whileStmt
                | block;
    exprStmt     → expression ";" ;
    forStmt      → "for" "(" ( varDecl | exprStmt | ";" )
                            expression? ";"
                            expression? ")" statement ;
    ifStmt       → "if" "(" expression ")" statement ( "else" statement )? ;
    printStmt    → "print" expression ";" ;
    doWhileStmt  → "do" statement "while" "(" expression ")" ;
    whileStmt    → "while" "(" expression ")" statement ;
    breakStmt    → "break" ";" ;
    continueStmt → "continue" ";" ;
    block        → "{" declaration* "}" ;
    list         → "[" (expression,",")* "]" ;
    dict         → "{" (expression ":" expression)* "}"  ;

Expressions:


    expression  → assignment ;
    assignment  → identifier ( "=" ) assignment )?
                | ternary ;
    ternary     → logic_or ( "?" expression ":" ternary )?
    logic_or    → logic_and ( "or" logic_and )*
    logic_and   → equality ( "and" equality )*
    equality    → comparison ( ( "!=" | "==" ) comparison )*
    comparison  → term ( ( ">" | ">=" | "<" | "<=" ) term )*
    term        → factor ( ( "-" | "+" ) factor )*
    factor      → unary  ( "/" | "*" | "%" | "^") unary )
    unary       → ( "!" | "-" ) unary | call ;
    call        → primary ( "(" arguments? ")" )* ;
    arguments   → expression ( "," expression )* ;
    primary     → NUMBER | STRING | "true" | "false" | "nil"
                | IDENTIFIER
    factor      → exponent ( ( "/" | "*" | "%" ) exponent )
    exponent    → unary ( ( "^" ) ) unary , 
    unary       → ( "!" | "-" ) unary | call ;
    call        → primary ( "(" arguments? ")" )* | primary ( "[" expression "]" ) ;
    arguments   → expression ( "," expression )* ;
    primary     → NUMBER | STRING | "true" | "false" | "nil"
                | IDENTIFIER | dict |list | ("expression")
   
lexical:


    NUMBER       → DIGIT+ ( "." DIGIT* )? | "." DIGIT+ ;
    STRING       → '"' <any char except '"'>* '"' ;
    IDENTIFIER   → ALPHA ( ALPHA | DIGIT )* ;
    ALPHA        → 'a' ... 'z' | 'A' ... 'Z' | '_' ;
    DIGIT        → '0' ... '9' ;
    