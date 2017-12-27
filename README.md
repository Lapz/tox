## Grammar
[![](https://tokei.rs/b1/github/lapz/lexer)](https://github.com/lapz/lexer)

[![Build Status](https://travis-ci.org/Lapz/lexer.svg?branch=master)](https://travis-ci.org/Lapz/lexer)

Statements:
  
    program      → declaration* EOF ;
    declaration  → varDecl
                 |funDecl
                 | classDecl
                 | typeDecl;
                 | statement ;
    varDecl      → "var" IDENTIFIER ( ":" type) ? ( ("="| "+=" | "*=" | "-="), expression )? ";" ;
    funDecl      → "fun" function;
    typeDecl     → "type" IDENTIFIER "=" type;
    classDecl    → "class" IDENTIFIER 
                 "{" (IDENTFIER:type";")* function* "}"
    statement    → exprStmt
                 | forStmt
                 | ifStmt
                 | printStmt
                 | whileStmt
                 | block
                 | list
                 | dict ;
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
    type         → "bool" | "int" | "str" | "float" | "nil";

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
                | IDENTIFIER | ("expression")
   
lexical:


    NUMBER       → DIGIT+ ( "." DIGIT* )? | "." DIGIT+ ;
    STRING       → '"' <any char except '"'>* '"' ;
    IDENTIFIER   → ALPHA ( ALPHA | DIGIT )* ;
    ALPHA        → 'a' ... 'z' | 'A' ... 'Z' | '_' ;
    DIGIT        → '0' ... '9' ;

  
