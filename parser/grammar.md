# Tox Grammar

## Comments

```text
comment : block_comment |line_comment ;
block_comment : "/*" block_comment_body * "*/" ;
block_comment_body : [block_comment | character] * ;
line_comment : "//" * ;
```

## Program

```text
program: declaration* EOF;
declaration: fnDef | classDef | typeAliasDef | enumDef
fnDef: (visibility)? "fn" ident  (type_params)? (func_params)? (return_type)? block ;
classDef: (visibility)? "class" ident  (type_params)? class_body;
typeAliasDef: (visibility)? "type" ident (type_params)? "=" type ";" ;
enumDef: (visibility)? "enum" ident (type_params)? enum_variants ;
```

## Visibility

```text
visibility:("export") ?;
```

## Type Params

```text
type_params: "<" (type| type_param) ("," type| type_param)*   ">";
type_param: ident ;
```

## Pattern

```text
pattern: tuple_pattern | binding_pattern | placeholder_pattern | literal_pattern ;

tuple_pattern: "(" pattern ((",")? pattern)* ")" ;
placeholder_pattern: "_";
literal_pattern: literal;
binding_pattern: ident;
```

## Type

```text
type: array_type | paren_type | fn_type | ident | "void" | "self"
array_type: "[" type "]";
paren_type:"(" type ((",")? type)* ")";
fn_type: "fn" "(" type ((",")? type)* ")" "->" type;
```

## Functions

### Func Params

```text
func_params: "(" ((",")? func_param)* ")"
func_param: pattern ":" type ;
```

### Return Type

```text
return_type: "->" type;
```

## Enums

### Enum Variants

```text
enum_variants: "{" ((",")? enum_variant)* "}"
enum_variant: ident ("(" (",")? type ")")? ;
```
 
## Classes

### Class Body

```text
class_body: "{" named_field* | fnDef* "}"
named_field: ident ":" type ";" ;
```
