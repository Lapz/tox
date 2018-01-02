# Todo
- [x] Add with position to the ~~variables~~ and expressions themselves
- [x] Add the parsing of types
- [x] Add the resolution of function
- [ ] Add a reporter which reportes errors
- [x] Add the infernece of function method calling
- [x] Add an expression that allows for the construction of a class. e.g
    ```rust
    class Person {
        name:str,surname:str;
    }
    ```
- [x] Improve type inference messages
- [x] Implement the interpreter
- [x] Add Exponential to the parser
- [x] Improve the inference of comparison operators
- [] Move the functions in inference onto a struct called ```TyChecker```
- [] Have a field on the type check that contains the types that this points too. When this is acessed check if that field exist and the return the type of that field.