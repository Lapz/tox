## Grammar
[![](https://tokei.rs/b1/github/lapz/lexer)](https://github.com/lapz/tox)

[![Build Status](https://travis-ci.org/Lapz/lexer.svg?branch=master)](https://travis-ci.org/Lapz/tox)
# TOX

LLROX is a statically typed version of [lox](http://www.craftinginterpreters.com) that is written in rust.

# Example Program

```ts 
fun fib(n:int) -> int {
    if (n < 2) 
      return n;
    return fib(n - 2) + fib(n - 1);
}
```

A simple example that makes of uses of the classes

```ts
class Toggle {
    state:bool;
  
    fun value() -> bool {
      return this.state;
    }
  
    fun activate() -> Toggle {
      this.state = !this.state;
      return this;
    }
}
  
var toggle  = Toggle{state:true};

print toggle.activate().value();

print toggle.activate().value();
```

A guessing game example 

```ts

print "Guess the number!";


var secret_number = rand(1,101);
print "Please input your guess.";

var guess = io.readline();

print "You guessed: " + guess;

if (int(guess) > secret_number) {
  print "Too small!";
}

if (int(guess) < secret_number) {
  print "Too big!";
}

if (int(guess) == secret_number) {
  print "You win!";
}

```


# TODO

- [x] Add with position to the ~~variables~~ and expressions themselves
- [x] Add the parsing of types
- [x] Add the resolution of function
- ~~[ ] Add a reporter which reportes errors~~
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
- [x] Move the functions in inference onto a struct called ```TyChecker```
- [x] Have a field on the type check that contains the types that this points too. When this is acessed check if that field exist and the return the type of that field.
- [ ] Change how method inheritance is done
- [ ] Implement closures properly


## Run the Compiler
 RUST_BACKTRACE=1 cargo run ../test/test.lox && llc -filetype=obj out.bc \
&& clang++ -g -O3 out.bc `lvm-config --cxxflags --ldflags --system-libs --libs core` && ./toy