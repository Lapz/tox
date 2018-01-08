## Grammar
[![](https://tokei.rs/b1/github/lapz/lexer)](https://github.com/lapz/lexer)

[![Build Status](https://travis-ci.org/Lapz/lexer.svg?branch=master)](https://travis-ci.org/Lapz/lexer)
# LLROX

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