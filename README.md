## Grammar
[![](https://tokei.rs/b1/github/lapz/lexer)](https://github.com/lapz/tox)

[![Build Status](https://travis-ci.org/Lapz/lexer.svg?branch=master)](https://travis-ci.org/Lapz/tox)
# TOX

Tox is a statically typed version of [lox](http://www.craftinginterpreters.com) that is written in rust.

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


while (true) {
  var secret_number = random(1,101);
  print "Please input your guess.";

  var guess = io.readline();

  print "You guessed: " + guess;

  if (to_int(guess) > secret_number) {
    print "Too small!";
  }

  if (to_int(guess) < secret_number) {
    print "Too big!";
  }


  if (to_int(guess) == secret_number) {
    print "You win!";
    break;
  }
}

```

#TODO

* ~~Parse field get property and field get call differently;~~
* ~~Implement `and` or `or`~~
* Implement classes
* Implement SETPARAM

