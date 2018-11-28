## Grammar
[![](https://tokei.rs/b1/github/lapz/lexer)](https://github.com/lapz/tox)

[![Build Status](https://travis-ci.org/Lapz/lexer.svg?branch=master)](https://travis-ci.org/Lapz/tox)
# TOX

Tox is a statically typed version of [lox](http://www.craftinginterpreters.com) that is written in rust.

# Example Program

```ts 
fn fib(n:int) -> int {
    if (n < 2) 
      return n;
    return fib(n - 2) + fib(n - 1);
}
```

A simple example that makes of uses of the classes

```rust
class Toggle {
    state:bool;
  
    fn value() -> bool {
      return this.state;
    }
  
    fn activate() -> Toggle {
      this.state = !this.state;
      return this;
    }
}
  
fn main() {
  var toggle  = Toggle{state:true};

  print toggle.activate().value();

  print toggle.activate().value();
}
```

