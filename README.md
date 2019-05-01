## Grammar
[![](https://tokei.rs/b1/github/lapz/tox)](https://github.com/lapz/tox)

[![Build Status](https://travis-ci.org/Lapz/tox.svg?branch=master)](https://travis-ci.org/Lapz/tox)
# TOX

Tox is a statically typed version of [lox](http://www.craftinginterpreters.com) that is written in rust.


# Usage

```
USAGE:
    tox [FLAGS] [source]

FLAGS:
    -h, --help         Prints help information
    -i, --interpter    Run in interpreter mode
    -V, --version      Prints version information

ARGS:
    <source>    The source code file
```


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


# TODO

- [ ] Implement exhaustive pattern matching
- [ ] Implement monomorphistation
- [ ] Implement the gc 
- [ ] Add constant folding



# Resources
* [rust](https://github.com/rust-lang/rust)
* [plank](https://github.com/jDomantas/plank)
* [lox](http://www.craftinginterpreters.com)
* [menhir-lang](https://github.com/GeorgeKT/menhir-lang)
* [minicom](https://github.com/agatan/minicom)
* [tiger-rs](https://github.com/antoyo/tiger-rs)
* [Kaleidoscope](https://llvm.org/docs/tutorial/index.html)
* [kaleidoscope-rs](https://github.com/BookOwl/kaleidoscope-rs)
* [inko](https://gitlab.com/yorickpeterse/inko)
* [NovaLang](https://github.com/boomshroom/NovaLang)
* [gluon](https://github.com/gluon-lang/gluon)
* [dora](https://github.com/dinfuehr/dora)
* Modern Compiler Implementation in [ML](http://www.cs.princeton.edu/~appel/modern/ml/), [java](http://www.cs.princeton.edu/~appel/modern/java/) and [C](https://www.cs.princeton.edu/~appel/modern/c/)
 * [Developing Statically Typed Programming Language](http://blog.mgechev.com/2017/08/05/typed-lambda-calculus-create-type-checker-transpiler-compiler-javascript/)
* [/r/ProgrammingLanguages](https://www.reddit.com/r/ProgrammingLanguages/)
* [awesome-compilers](https://github.com/aalhour/awesome-compilers)
