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