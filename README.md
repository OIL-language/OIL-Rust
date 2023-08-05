# Obvious-imperative-language
OIL is an imperative programming language designed for simplicity, elegance and shorter debugging time, as whatever is the most obvious thing to do should work by default.
## Syntax (Theoretical)
```rust
import std;

use std.io;

fn main() {
    Range.new(0, 100)
         .each(fn (let i: U32) if i % 3 == 0 && i % 5 == 0 {
             io.println("Fizzbuzz");
         } else if i % 3 == 0 {
             io.println("Fizz");
         } else if i % 5 == 0 {
             io.println("Buzz");
         } else {
             io.println(i);
         })
}
```
