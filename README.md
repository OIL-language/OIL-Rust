# Obvious-imperative-language
OIL is an imperative programming language designed for simplicity, elegance and shorter debugging time, as whatever is the most obvious thing to do should work by default.
## Syntax (Theoretical)
```rust
import std;

use std.io.STDOUT;

fn main() {
    Range.new(0, 100)
        .each(fn (let i: U32) {
            let any_conditions: Bool = false;

            [("Fizz", 3), ("Buzz", 5), ("Foo", 2), ("Bar", 13), ("Baz", 19)]
                .to_iter()
                .each(fn (let (name, multiple): (String, U64)) {
                    if i % multiple == 0 {
                        any_conditions = true;
                        STDOUT.print(i);
                    }
                });

            if !any_conditions {
                STDOUT.print(i);
            }

            STDOUT.println();
        });
};
```
