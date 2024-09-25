# gemau

The first seed of what will hopefully be a computer algebra system for CGT.

The current goal of this crate is to provide first-class support for partizan
misère research. At present, it contains only functionality for studying Left
dead ends, but this will soon increase. In the long term, this crate will also
aim to serve more traditional use cases, like normal play.

## Examples

```rust
use gemau::LeftDeadEnd;

let g = LeftDeadEnd::waiting(4);
let h = g + 2;

// factors are 0, 1, 2, W_4, 1 + W_4, 2 + W_4
assert_eq!(h.factors().len(), 6);

let k = LeftDeadEnd::with_options(3..7);
assert_eq!(k.flex(), 1);
assert_eq!(k.race(), 4);
assert_eq!(k.birth(), 7);

let j = LeftDeadEnd::waiting(3);
let k = k + j;
assert_eq!(k.flex(), 9);
assert_eq!(k.race(), 5);
assert_eq!(k.birth(), 10);
```

If you want the crate to be finished quicker, then you could consider
contributing. :)

## Similiar projects

Given the current emptiness of this project, you should take a look at the
following:

- [CGSuite](https://www.cgsuite.org/) if you need to get things done.
- [cgt](https://crates.io/crates/cgt) if you like your games played rusty.
- [haskell-cgt](https://github.com/kamekura/haskell-cgt) for the Haskellians.

## License

This project is released under [The Unlicense](https://unlicense.org/),
dedicated to the public domain.

## Contributing

Contributions welcome! :)

By submitting a pull request or otherwise contributing to this project, you
agree to dedicate your contribution to the public domain under the terms of
[The Unlicense](https://unlicense.org/), and you certify that you have the
right to do so.
