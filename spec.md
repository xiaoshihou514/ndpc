# :toolbox:Natural Deduction Proof Compiler

## About the project

This project aims to take a `.ndp` file (natural deduction proof file), written in a DSL we have defined, check its validity, give reasonable diagnostics, and output a pretty printed version of the proof.

Here are some examples of the syntax:

```
forall n.(~even(n) -> odd (n)) [premise]
forall n.(~odd (n) -> even(n)) [premise]
  c [forall I const] -- example comment
  ~even(c) -> odd (c) [forall E(1)]
  ~odd (c) -> even(c) [some reson]
  odd (c) / ~odd (c)
    -- or elimination, these should be the only keywords in our DSL
    either:
    odd (c)               [asm]
    even(c) / odd (c)     [/I(...)]

    or:
    ~odd (c)              [asm]
    even(c)               [forall. E(3)]
    even(c) / odd (c)     [/I(10)]

  even(c) / odd (c)       [even(c) / odd (c)]

forall n.(even(n) / odd (n)) [forall I(3 − 12)]
```

```
forall x. (forall y. (child(y, x) -> fly(y)) ^ dragon(x) -> happy(x)) [premise]
forall x. (green(x) ^ dragon(x) -> fly(x)) [premise]
forall x. (exists y. (parent(y, x) ^ green(y)) -> green(x)) [premise]
forall z x. (child (x, z) ^ dragon(z) -> dragon(x)) [premise]
forall x y. (child (y, x) -> parent(x, y)) [premise]

  c [forall I const]
    dragon(c) [ass]
      green(c) [ass]
         d [forall I const]
         child(d, c) [ass]
         child (y, x) -> parent(x, y) [forall E(5)]
         parent(c, d) [->E(11,10)]
         parent(c, d) ^ green(c) [^I(8,11)]
         -- <hard struggle>
         fly(d) [forall->E(17,2)]
        -- some conclusion
      -- some more conclusion
    -- and more...
  dragon(c) → (green(c) → happy(c)) [???]
forall x. (dragon(x) → (green(x) → happy(x))) [forall I(6,24)]
```

The pretty printed version uses unicode ∀, ∃ and has nice boxes indicating the scope.

The compiler should implement the following functionalities:

```
First logic compiler
Usage: ndpc [OPTION] [FILE]...

Arguments:
[FILE]... input files, use - for stdin

Options:
check check proof validity
fmt format proof file
<default> generate pretty printed boxes
```

To facilitate using this toy compiler, we will also provide a cross platform gui, as well as a tutorial on how to write proofs in our determined format.

## The compiler

### Lexing

It's possible to do lexing within the parser, but since we are implementing a formatting module, this will come in handy afterwards. This module should have the methods that would remove comments and trim redundant spaces.

### Parsing

We should parse each line independently, wrapped in a scope object.

### Checking

Things we need to check:

- Are there any free variables?
- Is every proof valid according to the rules?

### Assembling

This should just be pretty printing to a file.

## The GUI

Plan?: Rust + [iced](https://iced.rs/), should live in another repo
