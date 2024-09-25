<div align="center">

# Ndpc

ndpc is the compiler for the natural deduction proof markup language.

[Getting started](https://xiaoshihou514.github.io/ndpc/getting-started.html) •
[Tutorial](https://xiaoshihou514.github.io/ndpc/tutorial.html) •
[Cheatsheet](https://xiaoshihou514.github.io/ndpc/rules.html)

</div>

## What is ndpc?

Ndpc introduced a markup language for creating [natural deduction](https://wikipedia.org/wiki/Natural_deduction) proofs for propositional logic and classical first order logic, with support for validation, formatting, and exporting to HTML.

Compared to existing similar tools, ndpc is easier to use and looks much more similar to hand written proofs.

As an example:

```
-- This is a proof that mainly uses =sub
forall x. x = a / x = b                     [premise]
g(a) = b                                    [premise]
forall x. forall y. (g(x) = g(y) -> x = y)  [premise]
g(b) = a / g(b) = b                         [premise]
  -- indent to begin a new box!
  g(b) = a                                  [ass]
  g(g(a)) = a                               [=sub(5, 2)]
  g(g(a)) = a                               [tick(6)]

  g(b) = b                                  [ass]
  g(b) = g(a)                               [=sub(8,2)]
  forall y. (g(b) = g(y) -> b = y)          [forallE(3)]
  b = a                                     [forall->E(9,10)] -- you can't do it all in one go
  g(a) = a                                  [=sub(8,11)]
  g(g(a)) = a                               [=sub(12,12)]
g(g(a)) = a                                 [/E(4,5,7,8,13)]
```

## Getting started

Go to our [getting started page](https://xiaoshihou514.github.io/ndpc/getting-started.html) for details about installation and basic usage.

## Tutorial

An online tutorial is available [here](https://xiaoshihou514.github.io/ndpc/tutorial.html), which covers the syntax (there isn't that many).

## Troubleshooting

Oh no! Ndpc found errors in your perfectly fine proof! The [syntax gotcha](https://xiaoshihou514.github.io/ndpc/syntax-gotchas.html) and the [list of supported rules](https://xiaoshihou514.github.io/ndpc/rules.html) may help.

## Related projects

- [ndp.vim](https://github.com/xiaoshihou514/ndp.vim): (Neo)Vim support for ndp files
- [aristotle](https://github.com/xiaoshihou514/aristotle): GUI frontend for ndpc (WIP)
