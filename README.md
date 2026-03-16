<div align="center">

# Ndpc

<img src="https://github.com/user-attachments/assets/a6a3fd99-1a96-40c5-805b-235e43202112" alt="logo" width="30%" />

Proof assistant for single sorted predicate logic.

[Getting started](https://xiaoshihou514.github.io/ndpc/getting-started.html) •
[Tutorial](https://xiaoshihou514.github.io/ndpc/tutorial.html) •
[Reference](https://xiaoshihou514.github.io/ndpc/syntax.html)

</div>

[中文文档](./README-zh.md)

Ndpc enables correct, maintainable and formally verified proofs for single sorted predicate logic, whose style follows closely with "hand written" proofs. It can:

- Proof checking
- Generate corresponding Lean4 proofs
- Export to HTML, Latex and Typst

## Monorepo layout

- `core/`: shared parser, checker, formatter, and proof model code
- `cli/`: command-line entrypoint plus code generators and release packaging
- `web/`: Laminar + Scala.js scaffold wired to the shared core

## Build and test

You need [sbt](https://www.scala-sbt.org/) and a JDK installed.

```bash
sbt coreJVM/test
sbt cli/test
sbt web/fastLinkJS
sbt "cli/run -- check example.ndp"
```

## Release builds

Build the runnable jar:

```bash
sbt cli/assembly
```

This writes `ndpc.jar` at the repository root.

Build a native executable with GraalVM `native-image`:

```bash
sbt cli/graalNativeImage
```

This writes `ndpc-graal` at the repository root.

Build a native executable with Scala Native:

```bash
sbt cliNative/rootNativeLink
```

This writes `ndpc-native` at the repository root.

## Getting started

Go to our [getting started page](https://xiaoshihou514.github.io/ndpc/getting-started.html) for details about installation and basic usage.

An online tutorial is available [here](https://xiaoshihou514.github.io/ndpc/tutorial.html). There is also a [language reference](https://xiaoshihou514.github.io/ndpc/syntax.html).

## Troubleshooting

Use [github issues](https://github.com/xiaoshihou514/ndpc/issues/new) or [github discussions](https://github.com/xiaoshihou514/ndpc/discussions).

## Related projects

- [ndp.vim](https://github.com/xiaoshihou514/ndp.vim): (Neo)Vim support for ndp files
- [aristotle](https://github.com/xiaoshihou514/aristotle): GUI frontend for ndpc
- [boxproof](https://github.com/YunkaiZhang233/boxproof): Supporting library for Latex backend
- [boxproof-typst](https://github.com/xiaoshihou514/boxproof-typst): Supporting library for Typst backend
