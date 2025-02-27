# The `pun` Programming Language : A Fun Programming Language with Properties.

[![tests](https://github.com/jtkristensen/pun-lang/actions/workflows/main-test.yaml/badge.svg)](https://github.com/jtkristensen/pun-lang/actions/workflows/main-test.yaml)
[![hlint](https://github.com/jtkristensen/pun-lang/actions/workflows/main-hlint.yaml/badge.svg)](https://github.com/jtkristensen/pun-lang/actions/workflows/main-hlint.yaml)

Pun is a small functional programming langauge (similar to the Fun langauge
from Pierce's "Types and Progamming Languages", and the REC language from
Glynn Winskel's "The Formal Semantics of Programming Langauges").

The main feature is a `property` declaration, which has the syntactic form:

```haskell
propery <name> <parameters> . <term> .
```

that lets the programmer specify properties to be checked by QuickCheck,
only that rather than having to write a generator by hand, the `pun`
interpreter will generate terms to substitute in for the parameters.

Example, here is a property.

```haskell
property plus-is-commutative m n . m + n = n + m .
```

Which will check that the term `m + n = n + m` normalizes to `true` for a
suitable handful of choices of `m` and `n` that make the term well-typed.

# Installation

Assuming that you have the haskell build-tool `stack` installed.
You should be able to install `pun` by running

```bash
> stack install
```

from within the repository.

# Artifact

To reproduce the results the paper currently submitted to RC 2025, first install `pun` and use the `--check` flag to check all the properties in a given file.

For the smaller examples inlined in the text, run

```bash
pun --check examples/rc1.pun
pun --check examples/rc2.pun
```

For the benchmark, demonstrating pun on the binary search tree properties defined in Hughes's *How to Specify It!* (2019), run

```bash
pun --check benchmark/BST.pun
```

# Getting started.

To typecheck a a program, run:

```bash
> pun --types <program-name>.pun
```

and start typing in terms to evaluate.

To run the properties specified in a program, pass the flag `--check` like so:

```bash
> pun --check <program-name>.pun
```

the folder `./examples` contains a number of example programs to get you started.
