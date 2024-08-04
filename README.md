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

To reproduce the result the paper currently submitted to IFL, run

```bash
pun --check examples/ifl1.pun
pun --check examples/ifl2.pun
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
