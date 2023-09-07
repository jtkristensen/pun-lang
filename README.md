# The `pun` Programming Language,The `fun` Language with Properties.

[![tests](https://github.com/jtkristensen/pun-lang/actions/workflows/main-test.yaml/badge.svg)](https://github.com/jtkristensen/pun-lang/actions/workflows/main-test.yaml)
[![hlint](https://github.com/jtkristensen/pun-lang/actions/workflows/main-hlint.yaml/badge.svg)](https://github.com/jtkristensen/pun-lang/actions/workflows/main-hlint.yaml)

Pun is an extension of a minimal garden variety functional programming
langauge (similar to the Fun langauge from Pierce's "Types and Progamming
Languages", and the REC language from Glynn Winskel's "The Formal Semantics
of Programming Langauges").

The main feature is a `property` declaration, which has the form:

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
