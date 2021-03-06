## Reproduce my Development Environment, Reproduce my Results!

The primary entry-points for working with this apparatus are the respective
`run-*.sh` shell scripts. These employ [Docker](https://www.docker.com) to run
the [Haskell](run-haskell.sh), or [Idris](run-idris.sh) apparatus,
respectively.  This bears the benefits of (1) a minimal-effort install, and (2)
a reproducible development environment.

To get started, install Docker, and run the respective shell script to start up
a Docker image with the respective tools (GHC and/or Idris) installed.

Currently, the [Haskell implementation](haskell) is (much) outdated. To play
with the [Idris implementation](idris):

```
$ ./run-idris.sh
~/mechanics/idris $ make && ./Main.bin
```

## Grammar

```
Expr := Number
      | VarName
      | Expr `+` Expr
      | Expr `-` Expr
      | Expr `*` Expr
      | Expr `/` Expr
      | `let` VarName `=` Expr `in` Expr
      | `if` Expr `then` Expr `else` Expr
      | `(` Expr `)`
      | `<` Expr `,` Expr `>`
      | FunName Args

Args := `(` ArgList `)`

ArgList :=
         | NonEmptyArgList

NonEmptyArgList := Expr
                 | Expr `,` NonEmptyArgList
```

### Built-in Functions

The built-in functions are `fst`, `snd`, `dup`, and `diff`. `diff` takes a
variable name and an expression, and yields the expression differentiated with
respect to the given variable name. The other built-in functions work as
follows:

```
> fst(<x,y>)
Just x
> snd(<x,y>)
Just y
> dup(x)
Just < x, x >
```

### Precedence and Associativity

* `let` and `if` have the lowest precedence.
* `+` and `-` are left-associative and have higher precedence than `let` and `if`.
* `*` and `/` are left-associative and have higher precedence than `+` and `-`.

As usual, parentheses can be used to override precedence and associativity
rules.

### Whitespace Rules

* `let` and `if` are followed by at least one whitespace character.
* `in`, `then`, and `else` are surrounded by at least one whitespace character.
* All other tokens are separated by arbitrary whitespace.

## Derivatives

### `let`s

```
Judgement: d/dx e1 ~> e2, where x \in VarName and e1, e2 \in Expr.

Rules:

    d/dx e2[e1/y] ~> e3
--------------------------- (if x /= y)
d/dx let y = e1 in e2 ~> e3

    d/dy e2[e1/x] ~> e3
--------------------------- (where y is fresh)
d/dx let x = e1 in e2 ~> e3

```

### `if`s

`if`s are differentiation according to the following principles:

1. If the derivatives of the branches unify, then the derivative of a branch is
the unified derivative.

2. Otherwise, the derivative of a branch is a branching derivative, with the
original condition.

Unification, for now, is trivial — it is mere equality.

## Gotcha's

### The Idris Interpreter is Lazy

The Idris interpreter is lazy, while the Idris run-time is eager, unless
specified otherwise. This makes the interpreter useless for the shotgun testing
of run-time behaviour. Things that terminate in the interpreter, might not
terminate at run-time.
