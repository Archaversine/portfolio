---
title: Building a Typechecker in Prolog
author: Adam Brohl
---

Many programs employ a static type system. This means that instead of checking 
at runtime if an arbitrary operation is valid on some values, values are given
a type before the program is run, and a typechecker will ensure that there does 
not exist any place in the program where a function acts on the wrong types of 
arguments.

Typecheckers are written in all sorts of languages, such as C/C++, Rust,
Python, etc. This post explores how a type checker could be represented in
Prolog. Prolog is a logical oriented programming language where instead of
defining a list of instructions (imperative) or a transformation from some
input to an output (functional), a program is created by listing a set of facts
and relations between those facts. 

## Basic Prolog Syntax

### Declaring Facts

In Prolog, facts are typically written in the form `factname(object)` and end
with a `.` at the end. Both the fact name and the object must start with 
a lowercase letter.

```prolog
parent(alice, bob).
parent(bob, david).
parent(carol, david).

child(david).
```

In the above program, the following facts are declared:

- `alice` is the parent of `bob`
- `bob` is the parent of `david`
- `carol` is the parent of `david`
- `david` is a child

_NOTE: `parent(alice, bob)` doesn't necessarily have to mean `alice` is the
parent of `bob`, it could mean that `bob` is the parent of `alice`; it is up to
the programmer to decide the correct meaning._

### Declaring Rules

Ini addition to the example above, we can also define a set of logical rules:

```prolog 
grandparent(A, B) :-
    parent(A, X),
    parent(X, B).
```

The `A :- B` means `A` is implied by `B`, and the comma (`,`) operator
represents or. The above code can be interpreted as: "`A` is a grandparent of 
`B` _if_ `A` is a parent of some object `X`, and that `X` is a parent of `B`".
The capital letters signify variables instead of actual atomic values.

### Querying

