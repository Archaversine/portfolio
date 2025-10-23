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

Once a set of facts and rules (a knowledgebase) has been constructed, queries
can be made to the knowledge base to determine if certain facts are true or not.
In the previous parent and grandparent example, we can make specific queries
to see if certain people are parents or grandparents.

```prolog 
?- parent(alice, bob)
true

?- parent(bob, alice)
false

?- grandparent(alice, bob)
false

?- grandparent(alice, david)
```

On top of this, queries can contain variables. This is useful for searching the 
knowledge base for facts that satisfy the query. For example, to find the 
grandparent of david, the following query can be made:

```prolog 
?- grandparent(A, david)
A = alice
```

If there are multiple things that satisfy a query, Prolog will list all of them
until either no more are left or specified to stop by the user.

## Designing a Type System

In this post, I will walk through how Prolog can be used to implement a type 
checking algorithm for a type system that has the following features:

- Basic types (Int, String, etc.)
- First class functions
- First class types
- Parametric polymorphism
- Ad-hoc polymorphism
- Generic Types (List, Vector, etc.)
- Dependent Types (Pi & Sigma)

## Implementing

### Type Facts

In order to implement a type system, we need to have some way to specify what 
the type of an arbitrary expression is. If this was part of a compiler, these
facts would be generated automatically depending on the semantics of the language.

Similar to the parent and grandparent facts defined in the previous examples, 
we can make a `type` rule where `type(A, Ty)` means "The type of `A` is `Ty`":

```prolog 
type(a, int).
type(b, float).
type(c, string).
```

The above code can be read as:
- The type of `a` is `int`
- The type of `b` is `float`
- The type of `c` is `string`

Some types, such as integers can be defined as literals like `123` or
`"some text"`. Prolog comes built in with some predicates to make defining 
type checking rules for these literals easy:

```prolog
type(X, int) :-
    integer(X).

type(X, float) :- 
    float(X).

type(X, string) :-
    string(X).
```

Then we can make the following queries to ask Prolog what the types of our 
literals would be:

```prolog 
?- type(3, Ty)
Ty = int

?- type(3.14, Ty)
Ty = float

?- type("Hello, World!", Ty)
Ty = string

```

#### Typechecking

Along with declaring type facts, we can also specify a rule that returns true
if an expression typechecks:

```prolog
typecheck(A) :-
    type(A, _).
```

The `typecheck` rule simply checks if `A` has some type `_`. The underscore is
used to denote a variable whose value we don't care about. Since we are only
checking *if* such a value exists and not what that value actually is, it can
be ignored.

Using the following knowledge base:

```prolog
type(a, int).
```

The `typecheck` rule behaves as expected:

```prolog
?- typecheck(a)
true

?- typecheck(b)
false
```

### Type Unification

Another feature of typecheckers is the ability to tell if two expressions have
the same type. This is useful in, for example, checking if the left and right 
side of a binary operator, like addition, are the same type. Prolog implements
its own unification algorithm for searching a knowledgebase, so implementing
such an algorithm is trivial:

```prolog
unified(A, B) :-
    type(A, Ty),
    type(B, Ty).
```

In english, the above code snippet states that `A` and `B` are unified if the
type of `A` is `Ty`, *and* the type of `B` is *also* `Ty`.

An example of unification in action:

```prolog
type(a, int).
type(b, int).
type(c, float).

?- unified(a, b)
true

?- unified(a, c)
false
```

### Function Types

Most programming languages implement functions in some form, even if they
aren't directly representable at the type level. Especially Because one of the
goals of this type system is to implement first class functions, they will need
to be easily representable at the type level. 

This type system will closely mimic the Haskell type system, and as such 
functions will behave in a similar manner. Specifically, functions will only 
take in one parameter and return one output value. To represent functions
that take in multiple parameters, either a tuple can be used for the input 
parameter, or the function can be curried (e.g. `a -> b -> c`).

In Prolog, structs/tuples can be represented by using syntax similar to 
defining rules. For example, `toFloat`, a function that takes in an integer
and returns a float, could be encoded by:

```prolog
type(toFloat, fun(int, float))
```

In addition to function types, it would help to give the type checker an 
understand of function application and how to determine the type of a function
applied to an expression.

```prolog 
type(app(F, X), B) :-
    type(F, fun(A, B)),
    type(X, A).
```

In the above code, `app(F, X)` is used to represent "the function `F` applied 
to the value `X`". The type fact states that the type of `F` applied to `X` is
`B` if the type of `F` is a function from `A` to `B`, and the type of `X` is `A`.

Consider the type facts:

```prolog
type(f, fun(int, float)).
type(g, fun(float, string)).

type(x, int).
```

It makes sense that applying `f` to `x` would result in a float, and that 
applying `g` to that float would return a string. Lets see if our typechecker agrees:

```prolog
?- type(app(f, x), Ty)
Ty = float

?- type(app(g, app(f, x)), Ty)
Ty = string
```

It correctly infers the type of each expression. It also correctly identifies
nonsensical function applications:

```prolog
?- typecheck(app(f, f))
false
```

And this concept can be extended to (curried) functions that act on multiple
arguments. Here is a code snippet that shows a function `f` that takes in an 
integer and a float and returns a string:

```prolog
type(x, int).
type(y, float).
type(f, fun(int, fun(float, string))).

?- type(app(f, x), Ty)
Ty = fun(float, string)

?- type(app(app(f, x), y), Ty)
Ty = string
```

### Parametric Polymorphism


