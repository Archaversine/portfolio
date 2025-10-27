---
title: Automatic Type Differentiation in Haskell
author: Adam Brohl
mathjax: on
---

## Algebra of Types

Haskell is one of the few programming languages that fully supports Algebraic 
Data Types (ADTs) without requiring silly workarounds. As one might expect, 
ADTs have algebraic properties similar to algebra with numbers. These operations
include addition, multiplication, exponentiation, and even differentiation. This
post explores how to implement automatic type differentiation in Haskell.

### Type Cardinality

When working with types, an often useful piece of information to know is how 
many valid inhabitants (the cardinality) a type has. For example, a boolean
has exactly two valid inhabitants: true and false. The cardinality of a type
$T$ is represented by the notation $|T|$. If two different types have the same
cardinality, they are said to be isomorphic. This is because a bidirectional 
one-to-one mapping can be constructed between both types.

Let's start with one of the simplest types in Haskell: the unit type. The unit
type (written as `()`) has exactly one inhabitant: `()`. Because of this,
the cardinality of the unit type is one ($|()| = 1$). When performing 
multiplication, the unit type can be thought of as an identity value.

The next interesting type is the sum type. The most common sum type in Haskell
is the `Either` type:

```haskell 
data Either a b = Left a | Right b
```

To determine the cardinality of this type, lets look at `Either` with some
types we've already looked at: unit and boolean (`Either () Bool`). Looking
at the `Left` constructor, there is only one inhabitant: `Left ()`. And
looking at the `Right` constructor, there are two inhabitants: `Right True`
and `Right False`. So in total, there are $1 + 2 = 3$ inhabitants of
`Either () Bool`, so its cardinality is 3. A sharp reader may notice that _sum_
types _add_ cardinalities together, and this is correct. To generalize this
for any two types $\alpha$ and $\beta$ with `Either`:
$|Either\space\alpha\space\beta| = |\alpha| + |\beta|$. In fact, for any sum type $S$ with
$n$ constructors, $|S|=\sum_{i=1}^n|C_i|$ (where $C_i$ is the _ith_ constructor).
