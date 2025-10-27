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

As one might expect, this pattern extends to product types. Computing the 
cardinality of a product type is equal to the product of its inner cardinalities
multiplied together. In other words, for some product type $P$ where 
$P = \{p_1, p_2, ..., p_n\}$, $|P|=\prod_{i=1}^n|p_i|$.

The behavior of both sum types and product types can be used together to compute
the cardinality of any arbitrary ADT. Consider the following example:

```haskell 
data MyType a b = First Bool (Either a Bool)
                | Second (Either a b)
                | Third a b Bool
```

Now lets compute the cardinality of `MyType` parameterized by any two types 
$\alpha$ and $\beta$. First, we see that `MyType` is a sum type with tree 
constructors, so its cardinality must be the sum of the cardinalities of its
three constructors. Lets start with the first: `First Bool (Either a Bool)`.
This is a product type consisting of `Bool` and `Either a Bool`, so the 
cardinality will be $|Bool| + |Either\space\alpha\space Bool| = 2 + |Either\space\alpha\space Bool|$.
And re-using the rule for sum types, this simplifies to: $2 \times (|\alpha| + |Bool|) = 4 + 2|\alpha|$.
We do not know what type $\alpha$ is, so we cannot simplify any further.
Repeating this process for the next two constructors, we get that the second
constructor has the cardinality $|\alpha| + |\beta|$ and the third has the
cardinality $2|\alpha||\beta|$. Thus, the whole cardinality for `MyType` is 
$4 + 2|\alpha| + |\alpha| + |\beta| + 2|\alpha||\beta| = 4 + 3|\alpha| + |\beta| + 2|\alpha||\beta|$.

Before moving on to the next part, there is actually one more type of 
significance: the `Void` type. In Haskell this is defined as:

```haskell
data Void
```

Unlike the sum and product types, `Void` does not have any constructors. What 
this means in the context of a program is that it is impossible to create a
value of this type. Because of this behavior, `Void` is often used in proofs
to represent contradictary values, and to prove impossibilities. The cardainality
of this type is 0, because it has 0 inhabitants.

Lets look at the consequenes of this. Consider the type `Either Void Bool`. 
Using the cardinality rules previously defined, we can deduce $|Either\space Void\space Bool| = 0 + 2 = 2$.
And this makes sense. Because the `Left` constructor of Either requires a value
of type `Void`, `Left` cannot have any inhabitants. This means that the only 
inhabitants of `Either Void Bool` can be created using the `Right` constructor.
The `Right` constructor only has a `Bool` value, so we know there are only 
two inhabitants. This matches up with our previously calculated cardinality of 
2. This also has implications for product types as well. The cardinality of 
`(a, b)` is $|a| \times |b|$, which means that if either $|a|$ or $|b|$ is 0, 
then the cardinality of the entire tuple is also 0. This makes sense because
a tuple requires a value to be specified for both the left and the right side
at the same time.

## Isomorphisms

Now that we are able to represent ADTs as algebraic expressions, we can now 
look at one of the benefits of this sort of representation: isomorphisms. An 
isomorphism states that given two arbitrary types $\alpha$ and $\beta$, the
two are isomorphic if there exists two one-to-one mappings $f : \alpha\rightarrow\beta$
and $g : \beta\rightarrow\alpha$ such that $f \circ g = g \circ f = id$.

Lets example a simple morphism between the following two types:

```haskell 
data Either = Left () | Right ()
data Bool = True | False
```

In order to show that these two types are isomorphic, we need to specify 
mappings between both types:

```haskell
f :: Either -> Bool
f (Left ())  = False
f (Right ()) = True

g :: Bool -> Either
g True  = Right ()
g False = Left ()
```

And we need to show that composing the two mappings in either order is 
equivalent to the identity function. Since each type only has a small number
of inhabitants, we can simply show every possible scenario:

```haskell
f (g  True) => f (Right ()) => True
f (g False) => f (Left  ()) => False

g (f (Right ())) => g True  => Right ()
g (f (Left  ())) => g False => Left  ()
```

But lets look further into why exact this is. What exactly is necessary for 
these two types to be isomorphic? The answer is the have the same cardinality,
or same number of inhabitants. To further explain, if the two types do not have
the same cardinality, then one will have more inhabitants than the other. This
means that mapping from the type with more inhabitants to the other will 
ultimately result in a loss of information. And because of this, it becomes
impossible to create two lawful mappings that satisfy the isomorphism laws. 
Looking at the previous example, the cardinality of `Either` is $1 + 1 = 2$, and
the cardinality of `Bool` is also $2$. In fact, for any two types $\alpha$ and
$\beta$, if $|\alpha| = |\beta|$, then $\alpha\cong\beta$ ($\alpha$ is 
isomorphic to $\beta$).

### Derivatives

Apart from addition, multiplication, and exponentiation, one can also take
the _derivative_ of a type, which has some interesting properties. Lets consider
the following type:

```haskell
data MyType a = MyType a a
```

We know that the cardinality of `MyType a` is $a \times b$ (where $a$ and $b$ 
are cardinalities, not types). Lets take the derivative of `MyType a` with
respect to `a`:

$$\frac{\partial}{\partial a}(a \times a) = 2a$$

What exactly does this mean? Here, when we are taking the derivative of 
`MyType a`, we are actually computing the [zipper](https://wiki.haskell.org/index.php?title=Zipper)
of `MyType a`. If we focus one one particular `a` in `MyType a`, we would end
up with one of the folllowing types:

```haskell 
data MyType1 a = MyType1 a _
data MyType2 a = MyType2 _ a
```

The underscore represents the `a` we are focusing on. Here, we can see that 
there are two different `a`s we can focus on, and in both cases we have another
`a` that sits in the context. Using this intuition only, we could create a 
zipper type that uses the information:

```haskell
data MyTypeZipper a = MyTypeZipper Bool a
```

The `Bool` represents which `a` we are focusing on, and the other `a` represents
the value of the `a` we are not focusing on. Now let's revisit the derivative.
Recall that the cardinality of the derivative of `MyType a` was $2a$. Using the
cardinality rules in reverse, we can see that $2a$ is some kind of product type
where the left side has a cardinality of 2 and the right side has a cardinality
of $a$. We also know from earlier that `Bool` has a cardinality of 2, so we can 
assume that the left side is a boolean. We don't know exactly what $a$ is, so we
know the right side of the product type must be $a$, so we'll end up with some
type that looks like `(Bool, a)`. If we compare this to the `MyTypeZipper` type,
we can see that these two types are actually isomorphic because they have the
same cardinality.

This also works on more complex types. Lets create the zipper for the following 
type by computing its derivative:

```haskell
data MyType a = One a | Two a a
```

We'll start by computing the cardinality:

$$|MyType\space\alpha|= a + a \times a = a + a^2, a = |\alpha|$$

And now compute the derivative:

$$\frac{\partial}{\partial a}(a + a^2) = 1 + 2a$$

And construct a corresponding ADT:

```haskell
data MyZipper a = Either () (Bool, a)
```

Now lets understand the intuition. In `MyType a`, there are two possible 
constructors. If we focus on the `a` in the first constructor, then we don't 
have any other contextual information. Thus, we represent this scenario with
`Left ()`. In the second constructor, we can see that there are two different
`a`s we can focus on (which we specify in the zipper with a `Bool`), and will 
always end up with an extra `a` in the context. Hence, the `Right` constructor
will take in a value of type `(Bool, a)`.

## Automatically Computing the Derivative

Now, lets look at this in a Haskell program. Is there a way to automatically 
compute the derivative of a type using type level programming? If we make some
constraints on what sorts of ADTs we take in as input, then the answer is yes
we can. 

Lets start by defining a type family which computes the derivative of a type:

```haskell
-- derivative of ty with respect to x
type family TypeDeriv (x :: Type) (ty :: Type) :: Type where
```

As input, we are only going to pass in types that are constructed in one of
the following ways:

- Using the `Either` type
- Using the tuple type
- Using the `()` (unit) type
- Using the `Void` type

_Note: I could have also added `->` for exponentiation, but I am leaving that
out for simplicity._

Using standard differentiation rules, we can algebraically compute the
derivatives for each of these types:

$$\frac{\partial|\alpha+\beta|}{\partial x}=\frac{\partial a}{\partial x}+\frac{\partial b}{\partial x}$$
$$\frac{\partial|(\alpha\times\beta)|}{\partial x}=\frac{\partial a}{\partial x}b+a\frac{\partial b}{\partial x}$$
$$\frac{\partial|Unit|}{\partial x}=\frac{\partial}{\partial x}(1)=0$$
$$\frac{\partial|Void|}{\partial x}=\frac{\partial}{\partial x}(0)=0$$
$$\frac{\partial x}{\partial x}=1$$

Now, we can use these formulas in our type family:

```haskell
type family TypeDeriv (x :: Type) (ty :: Type) :: Type where
    TypeDeriv x (Either a b) = Either (TypeDeriv x a) (TypeDeriv x b)
    TypeDeriv x (a, b) = Either (TypeDeriv x a, b) (a, TypeDeriv x b)
    TypeDeriv x ()   = Void
    TypeDeriv x Void = Void
    TypeDeriv x x    = ()
```

Lets try this on the previous example:

```haskell
data MyType a = One a | Two a a
```

First, we need to rewrite `MyType` in terms of those supported by our type 
family:

```haskell
type MyType a = Either a (a, a)
```

And since type families have limitations on their resolution capabilites, we 
will create a fake arbitrary type `A` to act as a place holder for what would
be a typical type variable `a`.

```haskell
data A
```

Now, lets ask the compiler by using a hole what the type of `TypeDeriv A (Either A (A, A))`:

```haskell
myValue :: TypeDeriv A (MyType A)
myValue = _
```

```
Main.hs:40:8: error: [GHC-88464]
    • Found hole: _ :: Either () (Either ((), A) (A, ()))
    • In an equation for ‘myValue’: myValue = _
    • Relevant bindings include
        myValue :: TypeDeriv A (MyType A) (bound at Main.hs:40:1)
      Valid hole fits include
        myValue :: TypeDeriv A (MyType A) (bound at Main.hs:40:1)
   |
40 | myValue = _
   |           ^
```

We can see that the type family came up with the type `Either () (Either ((), A) (A, ()))`
instead of our previous answer of `Either () (Bool, a)`. When we came up with
an answer by hand, we did additional algebraic manipulations to simplify the answer.
Currently, the type family does not have the capability to do that (but could
theoretically!). We can prove that the type family returned a valid answer by
proving the two are isomorphic. If we look at both answers, we see that they 
both follow the pattern `Either () _`, so we know that at least that part is 
isomorphic because they are exactly the same. Now, we have to prove that 
`Either ((), A) (A, ())` is isomorphic to `(Bool, a)`. Lets compute the cardinality
of the first. When doing this, we get $1 \times A + A \times 1 = A + A = 2A$. 
This is equivalent to the cardinality of `(Bool, a)` which is $2 \times A = 2A$.
We see these two are isomorphic, and thus, the type family returned the correct
answer.
