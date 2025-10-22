---
title: Formally Verified Monads in Idris
author: Adam Brohl
---

One of the main appeals of programming in Haskell is the ability to express
algorithms with Monads, which allow the programmer to abstract sequences in any
way they like. For example, the State Monad abstracts the action of carrying an
extra state variable through different functions to provide a more ergonomic
interface for the programmer.

In Haskell, Monads are expected to follow a few laws, so that the behavior of
any Monad is consistent and doesn’t unintentionally produce bugs. There is one
problem with this: Haskell doesn’t provide an easy way to ensure that a Monad
actually follows these laws. Many are trivial an easy to implement, but some
are so complex and easy to get wrong that
[they warrant their own page on the Haskell wiki](https://wiki.haskell.org/ListT_done_right#Examples).

One way to solve this problem is by using a language like Idris, which boasts
support for dependent types and theorem proving. Idris is heavily inspired by
Haskell with an almost identical syntax, and for the most part, Functors,
Applicative Functors, and Monads are essentially the same in both with a couple
tweaks.

As defined in Haskell, for a type to be a Monad, it must also be a Functor and
an Applicative Functor. So in order to have a verified Monad, one first needs a
verified Functor. Below are the following types which will become verified
Monads:

```haskell 
data Box a = Value a

data Option a = Some a | None
```

## Verifying Functors

To begin, a new interface (same as a typeclass from Haskell) will be made to
demonstrate a verified functor. The laws a Functor must obey are:

1. **Identity**: `fmap id == id` - Mapping the identity function to a functor
    is equivalent to applying the identity function to the functor.
2. **Composition**: `fmap (f . g) == fmap f . fmap g` - Mapping the composition 
    of `f` and `g` is equivalent to mapping each one individually in the same order.

```haskell 
Id : a -> a
Id = id

interface Functor f => FunctorV f where 
  functorIdentity    : {a : Type} -> (x : f a) -> map Id x = x

  functorComposition : {a, b, c : Type}
                    -> (x : f a)
                    -> (g : b -> c)
                    -> (h : a -> b)
                    -> map (g . h) x = map g (map h x)
```

Note the Id function at the top. This is used in place if id in
`functorIdentity`'s type signature to avoid the compiler mistaking it for a
type variable instead of the actual identity function.

The curly bracket’s in `functorIdentity` represent an implict argument, where the
compiler will automatically determine the correct value. Next is `(x : f a)`,
which states that the parameter `x`` is of type `f a` where `f` is the functor that
is being verified. The return type is a proof stating the Functor identity law.

Assuming `Box` and `Option` have valid Functor implementations, 
`functorIdentity` is trivial to implement.

```haskell 
FunctorV Box where 
  functorIdentity (Value _) = Refl

  functorComposition = ?todo1

FunctorV Option where 
  functorIdentity None = Refl
  functorIdentity (Some _) = Refl

  functorComposition = ?todo2
```

`Refl` is the only value to the type `a = a` and is used to show trivial proofs, or
when both sides of `=` are identical. One thing that is important to note for
these proofs is that they all pattern match on the constructors specifically.
This is because the implementation of `map` for both `Box` and `Option` pattern
matches on the exact same constructors, which is what the compiler looks for.
In this case, pattern matching on the constructors is enough for Idris to
determine both sides of the `=` are equal, allowing the use of `Refl`, but with
more complex Functors more work might be required to construct the proof.

Next is the implementation of `functorComposition`, this law is more complex
than the identity law, but still follows the same principles. In fact, pattern
matching is all that’s needed in this scenario.

```haskell 
FunctorV Box where 
  functorIdentity (Value _) = Refl

  functorComposition (Value _) = Refl

FunctorV Option where 
  functorIdentity None = Refl
  functorIdentity (Some _) = Refl

  functorComposition None = Refl
  functorComposition (Some _) = Refl
```

## Verifying Applicative Functors

Now Box and Option are verified Functors, it’s time to look at the Applicative Functor laws:

1. **Identity**: `pure id <*> v = v` - Using the applicative operator in
    combination with the identity function on a functor is the same as
    applying the identity function to the functor.
2. **Composition**: `pure (.) <*> u <*> v <*> w = u <*> (v <*> w)` - Applying
    the functional composition operator to three Functors is the same as
    applying the first Functor to the application of the other two.
3. **Homomorphism**: `pure f <*> pure x = pure (f x)` - Applying a function
    to a value where both have been lifted via pure is the same as applying to
    the function to the value and then lifting the result.
4. **Interchange**: `u <*> pure y = pure ($ y) <*> u` - The order of
    application of a functor to a lifted value can be switched as long as the
    change in order is also reflected in the lifted value.

As done with `FunctorV`, the new `ApplicativeV` can be defined as:

```haskell 
interface (FunctorV f, Applicative f) => ApplicativeV f where 
    appId : {a : Type} -> (x : f a) -> pure Id <*> x = x

    appComp : {a, b, c : Type} 
           -> (u : f (b -> c)) 
           -> (v : f (a -> b))
           -> (w : f a)
           -> pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

    appHom : {a, b : Type} 
          -> (g : a -> b) 
          -> (x : a) 
          -> pure g <*> pure x = pure {f} (g x)

    appInter : {a, b : Type} 
            -> (u : f (a -> b)) 
            -> (y : a) 
            -> u <*> pure y = pure ($ y) <*> u
```

Note the use of `{f}` in the appHom function. The law itself never uses an
Applicative Functor explicitly, so the type checker can’t automatically infer
the type `f`. Thus, `{f}` implicitly passes the Functor to the `pure` function which
is just enough information for the type checker to be happy.

Other than that, the implementations for `ApplicativeV` are also trivial,
and only require pattern matching:

```haskell
ApplicativeV Box where
    appId    (Value _)                     = Refl
    appComp  (Value _) (Value _) (Value _) = Refl
    appHom   g x                           = Refl
    appInter (Value) _                     = Refl

ApplicativeV Option where
    appId None     = Refl 
    appId (Some _) = Refl

    appComp  None     _        _       = Refl 
    appComp (Some _)  None     _       = Refl 
    appComp (Some _) (Some _)  None    = Refl 
    appComp (Some _) (Some _) (Some _) = Refl 

    appHom g x = Refl 

    appInter None _     = Refl
    appInter (Some _) _ = Refl
```

## Verifying Monads

Finally, all of the groundwork as been done to prove the Monad laws for 
`Box`, and `Option`. These three laws are:

1. **Left Identity**: `pure x >>= k = k x` - Lifting a value with pure and binding it with a Monadic action is the same as applying the action to the value.
2. **Right Identity**: `n >>= pure = n` - Binding a Monadic value with pure will result in the same value.
3. **Associativity**: `n >>= (\x -> k x >>= h) = (n >>= k) >>= h` - The bind operator (`>>=`) is associative.

The definition of `MonadV` is as follows:

```haskell 
Pure : Applicative f => a -> f a
Pure = pure

interface (ApplicativeV m, Monad m) => MonadV m where 
    monadLeftId  : {a : Type} 
                -> (x : a) 
                -> (k : a -> m b) 
                -> pure x >>= k = k x

    monadRightId : {a : Type} -> (x : m a) -> x >>= Pure = x

    monadAssoc   : {a, b, c : Type} 
                -> (n : m a)
                -> (k : a -> m b) 
                -> (h : b -> m c)
                -> n >>= (\x => k x >>= h) = (n >>= k) >>= h
```

And pattern matching once again provides all the necessary proofs:

```haskell 
MonadV Box where
  monadLeftId x k        = Refl
  monadRightId (Box _)   = Refl
  monadAssoc (Box _) k h = Refl

MonadV Option where 
    monadLeftId x k         = Refl

    monadRightId None       = Refl
    monadRightId (Some _)   = Refl

    monadAssoc None _ _     = Refl
    monadAssoc (Some _) _ _ = Refl
```

Now everywhere `Box` or `Option` is used, it has been proven that they will follow
Monad and Functor laws because it was proven with their implementation of
`MonadV`. This also provides a great tool for programmers to ensure if their
implementation of a particular Monad is valid or not, as in Haskell the only
way to tell is if it type checks, which can yield many false positives.

This same technique can be applied to many other concepts, such as verifying
the associativity law of Semigroups, or even the identity law of Monoids, etc.
It also provides a great way to incorporate mathematical ideas into one’s code
without having to worry about the correctness of the implementation.
