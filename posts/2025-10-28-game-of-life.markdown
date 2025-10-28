---
title: Implementing Game of Life with Comonads
author: Adam Brohl
---

One of the most highly associated words in Haskell is _monad_, and for good 
reason too. Without monads, programming in Haskell would be extremely tedious,
and a lot of generalized abstractions wouldn't even exist. But there is a lot
more to Haskell than just monads. As you might expect from the title of this 
post, I am talking about comonads. There are already multiple posts explaining
comonads, and especially how to model Conway's Game of Life with them, but I 
personally found them to be more technical and offer little to no intuitive 
explanations. Hopefully this post provides that.

## What is a Comonad?

First, what exactly is a comonad. The most often used definition I see people
give to this quesion is that it is, "the dual of a monad." If you are not well
versed in category theory, I find this explanation to be a bit lacking. Lets 
examine exactly what that means. In Haskell, we can define the `Monad`
typeclass is like so:

```haskell
class Applicative m => Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
```

Intuitively, we can think of the `(>>=)` operater as something that:

- Takes in a value `a` in some context `m`
- Takes in a function that acts on an `a` and returns `b` in a context `m`
- Returns a `b` in context `m`.

When we take the dual of monad, what we actually do in Haskell is reverse the
direction of the arrows to something like: `m a <- (a <- m b) <- m b`. We can 
flip the expression around by pointing the arrows to the right instead of the
left and get: `m b -> (m b -> a) -> m a`. We can also swap `b` and `a` so that
`a` appears first, which we can do because they are both type variables, and
get: `m a -> (m a -> b) -> m b`. And finally, a comonad is typically represented
by `w` instead of `m` (where the `w` is a horizontally mirrored `m`), so we can
rewrite it as: `w a -> (w a -> b) -> w b`. And this new function is the dual 
of bind, named `extend`.

Due to some technical reasons beyond the scope of this post, there is no 
coapplicative class in Haskell. Instead, one typically tries to define all the 
relevant functions inside the `Comonad` typelcass. So lets look at the three
functions from `Monad` we'll need to compute the duals of:

- `(>>=)  :: m a -> (a -> m b) -> m b`
- `join   :: m (m a) -> m a`
- `return :: a -> m a`

`join` and `return` are relatively easy to compute the duals of, as we just
use the same process as before:

- `cojoin   :: w a -> w (w a)`
- `coreturn :: w a -> a`

While `cojoin` and `coreturn` are used in some places, they often go by 
more intuitive names. `cojoin` is usually referred to as `duplicate`, and 
`coreturn` is typically called `extract`. I will use the latter in both cases.
It is also worth noting that the dual of `(>>=)` is named `(=>>)`. Putting
these all together, we can finally write code for the `Comonad` typeclass:

```haskell
class Functor w => Comonad w where
    (=>>)     :: w a -> (w a -> b) -> w b
    extract   :: w a -> a 
    duplicate :: w a -> w (w a)
```

_Note: `duplicate` and `extend` (`=>>`) can actually be defined in terms of 
each other. So when creating a comonad instance, you typically only define
2 out of the three functions._

All of these will be explained in more further detail further on. But for now, 
Intuitively, we could think of the each of the following intuition-based 
explanations for all the comonad functions:

- `(=>>)`: This function takes in an `a` in some context `w`, and a function 
  that maps from a `w a` to some value `b`, and returns a `b` in some context.
- `extract`: This function _extracts_ an `a` out of some context `w`.
- `duplicate`: This function _duplicates_ the context `w` of `a`.

### Real World Example

A real life example of a comonad that really stuck with me was a HDD. 
Internally, an HDD can contain terabytes of information, but accesses singular
points of the data using a type of needle. Here, the `extract` function would
read the data the needle is currently pointing to, the `duplicate` function
would treat each individually point as an HDD centered around that point, and 
the `(=>>)` operator can be thought of something that takes an HDD on the left 
side, and a formula which may use information from anywhere on the drive that
yields a single value, which will result in a new HDD with that formula applied
to every point.

### Store Comonad

The `comonad` library gives us a tool 

## Conway's Game of Life

### Why Are Comonads Useful Here?

### Grid Type

### Implementing the Game
