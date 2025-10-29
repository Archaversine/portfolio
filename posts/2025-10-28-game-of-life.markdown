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

The `comonad` library implements a comonad for us that behaves in a very similar
way: the `Store` comonad. In its simplest form, the `Store` comonad is 
implemented as:

```haskell 
data Store s a = Store { peek :: s -> a, pos :: s }
```

The `peek` value represents a function that takes in some index of type `s`,
and returns a value of type `a`. The `pos` function returns the current index.
In terms of the previous HDD example, `pos` represents where the needle is 
currently pointing to.

After defining a `Functor` instance, we can than define a `Comonad` instance:

```haskell
data Store s a = Store { peek :: s -> a, pos :: s } deriving Functor

instance Comonad (Store s) where
    extract st   = peek st (pos st)
    duplicate st = Store (\s -> Store (peek st) s) (pos st)
```

Extracting a value is as simple as peeking at the current position of the store.
Duplicating the store requires redefining the `peek` function in such a way 
that indexing with `s` returns a store whose position is set at `s`.

## Conway's Game of Life

Conway's Game of Life is an incredibly simple simulation containing a grid of 
cells which only have two states: alive and dead. The state of each cell is 
entirely dependent (except for the initial states which are provided by the 
user) on their adjacent neighbors. In other words, the value of each cell can
be extracted from the context (i.e. the board), which is exactly what the two
previous examples covered.

### Grid Type

Due to performance reasons, I will not be using the `Store` type that comes
in the `comonad` library. Instead, I will be creating a new datatype that 
resembles it very closely, and even create an instance of the `ComonadStore` 
typeclass for the new type.

```haskell
import Data.Vector (Vector)
import qualified Data.Vector as Vector

data Grid a = Grid !(Vector (Vector a)) (Int, Int) deriving Functor
```

As one might assume, the `Grid` type will be the comonad that represents the
state of all cells in the simulation. The datatype consists of two main parts.
The first is the actual array of cells, which is encoded as a vector of vectors,
and the second is a tuple of integers, which represents the location in the 
array that is currently "selected". This is very similar to the `Store` comonad
except instead of having an indexing function, we carry around the thing we are
indexing. Now, we can define an instance of `Comonad` for our new `Grid` type:

```haskell
instance Comonad Grid where
    extract   (Grid xs (x, y)) = xs ! x ! y
    duplicate (Grid xs (x, y)) = Grid duplicated (x, y)
        where duplicated = fmap (\(x', v)
                            -> fmap (\(y', _)
                                -> Grid xs (x', y')) (Vector.indexed v))
                            (Vector.indexed xs)

```

The `extract` function is trivial; it simply retrieves the values at the stored
pair of indices. The `duplicate` function is implemented in a very similar way
to the `Store` comonad, where each element of the original array becomes a vector
centered around that particular index.

I also mentioned the `ComonadStore` typeclass. This is similar to something
like `MonadState` where the `get` and `put` can be defined for monads other 
than `State`. We will use this to be able to use the `peek` and `pos` functions
on our `Grid` type:

```haskell
instance ComonadStore (Int, Int) Grid where
    pos (Grid _ p) = p
    peek (x, y) (Grid xs _) = xs ! x ! y 
```

### Implementing the Game

Now that we have our underlying `Grid` type, we can actually start implmeneting
the rest of the game, which is now shockingly easy. First, we need a function 
which computes all adjacent locations for an arbitrary given location. I've
defined two helper functions that do just that:

```haskell
gridSize :: Int
gridSize = 20

adjacentOffsets :: [(Int, Int)]
adjacentOffsets = [(x, y) | x <- [-1..1], y <- [-1..1], (x, y) /= (0, 0)]

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [ (x', y')
                   | (dx, dy) <- adjacentOffsets
                   , let x' = x + dx
                   , let y' = y + dy
                   , x' >= 0 && x' < gridSize
                   , y' >= 0 && y' < gridSize
                   ]
```

_Note: The `neighbors` function filters out any invalid locations, so we don't
have to worry about indexing in an unsafe way._

And finally, some helper functions for working with our grid type:

```haskell
printGrid :: Grid Bool -> IO ()
printGrid (Grid xs _) = mapM_ printRow xs
    where printRow row = putStrLn $ intersperse ' ' 
                                  $ map (bool '.' '#') (Vector.toList row)

gridFromSquares :: [(Int, Int)] -> Grid Bool
gridFromSquares xs = Grid grid (0, 0)
    where grid = Vector.generate gridSize
                    (\x -> Vector.generate gridSize (\y -> (x, y) `elem` xs))
```

The `gridFromSquares` function takes in a list of squares that should start
as active, and then generate the corresponding 2D vector. We are also using
`Grid Bool` as the actual type of the board, where `True` represents alive and
`False` represents dead.

Now, we can define the actual logic for the game that specifies what the state
of a square should be for the next iteration. The game of life has a very 
simple set of rules:

- If a cell is currently alive, it will remain alive as long as there are at 
  least 2 neighboring living cells and at most 3 neighboring cells. Otherwise, 
  it will die.
- If a cell is not currently alive, then it will _only_ become alive if there
  are exactly three neighboring cells that are alive.

We can represent this logic as a function that takes in a grid, and returns
the state for the currently selected cell:

```haskell
step :: Grid Bool -> Bool
step grid = if self then ns `elem` [2, 3] else ns == 3
    where ns   = length $ filter (flip peek grid) $ neighbors (pos grid)
          self = peek (pos grid) grid -- current cell state

```

Notice the type signature of `step`. It fits perfectly into the `(=>>)` operator
that we defined earlier for comonads. And this was done on purpose. Lets see 
what happens when we use this function on a grid.

```haskell
main :: IO ()
main = do 
    let grid = gridFromSquares [(1, 1), (1, 1), (2, 2), (3, 3), (5, 5)]

    printGrid grid
    putStrLn ""
    printGrid (grid =>> step)
```

We will get the following output to the screen:

```
. . . . . . . . . . . . . . . . . . . .
. # # . . . . . . . . . . . . . . . . .
. . # . . . . . . . . . . . . . . . . .
. . . # . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . .
. . . . . # . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . .

. . . . . . . . . . . . . . . . . . . .
. # # . . . . . . . . . . . . . . . . .
. # # # . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . .
```

And by the magic of the comonad, everything works. We can see that some of our
initial cells stayed active because they had a sufficient amount of adjacent
living neighbors, two of them died off because they were too isolated, and two
cells switched to alive because there were enough living neighbors nearby. Why
does this work? This works because when we use the `extend` function `(=>>)`
the `Grid` type is duplicated to a `Grid (Grid Bool)`, and then our `step` 
function is mapped via `fmap` to cause each inner `Grid Bool` to collapse back
into a `Bool`.
