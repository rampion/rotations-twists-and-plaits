<!--
```haskell
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module README where
import Control.Monad.State (state, runState)
import Control.Applicative.Backwards (Backwards(..))
import Data.Functor.Product (Product(..))
import Data.Functor.Classes (Show1(..))
```
-->

List cycles
===========

Swapping adjacent items in a list:

```haskell
-- |
-- Swaps adjacent items in a list:
--
--    >>> twistList [0..3]
--    [1,0,3,2]
--
-- Leaves last element alone if list has odd length:
--
--    >>> twistList [0..4]
--    [1,0,3,2,4]
--
-- Serves as its own inverse:
--
--    >>> twistList . twistList $ [0..9]
--    [0,1,2,3,4,5,6,7,8,9]
--
twistList :: [a] -> [a]
twistList (a0:a1:as) = a1 : a0 : twistList as
twistList as = as
```

We don't have to start with the first item though:

```haskell
-- |
-- Swaps adjacent items in a list, skipping the first:
--
--    >>> offsetTwistList [0..4]
--    [0,2,1,4,3]
--
-- Leaves last element alone if list has even length:
--
--    >>> offsetTwistList [0..5]
--    [0,2,1,4,3,5]
--
-- Serves as its own inverse:
--
--    >>> offsetTwistList . offsetTwistList $ [0..9]
--    [0,1,2,3,4,5,6,7,8,9]
--
offsetTwistList :: [a] -> [a]
offsetTwistList (a:as) = a :  twistList as
offsetTwistList []     = []
```

Though each is its own inverse, we can compose them to get a `k`-cycle generator
for any list of length `k`.

```haskell

-- |
-- Permutes the contents of a list:
--
--     >>> plaitList [0..3]
--     [1,3,0,2]
--     >>> plaitList [0..4]
--     [1,3,0,4,2]
--
-- Repeating this permutation k times will yield the original order for a list
-- of length k:
--
--     >>> mapM_ print . take 5 $ iterate plaitList [0..3]
--     [0,1,2,3]
--     [1,3,0,2]
--     [3,2,1,0]
--     [2,0,3,1]
--     [0,1,2,3]
--     >>> mapM_ print . take 6 $ iterate plaitList [0..4]
--     [0,1,2,3,4]
--     [1,3,0,4,2]
--     [3,4,1,2,0]
--     [4,2,3,0,1]
--     [2,0,4,1,3]
--     [0,1,2,3,4]
--
plaitList :: [a] -> [a]
plaitList = offsetTwistList . twistList

```

Let's call this type of cycle generator a plait (to avoid collisions with braid
theory).

Choosing to do the offset twist second is a purely arbitrary choice, so there's
the inverse plait, which does the offset twist first:

```haskell
-- |
-- Permutes the contents of a list:
--
--     >>> inversePlaitList [0..3]
--     [2,0,3,1]
--     >>> inversePlaitList [0..4]
--     [2,0,4,1,3]
--
-- Repeating this permutation k times will yield the original order for a list
-- of length k:
--
--     >>> mapM_ print . take 5 $ iterate inversePlaitList [0..3]
--     [0,1,2,3]
--     [2,0,3,1]
--     [3,2,1,0]
--     [1,3,0,2]
--     [0,1,2,3]
--     >>> mapM_ print . take 6 $ iterate inversePlaitList [0..4]
--     [0,1,2,3,4]
--     [2,0,4,1,3]
--     [4,2,3,0,1]
--     [3,4,1,2,0]
--     [1,3,0,4,2]
--     [0,1,2,3,4]
--
-- Serves as an inverse for 'plaitList':
--
--     >>> plaitList . inversePlaitList $ [0..3]
--     [0,1,2,3]
--     >>> inversePlaitList . plaitList $ [0..4]
--     [0,1,2,3,4]

inversePlaitList :: [a] -> [a]
inversePlaitList = twistList . offsetTwistList
```

Other cycle generators for lists are left and right rotation, where every item
shifts one position in the same direction, except for the item that would be shifted
off, which is moved all the way to the other end of the list:

```haskell
-- |
-- Move the first item in a list to the end, shifting all the other items one
-- position forwards
--
--     >>> rotateLeftList [0..3]
--     [1,2,3,0]
rotateLeftList :: [a] -> [a]
rotateLeftList []     = []
rotateLeftList (a:as) = as ++ [a]

-- |
-- Move the last item in a list to the front, shifting all the other items one
-- position backwards
--
--     >>> rotateRightList [0..3]
--     [3,0,1,2]
rotateRightList :: [a] -> [a]
rotateRightList [] = []
rotateRightList as = last as : init as
```

A nice property of a plait (or inverse plait) is that individual elements don't
move more than two spots from their original position, so evaluating a portion
of a plaited list only looks ahead a constant amount.

This makes that plaits safe for use on infinite lists - requesting a finite
amount of a plaited infinite list only requires a finite amount of work.  The
same is true for left rotations of lists, but not right rotations.

<!--
```haskell
{- $
```
-->
```haskell
>>> take 10 $ plaitList [0..]
[1,3,0,5,2,7,4,9,6,11]
>>> take 10 $ inversePlaitList [0..]
[2,0,4,1,6,3,8,5,10,7]
>>> take 10 $ rotateLeftList [0..]
[1,2,3,4,5,6,7,8,9,10]

```
<!--
```haskell

-}
```
-->

Cycles of Traversables
======================

Using `State` as an applicative functor, we can generalize left and right
rotation to work on `Traversable`s:

```haskell
-- |
-- tie off a state-like computation
knot :: (s -> (a,s)) -> a
knot f = a where (a,s) = f s

-- |
-- hold a value to be used later
hold :: a -> a -> (a, a)
hold a a' = (a', a)

-- |
-- Move the last item in a traversable to the front, shifting all the other items one
-- position backwards
--
--     >>> rotateRight [0..3]
--     [3,0,1,2]
--     >>> rotateRight (Pair [0..3] [4..7])
--     Pair [7,0,1,2] [3,4,5,6]
--
rotateRight :: Traversable t => t a -> t a
rotateRight = knot . runState . traverse (state . hold)

-- |
-- Move the first item in a traversable to the end, shifting all the other items one
-- position forwards
--
--     >>> rotateLeft [0..3]
--     [1,2,3,0]
--     >>> rotateLeft (Pair [0..3] [4..7])
--     Pair [1,2,3,4] [5,6,7,0]
rotateLeft :: Traversable t => t a -> t a
rotateLeft = knot . runState . forwards . traverse (Backwards . state . hold)
```

To examine behaviour of cycles in the presence of co-data, it's useful to have
traversable types for streams that extend infinitely to the right or left.

```haskell
data ConsStream a = a :> ConsStream a
  deriving (Functor, Foldable, Traversable)
infixr 4 :>

data SnocStream a = SnocStream a :< a
  deriving (Functor, Foldable, Traversable)
infixl 4 :<
```

<!--
```haskell
iterateRight :: (a -> a) -> a -> ConsStream a
iterateRight f a = a :> iterateRight f (f a)

iterateLeft :: (a -> a) -> a -> SnocStream a
iterateLeft f a = iterateLeft f (f a) :< a

positives :: ConsStream Integer
positives = iterateRight (+1) 1

negatives :: SnocStream Integer
negatives = iterateLeft (subtract 1) (-1)

integers :: Product SnocStream ConsStream Integer
integers = Pair negatives (0 :> positives)

lows :: ConsStream Double
lows = (\a -> 1/2 - 1/2 ^ a) <$> positives

highs :: SnocStream Double
highs = (\a -> 1/2 + 1/2 ^ negate a) <$> negatives

limitHalf :: Product ConsStream SnocStream Double
limitHalf = Pair lows highs

instance Show1 ConsStream where
  liftShowsPrec = \f _ p -> showParen (p >= 4) . loop f 4 where
    loop :: (Int -> a -> ShowS) -> Int -> ConsStream a -> ShowS
    loop _ 0 _ = showString "…"
    loop f n (a :> as) = f 4 a . showString " :> " . loop f (n - 1) as

instance Show1 SnocStream where
  liftShowsPrec = \f _ p -> showParen (p >= 4) . loop f 4 where
    loop :: (Int -> a -> ShowS) -> Int -> SnocStream a -> ShowS
    loop _ 0 _ = showString "…"
    loop f n (as :< a) = loop f (n - 1) as . showString " :< " . f 4 a

instance Show a => Show (ConsStream a) where
  showsPrec = liftShowsPrec showsPrec showList

instance Show a => Show (SnocStream a) where
  showsPrec = liftShowsPrec showsPrec showList
```
-->

We can rotate left-infinite streams to the right, and right-infinite streams to the left:

<!--
```haskell
{- $
```
-->
```haskell
>>> negatives
… :< -4 :< -3 :< -2 :< -1
>>> rotateRight negatives
… :< -5 :< -4 :< -3 :< -2

```

```haskell
>>> positives
1 :> 2 :> 3 :> 4 :> …
>>> rotateLeft positives
2 :> 3 :> 4 :> 5 :> …

```
<!--
```haskell

-}
```
-->

Streams that extend infinitely in both directions or that have an infinite middle 
can be rotated either way.

<!--
```haskell
{- $
```
-->
```haskell
>>> integers
Pair (… :< -4 :< -3 :< -2 :< -1) (0 :> 1 :> 2 :> 3 :> …)
>>> rotateLeft integers
Pair (… :< -3 :< -2 :< -1 :< 0) (1 :> 2 :> 3 :> 4 :> …)
>>> rotateRight integers
Pair (… :< -5 :< -4 :< -3 :< -2) (-1 :> 0 :> 1 :> 2 :> …)

```

```haskell
>>> limitHalf
Pair (0.0 :> 0.25 :> 0.375 :> 0.4375 :> …) (… :< 0.5625 :< 0.625 :< 0.75 :< 1.0)
>>> rotateLeft limitHalf
Pair (0.25 :> 0.375 :> 0.4375 :> 0.46875 :> …) (… :< 0.625 :< 0.75 :< 1.0 :< 0.0)
>>> rotateRight limitHalf
Pair (1.0 :> 0.0 :> 0.25 :> 0.375 :> …) (… :< 0.53125 :< 0.5625 :< 0.625 :< 0.75)

```
<!--

```haskell
-}
```
-->
So can we extend plaits similarly?  We'd expect plaits to behave well on 
streams that extend in either or both directions, but not on streams with an
infinite middle.
