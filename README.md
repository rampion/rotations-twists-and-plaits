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
```haskell ignore
>>> take 10 $ rotateRightList [0..]
<stack overflow>
```
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
-- swap a value with the current state
swap :: a -> a -> (a, a)
swap a a' = (a', a)

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
rotateRight = knot . runState . traverse (state . swap)

-- |
-- Move the first item in a traversable to the end, shifting all the other items one
-- position forwards
--
--     >>> rotateLeft [0..3]
--     [1,2,3,0]
--     >>> rotateLeft (Pair [0..3] [4..7])
--     Pair [1,2,3,4] [5,6,7,0]
rotateLeft :: Traversable t => t a -> t a
rotateLeft = knot . runState . forwards . traverse (Backwards . state . swap)
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

Rotating left-infinite streams to the left or right-infinite streams to the
right blows up, predictably:

```haskell ignore
>>> rotateRight positives
<stack overflow>
>>> rotateLeft negatives
<stack overflow>
```

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

So can we extend plaits similarly?  It's not hard to come up with an expected
behaviour for plaits of streams that extend in either or both directions:

```haskell ignore
>>> plait positives
2 :> 4 :> 1 :> 6 :> …
>>> plait negatives
… :< -6 :< -1 :< -4 :< -2
>>> plait integers
Pair (… :< -6 :< -1 :< -4 :< 1) (-2 :> 3 :> 0 :> 5 :> …)

```

However, ambiguity emerges when considering traversables with infinite middles.
We don't know the **parity** of the second half with respect to the first.

```haskell ignore
-- if 0.0 moves two positions right, should 1.0 move left two positions...
>>> plait limitHalf
Pair (0.25 :> 0.4375 :> 0.0 :> 0.484375 :> …)
     (… :< 0.515625 :< 1.0 :< 0.5625 :< 0.75)
>>> plait limitHalf -- ...or just one?
Pair (0.25 :> 0.4375 :> 0.0 :> 0.484375 :> …)
     (… :< 0.75 :< 0.5625 :< 1.0 :< 0.625)
```

To implement plaits, we'll be sending some values forwards and some values
backwards. We can't just use a `State s` or a `Backwards (State s)` applicative
functor; we need the
[Tardis](https://hackage.haskell.org/package/tardis/docs/Control-Monad-Tardis.html)
applicative functor.

```haskell
newtype Tardis bw fw a = Tardis { runTardis :: (bw,fw) -> (a, (bw,fw)) }
  deriving Functor

instance Applicative (Tardis bw fw) where
  pure a = Tardis $ \s -> (a,s)
  mf <*> ma = Tardis $ \ ~(bwx,fwx) -> 
    let ~(f, (bwf,fwf)) = runTardis mf (bwa,fwx)
        ~(a, (bwa,fwa)) = runTardis ma (bwx,fwf)
    in (f a, (bwf,fwa))
```

This gives us everything we need to implement twists for traversables:

```haskell
data Twist a = Twist 
  { parity  :: Bool
  , left    :: (a,a)
  , middle  :: (a,a)
  , right   :: (a,a)
  }
  deriving Functor

choose :: (a,a) -> Twist a
choose p@(a0,a1) = Twist True p p (a1,a0)

instance Applicative Twist where
  pure a = Twist False p p p where p = (a,a)
  ~(Twist x _ _ (f0,f1)) <*> ~(Twist y (a0,a1) _ _) = Twist z bl (b0,b1) br where
    b0 = f0 a0
    b1 = f1 a1
    bm = (b0,b1)
    bm' = (b1,b0)
    z = x /= y
    bl = if x then bm' else bm
    br = if y then bm' else bm
```

```haskell
dup :: a -> (a,a)
dup a = (a,a)

adjs :: Traversable t => t a -> t (a,a)
adjs = knot . uncurry . flip . curry . runTardis . traverse (Tardis . swap . dup)

alts :: Traversable t => t (a, a) -> (t a, t a)
alts = middle . traverse choose

twists :: Traversable t => t a -> (t a, t a)
twists = alts . adjs

-- |
-- Swaps adjacent items in a traversable:
--
--    >>> twist [0..3]
--    [1,0,3,2]
--    >>> twist positives
--    2 :> 1 :> 4 :> 3 :> …
--    >>> twist negatives
--    … :< -3 :< -4 :< -1 :< -2
--
-- Serves as its own inverse:
--
--    >>> twist . twist $ [0..9]
--    [0,1,2,3,4,5,6,7,8,9]
--
twist :: Traversable t => t a -> t a
twist = snd . twists

-- |
-- Swaps adjacent items in a list, skipping the first:
--
--    >>> offsetTwist [0..4]
--    [0,2,1,4,3]
--
-- Leaves last element alone if list has even length:
--
--    >>> offsetTwist [0..5]
--    [0,2,1,4,3,5]
--
-- Serves as its own inverse:
--
--    >>> offsetTwist . offsetTwist $ [0..9]
--    [0,1,2,3,4,5,6,7,8,9]
--
offsetTwist :: Traversable t => t a -> t a
offsetTwist = fst . twists
```
