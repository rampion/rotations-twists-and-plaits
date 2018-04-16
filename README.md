<!-- ROTATIONS, TWISTS AND PLAITS -->
<!--
I'm going to hide some chunks of Haskell in HTML comments, if I don't find
them very pedagogically useful.

Who wants to start an article with a big blob of imports and language statemnts?
Not me.

```haskell
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module README where
import Control.Monad.State (State, state, runState)
import Control.Applicative.Backwards (Backwards(..))
import Data.Functor.Classes (Show1(..))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Product (Product(..))
import Data.Tuple (swap)
```
-->

<!-- 
Using trick from https://gist.github.com/kannankumar/4c613cac6d9db896062a16e1cc57d3e5
to get github to host images
-->

So the other day I was [doodling
in](https://www.youtube.com/watch?v=e4MSN6IImpI&list=PLF7CBA45AEBAD18B8) a
meeting, and I kept drawing little permutation diagrams, like this one of a rotation:

<img height="122px" src="https://user-images.githubusercontent.com/23003/38763434-8426513a-3f69-11e8-80e1-c106013efd68.png" title="a four-element rotation"/>

A nice thing about rotations is that if you repeat a rotation enough times,
you cycle back to the original order.

<img height="122px" src="https://user-images.githubusercontent.com/23003/38763435-8433c6bc-3f69-11e8-9005-f64a205f5c3e.png" title="a series of four four-element rotations"/>

As the meeting went on, my diagramming got less elegant as I remembered the
[id and twist building blocks that graphical linear algebra uses for
diagrammatic reasoning](https://graphicallinearalgebra.net/2015/05/06/crema-di-mascarpone-rules-of-the-game-part-2-and-diagrammatic-reasoning/)

<img height="71px" src="https://user-images.githubusercontent.com/23003/38763431-8402a884-3f69-11e8-9bcc-1ee87be17ef7.gif" title="id is a straight line, twist is two crossed lines"/>

Looking at this version of rotation diagram, the gap above the first twist popped out.

<img height="122px" src="https://user-images.githubusercontent.com/23003/38763436-84411c36-3f69-11e8-99cf-85544a1bab1e.png" title="id⊕id⊕twist ; id⊕twist⊕id ; twist⊕id⊕id"/>

Doing two twists at once seemed like an obvious variation.

<img height="122px" src="https://user-images.githubusercontent.com/23003/38763432-840e56f2-3f69-11e8-9e72-90c22af69f51.png" title="id⊕id⊕twist ; id⊕twist⊕id ; twist⊕id⊕id"/>

It produced an different permuation than the rotation diagram, but one that shared
the same property: iterate the process exactly n times on a set of n points, 
and you got back to your original order:

<img height="122px" src="https://user-images.githubusercontent.com/23003/38763433-8419cbe0-3f69-11e8-895d-da29c5f8182f.png" title="prev diagram repeated four times"/>

Later, I started calling this type of permutation a **plait**.

Both rotations and plaits define a [hamiltonian path](https://en.wikipedia.org/wiki/Hamiltonian_path).
Each level of the diagram could represent a position in a container, and the diagram
itself as a way to permute the values of the container.

"That's kind of neat", I thought.

Lists
=====

List rotations can be implemented fairly directly:

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

To implement a plait, we'll need a way to swap all the
adjacent items in the list, like the diagram version does
with a set of parallel twists.

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

The other half of a plait is also twisting all the adjacent pairs, but this
time at an offset, skipping the first element.

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

Composing the twist and the offset twist gives us the
plait.

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

A nice property of a plait (or inverse plait) is that individual elements don't
move more than two spots from their original position, so evaluating a portion
of a plaited list only looks ahead a constant amount.

This makes that plaits safe for use on infinite lists - requesting a finite
amount of a plaited infinite list only requires a finite amount of work.

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

```

An infinite list can be safely left-rotated.

```haskell
>>> take 10 $ rotateLeftList [0..]
[1,2,3,4,5,6,7,8,9,10]

```
<!--
```haskell
-}
```
-->

But right-rotations will blow up when you try to evaluate the first element.

```haskell ignore
>>> take 10 $ rotateRightList [0..]
<stack overflow>
```

Traversables
============

Now that we've got lists as a proof-of-concept, let's implement rotations and
plaits for general traversables.

To examine behaviour of these permutations in the presence of codata, it's useful to have
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

Using the `State` applicative functor, we can implement left and right
rotation on `Traversable`s:

```haskell
-- |
-- tie off a State computation
knotState :: State s a -> a
knotState ma = a where (a,s) = runState ma s

-- |
-- swap a value with the current state
swapState :: a -> State a a
swapState a = state $ \a' -> (a', a)

-- |
-- Move the last item in a traversable to the front, shifting all the other
-- items one position backwards
--
--     >>> rotateRight [0..3]
--     [3,0,1,2]
--     >>> rotateRight (Pair [0..3] [4..7])
--     Pair [7,0,1,2] [3,4,5,6]
--
rotateRight :: Traversable t => t a -> t a
rotateRight = knotState . traverse swapState

-- |
-- Move the first item in a traversable to the end, shifting all the other
-- items one position forwards
--
--     >>> rotateLeft [0..3]
--     [1,2,3,0]
--     >>> rotateLeft (Pair [0..3] [4..7])
--     Pair [1,2,3,4] [5,6,7,0]
rotateLeft :: Traversable t => t a -> t a
rotateLeft = knotState . forwards . traverse (Backwards . swapState)
```

We can rotate left-infinite traversables to the right, and right-infinite traversables to the left:

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

Rotating left-infinite traversables to the left or right-infinite traversables to the
right blows up, predictably:

```haskell ignore
>>> rotateRight positives
<stack overflow>
>>> rotateLeft negatives
<stack overflow>
```

Traversables that extend infinitely in both directions or that have an infinite middle 
can be safely rotated in either direction.

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

To implement plaits, we'll be sending some values forwards and some values
backwards. We can't just use a `State s` or a `Backwards (State s)` applicative
functor, we need both. The applicative functor that sends state forwards
and backwards is called a
[Tardis](https://hackage.haskell.org/package/tardis/docs/Control-Monad-Tardis.html).

```haskell
newtype Tardis bw fw a = Tardis { runTardis :: (bw,fw) -> (a, (bw,fw)) }
  deriving Functor

instance Applicative (Tardis bw fw) where
  pure a = Tardis $ \p -> (a,p)
  mf <*> ma = Tardis $ \ ~(bws, fws) -> 
    let ~(f, (bwf, fwf)) = runTardis mf (bwa, fws)
        ~(a, (bwa, fwa)) = runTardis ma (bws, fwf)
    in (f a, (bwf, fwa))

-- |
-- Tie off a 'Tardis' computation, using the forward state output as the
-- backwards state input, and vice versa
knotTardis :: Tardis s s a -> a
knotTardis ma = a where ~(a, (bw,fw)) = runTardis ma (fw,bw)

-- |
-- Swap a value with both the forward and backward state
swapTardis :: a -> Tardis a a (a,a)
swapTardis a = Tardis $ \p' -> (p', p) where p = (a,a)
```

Now we can send one copy of a value to the next position and another to the
previous position:

<!--
```haskell
{- $
```
-->
```haskell
>>> let spread = knotTardis . traverse swapTardis
>>> spread [0..7]
[(1,0),(2,0),(3,1),(4,2),(5,3),(6,4),(7,5),(7,6)]
>>> spread $ Pair [0..3] [4..7]
Pair [(1,0),(2,0),(3,1),(4,2)] [(5,3),(6,4),(7,5),(7,6)]

```
<!--
```haskell
-}
```
-->

Comparing one of the prior examples with what we know of twisting lists,
a pattern emerges:

```haskell ignore
[(1,0),(2,0),(3,1),(4,2),(5,3),(6,4),(7,5),(7,6)] -- spread [0..7]
[ 1   ,   0 , 3   ,   2 , 5   ,   4 , 7   ,   6 ] -- twistList [0..7]
[   0 , 2   ,   1 , 4   ,   3 , 6   ,   5 , 7   ] -- offsetTwistList [0..7]
```

`spread` calculates both twists simultaneously, we just need to tease them apart
by flipping every other pair and unzipping.

Flipping every pair is easy with a functor, flipping *every other* pair
is a little tricky.  What's needed is a way to keep track of parity -
flip the items with odd parity, don't flip the ones with even parity.

But where do count your parity from? If you count from the left using `State`,
you won't be process left-infinite codata. If you count from the right using `Reverse . State`,
you'll have the same problem with right-infinite codata.

The solution I came up with is to count parity locally, changing it as you
`<*>` terms together, and to only start counting from the left or right
when needed.


```haskell
data AlternatingUnzip a = AlternatingUnzip 
  { leftParity  :: Bool
  , rightParity :: Bool
  , middle      :: (a,a)
  }
  deriving Functor

parity :: AlternatingUnzip a -> Bool
parity AlternatingUnzip{..} = leftParity /= rightParity

left :: AlternatingUnzip a -> (a,a)
left AlternatingUnzip{..} = (if leftParity then id else swap) middle

right :: AlternatingUnzip a -> (a,a)
right AlternatingUnzip{..} = (if rightParity then swap else id) middle

-- breaks the identity, interchange, and composition laws for Applicatives
instance Applicative AlternatingUnzip where
  pure a = AlternatingUnzip False False (a,a)
  mf <*> ma = AlternatingUnzip
    { leftParity = parity mf
    , rightParity = parity ma
    , middle = let ~(f0,f1) = right mf
                   ~(a0,a1) = left ma
               in (f0 a1, f1 a0)
    }

count :: (a,a) -> AlternatingUnzip a
count = AlternatingUnzip True False

```

Now we can calculate both the twist and the offset twist
simultaneously:

```haskell
twists0 :: Traversable t => t a -> (t a, t a)
twists0 = left . traverse count . knotTardis . traverse swapTardis
```

<!--
```haskell
{- $
```
-->
```haskell
>>> twists0 [0..7]
([1,0,3,2,5,4,7,6],[0,2,1,4,3,6,5,7])
>>> twists0 $ Pair [0..3] [4..7]
(Pair [1,0,3,2] [5,4,7,6],Pair [0,2,1,4] [3,6,5,7])

```
<!--
```haskell
-}
```
-->

This approach works, but requires two traversals through the data. We can compute
the same value in a single traversal by composing the functors, rather than the functions:

```haskell
twists1 :: Traversable t => t a -> (t a, t a)
twists1 = left . knotTardis . getCompose
        . traverse (Compose . fmap count . swapTardis)
```

<!--
```haskell
{- $
```
-->
```haskell
>>> twists1 [0..7]
([1,0,3,2,5,4,7,6],[0,2,1,4,3,6,5,7])
>>> twists1 $ Pair [0..3] [4..7]
(Pair [1,0,3,2] [5,4,7,6],Pair [0,2,1,4] [3,6,5,7])

```
<!--
```haskell
-}
```
-->

Our choice to use the `left` view of the unzipping means we can't use it on
values that extend infinitely to the left:

```haskell ignore
>>> negatives
… :< -4 :< -3 :< -2 :< -1
>>> twists1 negatives
<stack overflow>
```

This is where the `middle` view of the `AlternatingUnzip` comes in handy.
If any view is finitely computable, then the middle view is.

```haskell
twists :: Traversable t => t a -> (t a, t a)
twists = middle . knotTardis . getCompose
       . traverse (Compose . fmap count . swapTardis)

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
--    >>> twist . twist $ positives
--    1 :> 2 :> 3 :> 4 :> …
--    >>> twist . twist $ negatives
--    … :< -4 :< -3 :< -2 :< -1
--    
--
twist :: Traversable t => t a -> t a
twist = fst . twists

-- |
-- Swaps adjacent items in a traversable, skipping the first:
--
--    >>> offsetTwist [0..4]
--    [0,2,1,4,3]
--    >>> offsetTwist positives
--    1 :> 3 :> 2 :> 5 :> …
--    >>> offsetTwist negatives
--    … :< -5 :< -2 :< -3 :< -1
--
-- Serves as its own inverse:
--
--    >>> offsetTwist . offsetTwist $ [0..9]
--    [0,1,2,3,4,5,6,7,8,9]
--    >>> offsetTwist . offsetTwist $ positives
--    1 :> 2 :> 3 :> 4 :> …
--    >>> offsetTwist . offsetTwist $ negatives
--    … :< -4 :< -3 :< -2 :< -1
--
offsetTwist :: Traversable t => t a -> t a
offsetTwist = snd  . twists
```

We could use these twists to implement the plaits as we did for lists:

```haskell ignore
plait :: Traversable t => t a -> t a
plait = offsetTwist . twist

inversePlait :: Traversable t => t a -> t a
inversePlait = twist . offsetTwist
```

But this definition would make calculating each plait take two traversals, and
calculating both take four. Just as we calculated both twists with a single
traversal, so too can we calculate both plaits with a single traversal.  The
trick is to use a two item queue for the tardis state:

```haskell
delayedSwapTardis :: a -> Tardis (a,a) (a,a) (a,a)
delayedSwapTardis a = Tardis $ \ ~((bw0,bw1),(fw0,fw1)) -> 
  ((bw0,fw0), ((bw1,a),(fw1,a)))
```

Now we can copy a value to positions two moves away in a single pass:

<!--
```haskell
{- $
```
-->
```haskell
>>> let spread2 = knotTardis . traverse delayedSwapTardis
>>> spread2 [0..7]
[(2,1),(3,0),(4,0),(5,1),(6,2),(7,3),(7,4),(6,5)]

```
<!--
```haskell
-}
```
-->

And all we need to do to make the plaits is compose with
alternating unzipping.

```haskell
plaits :: Traversable t => t a -> (t a, t a)
plaits = middle . knotTardis . getCompose
       . traverse (Compose . fmap count . delayedSwapTardis)

-- |
-- Permutes the contents of a traversable:
--
--     >>> plait (Pair [0..3] [4,5])
--     Pair [1,3,0,5] [2,4]
--     >>> plait positives
--     3 :> 1 :> 5 :> 2 :> …
--     >>> plait negatives
--     … :< -2 :< -5 :< -1 :< -3
--
-- Repeating this permutation k times will yield the original order for a traversable
-- containing k items:
--
--     >>> mapM_ print . take 7 . iterate plait $ Pair [0..3] [4,5]
--     Pair [0,1,2,3] [4,5]
--     Pair [1,3,0,5] [2,4]
--     Pair [3,5,1,4] [0,2]
--     Pair [5,4,3,2] [1,0]
--     Pair [4,2,5,0] [3,1]
--     Pair [2,0,4,1] [5,3]
--     Pair [0,1,2,3] [4,5]
--
plait :: Traversable t => t a -> t a
plait = fst . plaits

-- |
-- Permutes the contents of a traversable
--
--     >>> inversePlait (Pair [0,1] [2..5])
--     Pair [2,0] [4,1,5,3]
--     >>> inversePlait positives
--     2 :> 4 :> 1 :> 6 :> …
--     >>> inversePlait negatives
--     … :< -6 :< -1 :< -4 :< -2
--
-- Repeating this permutation k times will yield the original order for a traversable
-- containing k items:
--
--     >>> mapM_ print . take 7 . iterate inversePlait $ Pair [0,1] [2..5]
--     Pair [0,1] [2,3,4,5]
--     Pair [2,0] [4,1,5,3]
--     Pair [4,2] [5,0,3,1]
--     Pair [5,4] [3,2,1,0]
--     Pair [3,5] [1,4,0,2]
--     Pair [1,3] [0,5,2,4]
--     Pair [0,1] [2,3,4,5]
--
inversePlait :: Traversable t => t a -> t a
inversePlait = snd . plaits
```

In addition to the example above, plaits perform well on traversables that
extend out infinitely in two directions:

<!--
```haskell
{- $
```
-->
```haskell
>>> integers
Pair (… :< -4 :< -3 :< -2 :< -1) (0 :> 1 :> 2 :> 3 :> …)
>>> plait integers
Pair (… :< -6 :< -1 :< -4 :< 1) (-2 :> 3 :> 0 :> 5 :> …)
>>> inversePlait integers
Pair (… :< -2 :< -5 :< 0 :< -3) (2 :> -1 :> 4 :> 1 :> …)

```
<!--
```haskell
-}
```
-->

However, ambiguity emerges when considering traversables with infinite middles.
We can't compute the parity of the second half with respect to the first, 
so the computation explodes.

```haskell ignore
>>> limitHalf
Pair (0.0 :> 0.25 :> 0.375 :> 0.4375 :> …) (… :< 0.5625 :< 0.625 :< 0.75 :< 1.0)
>>> plait limitHalf
<stack overflow>
>>> inversePlait limitHalf
<stack overflow>
```

So while you can plait some traversables that you can't rotate, the converse is true too.

Related topics
==============

While putting this post together it finally clicked in my head what my initial
diagrams reminded me of - [sorting networks](https://en.wikipedia.org/wiki/Sorting_network).
While [others have implemented sort-via-traverse](https://gist.github.com/treeowl/9621f58d55fe0c4f9162be0e074b1b29),
it be interested to see if the above techniques could be extended to implement a sorting network applicative.

Literate Haskell
================

This markdown file is literate haskell. 

If saved or symlinked as `README.lhs`, all the definitions can be compiled
by using the [`markdown-unlit`](https://hackage.haskell.org/package/markdown-unlit)
preprocessor:

```bash
$ ghc -pgmL markdown-unlit README.lhs
[1 of 1] Compiling README           ( README.lhs, README.o )
```

All the examples (except the ones that result in `<stack overflow>`) can be
checked via [`doctest`](https://hackage.haskell.org/package/doctest):

```bash
$ doctest -pgmL markdown-unlit README.lhs
Examples: 67  Tried: 67  Errors: 0  Failures: 0
```
