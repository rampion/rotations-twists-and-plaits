{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
module Braid where

import Control.Applicative.Backwards (Backwards(Backwards, forwards))
import Control.Monad.State (runState, state)
import Data.Traversable (foldMapDefault)

data ConsStream a = a :> ConsStream a
  deriving Functor
infixr 4 :>

instance Foldable ConsStream where foldMap = foldMapDefault

instance Traversable ConsStream where
  traverse f (a :> as) = (:>) <$> f a <*> traverse f as

instance Show a => Show (ConsStream a) where
  showsPrec = \p -> showParen (p >= 4) . loop 3 where
    loop :: Show a => Int -> ConsStream a -> ShowS
    loop 0 _ = showString "…"
    loop n (a :> as) = showsPrec 4 a . showString " :> " . loop (n - 1) as

data SnocStream a = SnocStream a :< a
  deriving Functor
infixl 4 :<

instance Foldable SnocStream where foldMap = foldMapDefault

instance Traversable SnocStream where
  traverse f (as :< a) = (:<) <$> traverse f as <*> f a
instance Show a => Show (SnocStream a) where
  showsPrec = \p -> showParen (p >= 4) . loop 3 where
    loop :: Show a => Int -> SnocStream a -> ShowS
    loop 0 _ = showString "…"
    loop n (as :< a) = loop (n - 1) as . showString " :< " . showsPrec 4 a

-- |
-- >>> positives
-- 1 :> 2 :> 3 :> …
positives :: ConsStream Integer
positives = loop 1 where
  loop !n = n :> loop (n+1)

-- |
-- >>> negatives
-- … :< -3 :< -2 :< -1
negatives :: SnocStream Integer
negatives = loop (-1) where
  loop !n = loop (n-1) :< n

-- |
-- >>> naturals
-- BiStream (… :< -3 :< -2 :< -1) 0 (1 :> 2 :> 3 :> …)
naturals :: BiStream Integer
naturals = BiStream negatives 0 positives

-- |
-- >>> rotateR [0..7]
-- [7,0,1,2,3,4,5,6]
-- >>> rotateR negatives
-- … :< -4 :< -3 :< -2
rotateR :: Traversable t => t a -> t a
rotateR ta = ta' where
  ~(ta', a) = traverse (\a -> state $ \s -> (s, a)) ta `runState` a

-- |
-- >>> rotateL [0..7]
-- [1,2,3,4,5,6,7,0]
-- >>> rotateL positives
-- 2 :> 3 :> 4 :> …
rotateL :: Traversable t => t a -> t a
rotateL ta = ta' where
  ~(ta', a) = forwards (traverse (\a -> Backwards . state $ \s -> (s, a)) ta) `runState` a

data Braid s a = Braid (s -> s -> (s, a, s)) (s -> s -> (s, a, s))
  deriving Functor

-- …a b c d e f g h…
-- …b a d c f e h g…
--   …d a f c h e…
--
--

hold :: a -> Braid a a
hold a = Braid (\l r -> (r,l,a))
               (\l r -> (a,r,l))

-- |
-- >>> tieL $ traverse hold [0..5]
-- [1,0,3,2,5,4]
-- >>> tieR $ traverse hold [0..5]
-- [0,2,1,4,3,5]
instance Applicative (Braid s) where
  pure a = Braid f f where f l r = (r,a,l)

  ~(Braid xf yf) <*> ~(Braid xa ya) = Braid xb yb where
    xb l r = 
      let ~(lf, f, rf) = xf l la
          ~(la, a, ra) = ya rf r
      in (lf, f a, ra)
    yb l r =
      let ~(lf, f, rf) = yf l la
          ~(la, a, ra) = xa rf r
      in (lf, f a, ra)
    
getBraid :: Braid s a -> a
getBraid (Braid x _) = a where
  ~(l, a, r) = x l r

braid :: Traversable t => t a -> t a
braid = getBraid . traverse hold

{-
-- |
-- >>> braid [0..5]
-- [2,0,4,1,5,3]
-- >>> braid it
-- [4,2,5,0,3,1]
-- >>> braid it
-- [5,4,3,2,1,0]
-- >>> braid it
-- [3,5,1,4,0,2]
-- >>> braid it
-- [1,3,0,5,2,4]
-- >>> braid it
-- [0,1,2,3,4,5]
-- >>> braid negatives
-- >>> braid positives
-- >>> braid naturals
braid :: Traversable t => t a -> t a
braid = tie . left . traverse hold . value . traverse hold . tie . right . traverse hold
-}
