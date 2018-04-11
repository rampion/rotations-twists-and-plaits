{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
module Braid where

import Control.Applicative.Backwards (Backwards(Backwards, forwards))
import Control.Monad.State (runState, state)
import Data.Traversable (foldMapDefault)

infixr 4 :>
infixl 4 :<
data ConsStream a = a :> ConsStream a
  deriving Functor
data SnocStream a = SnocStream a :< a
  deriving Functor
data BiStream a = BiStream (SnocStream a) a (ConsStream a)
  deriving (Functor, Show)

instance Foldable ConsStream where foldMap = foldMapDefault
instance Foldable SnocStream where foldMap = foldMapDefault
instance Foldable BiStream where foldMap = foldMapDefault

instance Traversable ConsStream where
  traverse f (a :> as) = (:>) <$> f a <*> traverse f as
instance Traversable SnocStream where
  traverse f (as :< a) = (:<) <$> traverse f as <*> f a
instance Traversable BiStream where
  traverse f (BiStream al a ar) = BiStream <$> traverse f al <*> f a <*> traverse f ar

instance Show a => Show (ConsStream a) where
  showsPrec = \p -> showParen (p >= 4) . loop 3 where
    loop :: Show a => Int -> ConsStream a -> ShowS
    loop 0 _ = showString "…"
    loop n (a :> as) = showsPrec 4 a . showString " :> " . loop (n - 1) as
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
