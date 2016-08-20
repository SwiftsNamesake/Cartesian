-- |
-- Module      : Cartesian.Internal.Core
-- Description : Defines basic functionality that is shared between Plane and Space
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created October 31 2015

-- TODO | -
--        -

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Cartesian.Internal.Core (module Cartesian.Internal.Core,
                                module Cartesian.Internal.Types) where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.List    (sort)
import Control.Lens ((%~))

import Cartesian.Internal.Types
-- import Cartesian.Internal.Lenses



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------

-- | Finds the overlap between two ranges (lower bound, upper bound).
-- | Yields the overlap of two closed intervals (n ∈ R)
-- TODO: Normalise intervals (eg. (12, 5) -> (5, 12))
-- TODO: Type for intervals (which would also encode the 'openness' of the lower and upper limits)
overlap :: (Ord n) => (n, n) -> (n, n) -> Maybe (n, n)
overlap (a, b) (c, d)
  | min (a, b) (c, d) /= (a', b') = Just (b', c')
  | otherwise                     = Nothing
  where [a', b', c', d'] = sort [a, b, c, d]

-- Vectors ---------------------------------------------------------------------------------------------------------------------------------

-- | Applies a function to each component in a vector
dotmap :: (Vector v, Num a) => (a -> b) -> v a -> v b
dotmap f v = vzip (const . f) v v


-- | Performs component-wise operations
dotwise :: (Vector v, Num a) => (a -> b -> c) -> v a -> v b -> v c
dotwise = vzip


-- | Dot product of two vectors
dot :: (Vector v, Num f) => v f -> v f -> f
dot a b = vfold (+) 0 $ dotwise (*) a b
-- dot (Vector x y z) (Vector x' y' z') = (x * x') + (y * y') + (z * z') -- TODO: Refactor with Num instance (?)


-- | Cross product
-- cross :: (Vector v, Num f) => v f -> v f -> v f
-- cross a b = _


-- | Euclidean distance between two points
euclidean :: (Vector v, Floating f) => v f -> v f -> f
euclidean a b = sqrt $ dot a b


-- |
magnitude :: (Vector v, Floating f) => v f -> f
magnitude v = euclidean v v


-- |
mag :: (Vector v, Floating f) => v f -> f
mag = magnitude

-- Instances -------------------------------------------------------------------------------------------------------------------------------

-- |
instance (Vector v, Floating f) => Num (v f) where
  -- TODO: Helper method to reduce boilerplate for component-wise operations
  (+) = dotwise (+) --
  (-) = dotwise (-) --
  -- A × B = ||A|| ||B|| sin angle n.
  -- (*) a b     = mag a * mag b -- TODO: Is this really correct?
  (*) a b = error "Vector multiplication is still a work in progress."
  fromInteger = fromScalar . fromInteger             --
  negate      = dotmap negate
  signum v    = dotmap signum v      -- TODO: Proper way of implementing this function for vectors
  abs         = fromScalar . magnitude --

--------------------------------------------------------------------------------------------------------------------------------------------

-- | Vector -> (magnitude, argument)
-- polar :: (Floating a, Eq a) => Vector a -> (a, a)
-- polar v@(Vector x y) = (magnitude v, argument v)

--
--
-- -- | Yields the overlap of two closed intervals (n ∈ R)
-- -- TODO: Normalise intervals (eg. (12, 5) -> (5, 12))
-- overlap :: Real a => (a, a) -> (a, a) -> Maybe (a, a)
-- overlap a b
-- 	| leftmost /= (α, β) = Just $ (β, γ) --
-- 	| otherwise                                  = Nothing --
-- 	where [α, β, γ, _] = sort [fst a, snd a, fst b, snd b] -- That's right.
-- 	      leftmost     = minimumBy (comparing fst) [a, b]  --
