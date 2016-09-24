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
import Control.Lens ((^.))
import Control.Applicative

import Cartesian.Internal.Types
import Cartesian.Internal.Lenses



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
  where
    [a', b', c', d'] = sort [a, b, c, d]

-- Vectors ---------------------------------------------------------------------------------------------------------------------------------

-- | Applies a function to each component in a vector
dotmap :: Vector v => (a -> b) -> v a -> v b
dotmap f v = vzip (const . f) v v


-- | Performs component-wise operations
dotwise :: Vector v => (a -> b -> c) -> v a -> v b -> v c
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

-- Convenience constructors ----------------------------------------------------------------------------------------------------------------

-- TODO: Generalise constructors, move to Internal.Core (✓)

-- | Creates a bounding box from two opposite corners
-- TODO: Better name (?)
-- TODO: Don't make assumptions about WHICH corners they are (✓)
-- TODO: Should we care about degenerate cases (such as 'a' and 'b' being identical)
fromCorners :: (Applicative v, Num n, Ord n) => v n -> v n -> BoundingBox (v n)
fromCorners a b = BoundingBox { cornerOf = min <$> a <*> b,
                                sizeOf   = abs <$> liftA2 (-) b a }


-- | 
fromAxes :: (Applicative v) => Axes v n -> BoundingBox (v n)
fromAxes axes' = let (begin', size') = unzipA axes' in BoundingBox { cornerOf = begin', sizeOf = size' }


-- | Top Left Bottom Right
fromExtents :: (Applicative v, Num n) => Axes v n -> BoundingBox (v n)
fromExtents extents' = let (begin', end') = unzipA extents' in BoundingBox { cornerOf = begin', sizeOf = liftA2 (-) end' begin' }

-- Booleans --------------------------------------------------------------------------------------------------------------------------------

-- | Finds the intersection (boolean AND) of two bounding boxes
intersect :: (Applicative v, Traversable v, Ord n, Num n) => BoundingBox (v n) -> BoundingBox (v n) -> Maybe (BoundingBox (v n))
intersect a b = do
  overlaps' <- traverse (uncurry overlap) (zipA (a^.extents) (b^.extents))
  return $ fromExtents overlaps'

