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
overlap :: (Ord n) => (n, n) -> (n, n) -> Maybe (n, n)
overlap (a, b) (c, d)
  | min (a, b) (c, d) /= (a', b') = Just (b', c')
  | otherwise                     = Nothing
  where [a', b', c', d'] = sort [a, b, c, d]

-- Vectors ---------------------------------------------------------------------------------------------------------------------------------

-- | Applies a function to each component in a vector
dotmap :: Vector v => (a -> b) -> v a -> v b
dotmap f v = vzip (const . f) v v


-- | Performs component-wise operations
dotwise :: Vector v => (a -> b -> c) -> v a -> v b -> v c
dotwise = vzip -- Hmmm. Dotwise isn't really a fold is it?


-- | Dot product of two vectors
dot :: (Vector v, Floating f) => v f -> v f -> f
dot a b = vfold (+) 0 $ dotwise (*) a b
-- dot (Vector x y z) (Vector x' y' z') = (x * x') + (y * y') + (z * z') -- TODO: Refactor with Num instance (?)


-- | Euclidean distance between two points
euclidean :: (Vector v, Floating f) => v f -> v f -> f
euclidean a b = sqrt $ dot a b


-- |
magnitude :: (Vector v, Floating f) => v f -> f
magnitude v = euclidean v v


mag :: (Vector v, Floating f) => v f -> f
mag = magnitude


-- | Angle (in radians) between the positive X-axis and the vector
-- argument :: (Floating a, Eq a) => Vector a -> a
-- argument (Vector 0 0 0) = 0
-- argument (Vector x y z) = atan $ y/x


-- arg :: (Floating a, Eq a) => Vector a -> a
-- arg = argument


-- | Vector -> (magnitude, argument)
-- polar :: (Floating a, Eq a) => Vector a -> (a, a)
-- polar v@(Vector x y) = (magnitude v, argument v)



-- | Intersect
-- TODO: Math notes, MathJax or LaTex
-- TODO: Intersect for curves (functions) and single points (?)
-- TODO: Polymorphic, typeclass (lines, shapes, ranges, etc.) (?)
-- intersect :: Num a => Line a -> Line a -> Maybe (Vector a)
-- intersect _ _ = error "Not implemented" -- Nothing
--
--
-- -- |
-- intersects :: Num a => Line a -> Line a -> Bool
-- intersects a b = case intersect a b of
-- 	Just _  -> True
-- 	Nothing -> False
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
--
--
-- -- |
-- -- TODO: Intersect Rectangles
--
--
--
-- -- | Coefficients for the linear function of a Line (slope, intercept). The Z-component is ignored.
-- -- Fails for vertical and horizontal lines.
-- --
-- -- TODO: Use Maybe (?)
-- --
-- coefficients :: (Fractional a, Eq a) => Line a -> Maybe (a, a)
-- coefficients (Line (Vector ax ay _) (Vector bx by _))
-- 	| ax == bx  = Nothing
-- 	| ay == ay  = Nothing
-- 	| otherwise = let slope = (by - ay)/(bx - ax) in Just (slope, ay - slope*ax)
