-- |
-- Module      : Cartesian.Plane
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, year
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created date year

-- TODO | - Which constraints are appropriate (Num is probably too generic, should be Real, maybe RealFrac)
--        - Strictness, performance

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Cartesian.Plane (module Cartesian.Plane,
                        module Cartesian.Plane.Types,
                        magnitude, dotmap) where -- TODO: Why do I need to export 'intersect' specifically when I'm already exporting this entire module



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.List (sort, minimumBy)
import Data.Ord  (comparing)
import Data.Complex hiding (magnitude)

import           Control.Monad (when)
import           Control.Applicative

import           Control.Lens ((^.))
import qualified Control.Lens as L

-- import Southpaw.Utilities.Utilities (pairwise)

import Cartesian.Internal.Types
import Cartesian.Internal.Lenses
import Cartesian.Internal.Core

import Cartesian.Space.Types
import Cartesian.Plane.Types



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------

-- | Determines if a point lies within a polygon using the odd-even method.
--
-- TODO: Use epsilon (?)
-- TODO: How to treat points that lie on an edge
inside :: Num n => Polygon n -> Vector2D n -> Bool
inside polygon (Vector2D x y) = undefined
  where
    lines   = polygon ++ [head polygon] -- Close the loop
    -- between (Line (Vector ax ay) (Vector bx by)) = _


-- |
-- instance Convertible (Vector2D f, Vector3D f) where
  -- _


-- |
to3D :: Num f => Vector2D f -> Vector3D f
to3D (Vector2D x' y') = Vector3D x' y' 0


-- |
from3D :: Num f => Vector3D f -> Vector2D f
from3D (Vector3D x' y' _) = Vector2D x' y'


-- | Perform some unary operation on a 2D vector as a 3D vector, converting the result back to 2D by discarding the z component.
-- TODO: Rename (?)
-- TODO: Loosen Num restriction (eg. to anything with a 'zero' value) (?)
in3D :: (Num f, Num f') => (Vector3D f -> Vector3D f') -> Vector2D f -> Vector2D f'
in3D f = from3D . f . to3D


-- | Same as in3D, but for binary operations.
-- _ :: _
-- _



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------

-- Vector math -----------------------------------------------------------------------------------------------------------------------------

-- | Angle (in radians) between the positive X-axis and the vector
-- argument :: (Floating a, Eq a) => Vector a -> a
-- argument (Vector 0 0) = 0
-- argument (Vector x y) = atan $ y/x
--
--
-- arg :: (Floating a, Eq a) => Vector a -> a
-- arg = argument
--
--
-- -- | Vector -> (magnitude, argument)
-- polar :: (Floating a, Eq a) => Vector a -> (a, a)
-- polar v@(Vector x y) = (magnitude v, argument v)

-- Geometry --------------------------------------------------------------------------------------------------------------------------------

-- | Yields the intersection point of two finite lines. The lines are defined inclusively by
--   their endpoints. The result is wrapped in a Maybe value to account for non-intersecting
--   lines.
--
-- TODO: Move equation solving to separate function (two linear functions)
-- TODO: Simplify logic by considering f(x) = y for vertical lines (?)
-- TODO: Return Either instead of Maybe (eg. Left "parallel") (?)
--
-- TODO: Math notes, MathJax or LaTex
-- TODO: Intersect for curves (functions) and single points (?)
-- TODO: Polymorphic, typeclass (lines, shapes, ranges, etc.) (?)
--
-- TODO: I'm pretty sure I've finished this function and then misplaced it...
-- intersect :: RealFrac n => Line n -> Line n -> Maybe (Vector2D n)
-- intersect a b = do
--   when ((fst $ deltas a) == 0) $ Just (error "Not implemented")
--   when ((fst $ deltas b) == 0) $ Just (error "Not implemented")
--   when (slope a == slope b)    $ Nothing
--   let Just $ Vector2D () ()
--   where
--     deltas   (Line (Vector2D ax ay) (Vector2D bx by)) = (bx - ax, by - ay) -- TODO: Rename (eg. deltas) (?)
--     vertical (Line (Vector2D ax _) (Vector2D bx _))   =  ax == bx
--     slope line     = let (dx, dy) = deltas line in dy/dx
--     intercept line@(Line (Vector x y) _)
--       | vertical line = Nothing
--       | otherwise     = Just $ y - slope line * x

-- Linear functions ------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Refactor
-- TODO: Invariants, check corner cases
-- TODO: Deal with vertical lines
-- TODO: Factor out infinite-line logic
-- TODO: Decide how to deal with identical lines
-- TODO: Factor out domain logic (eg. write restrict or domain function)
-- TODO: Visual debugging functions
intersect :: RealFloat f => Line (Vector2D f) -> Line (Vector2D f) -> Maybe (Vector2D f)
intersect f' g' = do
  p <- mp
  indomain f' p
  indomain g' p
  where
    -- indomain :: RealFloat f => Line (Vector2D f) -> Vector2D f -> Maybe (Vector2D f)
    indomain h' = restrict (h'^.begin) (h'^.end)

    -- mp :: Maybe (Vector2D f)
    mp = case [linear f', linear g'] of
      [Just f, Nothing] -> let x' = g'^.begin.x in Just $ Vector2D (x') (plotpoint f x')
      [Nothing, Just g] -> let x' = f'^.begin.x in Just $ Vector2D (x') (plotpoint g x')
      [Just f,  Just g] -> linearIntersect f g
      _                 -> Nothing


-- | Gives the linear function overlapping the given segment
linear :: RealFloat f => Line (Vector2D f) -> Maybe (Linear f)
linear line = Linear <$> intercept line <*> slope line


-- | Applies a linear function to the given value
-- TODO: Rename (?)
plotpoint :: RealFloat f => Linear f -> f -> f
plotpoint f x = slopeOf f*x + interceptOf f

-- ax + b = αx + β
-- ax - αx = β - b
-- (a - α)x = (β - b)
-- x = (β - b)/(a - α)

-- | Finds the intersection (if any) of two linear functions
linearIntersect :: RealFloat f => Linear f -> Linear f -> Maybe (Vector2D f)
linearIntersect f g
  | slopeOf f == slopeOf g  = Nothing
  | otherwise = let x = (β-b)/(a-α) in Just $ Vector2D x (a*x + b)
  where
    [a, α] = map slopeOf     [f, g]
    [b, β] = map interceptOf [f, g]


-- |
slope :: RealFloat f => Line (Vector2D f) -> Maybe f
slope (Line fr to)
  | dx == 0   = Nothing
  | otherwise = Just $ dy/dx
  where
    (Vector2D dx dy) = to - fr


-- |
intercept :: RealFloat f => Line (Vector2D f) -> Maybe f
intercept line = do
  slope' <- slope line
  return $ y' - slope'*x'
  where
    (x', y') = (line^.begin.x, line^.begin.y)

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Type for distinguishing inclusive and exclusive values
between :: Ord a => a -> a -> a -> Bool
between mini maxi a = mini <= a && a <= maxi


-- | Ensures that a given point lies within the domain and codomain
-- TODO: Let this function work on scalars, write another function for domain and codomain (?)
-- restrict domain codomain p = _
restrict :: (Num f, Ord f) => Vector2D f -> Vector2D f -> Vector2D f -> Maybe (Vector2D f)
restrict a b p@(Vector2D x y)
  | indomain && incodomain = Just p
  | otherwise              = Nothing
  where
    (Vector2D lowx lowy)   = dotwise min a b
    (Vector2D highx highy) = dotwise max a b
    indomain   = between lowx highx x
    incodomain = between lowy highy y

-- Geometry --------------------------------------------------------------------------------------------------------------------------------

-- |
-- inside :: (Num n, Ord n) => Triangle n -> Point n -> Bool
-- inside _ _ = False


-- |
-- intersects :: RealFrac r => Line r -> Line r -> Bool
-- intersects a b = case intersect a b of
--   Just _  -> True
--   Nothing -> False


-- -- | Yields the overlap of two closed intervals (n ∈ R)
-- -- TODO: Normalise intervals (eg. (12, 5) -> (5, 12))
-- overlap :: Real a => (a, a) -> (a, a) -> Maybe (a, a)
-- overlap a b
--   | leftmost /= (α, β) = Just (β, γ) --
--   | otherwise          = Nothing     --
--   where
--     [α, β, γ, _] = sort [fst a, snd a, fst b, snd b] -- That's right.
--     leftmost     = minimumBy (comparing fst) [a, b]  --


-- |
-- TODO: Intersect Rectangles



-- | Coefficients for the linear function of a Line (slope, intercept).
-- Fails for vertical and horizontal lines.
--
-- TODO: Use Maybe (?)
-- TODO: Rename (eg. toLinear, function) (?)
--
-- coefficients :: (Fractional a, Eq a) => Line a -> Maybe (a, a)
-- coefficients (Line (Vector ax ay) (Vector bx by)) = do
-- 	when (ax == bx) Nothing
-- 	when (ay == ay) Nothing
-- 	let slope' = (by - ay)/(bx - ax) in Just (slope', ay - slope'*ax)

-- Linear functions ------------------------------------------------------------------------------------------------------------------------

-- | Solves a linear equation for x (f(x) = g(x))
-- TODO: Use Epsilon (?)
-- solve :: (Fractional n, Eq n) => Linear n -> Linear n -> Maybe n
-- solve f g
--   | slope f == slope g = Nothing
--   | otherwise          = Just $ (intercept f - intercept g)/(slope f - slope g)
