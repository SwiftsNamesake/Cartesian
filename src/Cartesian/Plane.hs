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
                        magnitude, dotmap) where



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

--------------------------------------------------------------------------------------------------------------------------------------------

-- | Determines if a point lies within a polygon using the odd-even method.
--
-- TODO: Use epsilon (?)
-- TODO: How to treat points that lie on an edge
inside :: Num n => Polygon n -> Vector2D n -> Bool
inside polygon (Vector2D x y) = error "Cartesian.Plane.inside is still a work in progress"
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
-- _ f = from3D . (dotmap f) . to3D

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

-- Linear functions ------------------------------------------------------------------------------------------------------------------------

-- | Yields the intersection point of two finite lines. The lines are defined inclusively by
--   their endpoints. The result is wrapped in a Maybe value to account for non-intersecting
--   lines.
-- TODO: Refactor
-- TODO: Move equation solving to separate function (two linear functions)
-- TODO: Invariants, check corner cases
-- TODO: Deal with vertical lines
-- TODO: Factor out infinite-line logic
-- TODO: Decide how to deal with identical lines
-- TODO: Factor out domain logic (eg. write restrict or domain function)
-- TODO: Return Either instead of Maybe (eg. Left "parallel") (?)
-- TODO: Visual debugging functions
-- TODO: Math notes, MathJax or LaTex
-- TODO: Intersect for curves (functions) and single points (?)
-- TODO: Polymorphic, typeclass (lines, shapes, ranges, etc.) (?)
-- TODO: Intersect Rectangles
intersect :: RealFloat f => Line (Vector2D f) -> Line (Vector2D f) -> Maybe (Vector2D f)
intersect f' g' = do
  p <- mp
  indomain f' p
  indomain g' p
  where
    -- indomain :: RealFloat f => Line (Vector2D f) -> Vector2D f -> Maybe (Vector2D f)
    indomain h' = restrict (h'^.begin) (h'^.end) -- TODO: Rename

    -- mp :: Maybe (Vector2D f)
    mp = case (linear f', linear g') of
      (Just f, Nothing) -> let x' = g'^.begin.x in Just . Vector2D x' $ plotpoint f x'
      (Nothing, Just g) -> let x' = f'^.begin.x in Just . Vector2D x' $ plotpoint g x'
      (Just f,  Just g) -> linearIntersect f g
      _                 -> Nothing


-- | Gives the linear function overlapping the given segment, or Nothing if there is no such function
linear :: RealFloat f => Line (Vector2D f) -> Maybe (Linear f)
linear line = Linear <$> intercept line <*> slope line


-- | Applies a linear function to the given value
-- TODO: Rename (?)
plotpoint :: RealFloat f => Linear f -> f -> f
plotpoint f x = slopeOf f*x + interceptOf f


-- | Finds the intersection (if any) of two linear functions
-- TODO: Use Epsilon (?)
-- TODO: Rename (eg. 'solve') (?)
linearIntersect :: RealFloat f => Linear f -> Linear f -> Maybe (Vector2D f)
linearIntersect f g
  | slopeOf f == slopeOf g = Nothing
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
    (Vector2D x' y') = line^.begin

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
