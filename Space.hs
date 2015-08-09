-----------------------------------------------------------------------------
-- |
-- Module      :  Southpaw.Cartesian.Space
-- Copyright   :  (C) 2015 Jonatan H Sundqvist
-- License     :  MIT-style (see the file LICENSE)
-- Maintainer  :  Jonatan H Sundqvist <jonatanhsundqvist@gmail.com>
-- Stability   :  provisional
-- Portability :  Portable
--
-- Vector and coordinate system utilities.
-----------------------------------------------------------------------------

--
-- Cartesian.hs
-- This module exports the API for the Cartesian project
--
-- Jonatan H Sundqvist
-- January 27 2015
--

-- TODO | - Haddock header, sections, full coverage
--        - Separate 2D and 3D modules (✓)
--        - Factor out common functionality for Space.hs and Plane.hs

-- SPEC | -
--        -



module Southpaw.Cartesian.Space where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import Data.List (sort, minimumBy)
import Data.Ord  (comparing)



---------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------
-- |
data Vector num = Vector num num num -- TODO: Constraints on argument types (cf. GADT) (?)


-- |
data Line num = Line (Vector num) (Vector num) 


-- | Why the hell did I write this useless function?
vector :: Num a => a -> a -> a -> Vector a
vector = Vector -- SublimeHaskell prefers the eta-reduced version (point-free)



---------------------------------------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------------------------------------
instance (Floating a, Eq a) => Num (Vector a) where
	-- TODO: Helper method to reduce boilerplate for component-wise operations
	(+) = dotWise (+)
	(-) = dotWise (-)
	(*) = dotWise (*) -- TODO: Is this really correct?
	fromInteger n = Vector (fromInteger n) 0 0
	signum v@(Vector x y z) = Vector (x/mag v) (y/mag v) (z/mag v) -- TODO: Proper way of implementing this function for vectors
	abs v                   = Vector (mag v)   (0)       (0)



---------------------------------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------------------------------
-- Vector math ------------------------------------------------------------------------------------
-- | Performs component-wise operations
dotWise :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
dotWise f (Vector x y z) (Vector x' y' z') = Vector (f x x') (f y y') (f z z')


-- | Dot product of two vectors
dot :: Floating a => Vector a -> Vector a -> a
dot (Vector x y z) (Vector x' y' z') = (x * x') + (y * y') + (z * z') -- TODO: Refactor with Num instance (?)


-- | Euclidean distance between two points
euclidean :: Floating a => Vector a -> Vector a -> a
euclidean a b = sqrt $ dot a b


-- |
magnitude :: (Floating a, Eq a) => Vector a -> a
magnitude v = euclidean v v

mag :: (Floating a, Eq a) => Vector a -> a
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


---------------------------------------------------------------------------------------------------

-- | Intersect
-- TODO: Math notes, MathJax or LaTex
-- TODO: Intersect for curves (functions) and single points (?)
-- TODO: Polymorphic, typeclass (lines, shapes, ranges, etc.) (?)
intersect :: Num a => Line a -> Line a -> Maybe (Vector a) 
intersect _ _ = error "Not implemented" -- Nothing


-- |
intersects :: Num a => Line a -> Line a -> Bool 
intersects a b = case intersect a b of
	Just _  -> True
	Nothing -> False


-- | Yields the overlap of two closed intervals (n ∈ R)
-- TODO: Normalise intervals (eg. (12, 5) -> (5, 12))
overlap :: Real a => (a, a) -> (a, a) -> Maybe (a, a)
overlap a b
	| leftmost /= (α, β) = Just $ (β, γ) --
	| otherwise                                  = Nothing --
	where [α, β, γ, _] = sort [fst a, snd a, fst b, snd b] -- That's right.
	      leftmost     = minimumBy (comparing fst) [a, b]  --


-- |
-- TODO: Intersect Rectangles



-- | Coefficients for the linear function of a Line (slope, intercept). The Z-component is ignored.
-- Fails for vertical and horizontal lines.
--
-- TODO: Use Maybe (?)
--
coefficients :: (Fractional a, Eq a) => Line a -> Maybe (a, a)
coefficients (Line (Vector ax ay _) (Vector bx by _))
	| ax == bx  = Nothing
	| ay == ay  = Nothing
	| otherwise = let slope = (by - ay)/(bx - ax) in Just (slope, ay - slope*ax)



---------------------------------------------------------------------------------------------------
-- Entry point
---------------------------------------------------------------------------------------------------
main :: IO ()
main = do
	putStrLn "Hello World"
	putStrLn "Hola Mundo!"