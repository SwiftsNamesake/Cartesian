-----------------------------------------------------------------------------
-- |
-- Module      :  Cartesian
-- Copyright   :  (C) 2015 Edward Kmett, Niklas Haas
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
--        - Separate 2D and 3D modules (?)

-- SPEC | -
--        -



module Cartesian where



---------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------
data Vector num = Vector num num num -- TODO: Constraints on argument types (cf. GADT) (?)
data Line num = Line (Vector num) (Vector num) 


vector :: Num a => a -> a -> a -> Vector a
vector = Vector -- SublimeHaskell prefers the eta-reduced version (point-free)



---------------------------------------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------------------------------------
instance Floating a => Num (Vector a) where
	-- TODO: Helper method to reduce boilerplate for component-wise operations
	(+) = dotWise (+)
	(-) = dotWise (-)
	(*) = dotWise (*) -- TODO: Is this really correct?
	fromInteger n = Vector (fromInteger n) 0 0
	signum = id -- TODO: Proper way of implementing this function for vectors
	-- abs a = Vector (euclidean a) 0 0
	-- abs (Vector x y z) = sqrt $ (x**2) + (y**2) + (z**2)



---------------------------------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------------------------------
-- | Performs component-wise operations
dotWise :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
dotWise f (Vector x y z) (Vector x' y' z') = Vector (f x x') (f y y') (f z z')


-- | Dot product of two vectors
dot :: Floating a => Vector a -> Vector a -> a
dot (Vector x y z) (Vector x' y' z') = (x * x') + (y * y') + (z * z') -- TODO: Refactor with Num instance (?)


-- | Euclidean distance between two points
euclidean :: Floating a => Vector a -> Vector a -> a
euclidean a b = sqrt $ dot a b


-- | Intersect
-- TODO: Math notes, MathJax or LaTex
-- TODO: Intersect for curves and single points (?)
intersect :: Num a => Line a -> Line a => Maybe (Vector a) 
intersect _ _ = Nothing


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