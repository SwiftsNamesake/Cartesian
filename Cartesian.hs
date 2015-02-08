--
-- Cartesian.hs
-- This module exports the API for the Cartesian project
--
-- Jonatan H Sundqvist
-- January 27 2015
--

-- TODO | - Haddock header, sections, full coverage
--        -

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
	(*) = dotWise (*)
	fromInteger n = Vector (fromInteger n) 0 0
	signum = id -- TODO: Proper way of implementing this function for vectors
	-- abs a = Vector (euclidean a) 0 0



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



---------------------------------------------------------------------------------------------------
-- Entry point
---------------------------------------------------------------------------------------------------
main :: IO ()
main = do
	putStrLn "Hello World"
	putStrLn "Hola Mundo!"