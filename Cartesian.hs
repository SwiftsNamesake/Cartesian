--
-- Cartesian.hs
-- This module exports the API for the Cartesian project
--
-- Jonatan H Sundqvist
-- January 27 2015
--

-- TODO | -
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
vector = Vector -- SublimeHaskell prefers the eta-reduces version (point-free)



---------------------------------------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------------------------------------
instance Num a => Num (Vector a) where
	(Vector x y z) + (Vector x' y' z') = Vector (x+x') (y+y') (z+z')



---------------------------------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------------------------------
-- | Dot product of two vectors
dot :: Num a => Vector a -> Vector a -> a
dot (Vector x y z) (Vector x' y' z') = let dx = x' - x
                                                 dy = y' - y
                                                 dz = z' - z
                                             in sqrt $ dx**2 + dy**2 + dz**2

-- | Euclidean distance between two points
euclidean :: Num a => Vector a -> Vector a -> a
euclidean a b = sqrt $ dot a b



---------------------------------------------------------------------------------------------------
-- Entry point
---------------------------------------------------------------------------------------------------
main :: IO ()
main = do
	putStrLn "Hello World"
	putStrLn "Hola Mundo!"