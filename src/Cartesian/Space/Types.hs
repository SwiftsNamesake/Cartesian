-- |
-- Module      : Cartesian.Space.Types
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created October 31 2015

-- TODO | - Use Linear.V3 instead of defining my own vector type (?)
--        -

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------




--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Cartesian.Space.Types where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------




--------------------------------------------------------------------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------------------------------------------------------------------

-- |
data Vector3D f = Vector f f f -- TODO: Constraints on argument types (cf. GADT) (?)


-- |
data Line f = Line (Vector f) (Vector f)


-- |
data BoundingBox f = BoundingBox { _centre :: Vector f, _size :: Vector f }



-- Instances -------------------------------------------------------------------------------------------------------------------------------

-- |
instance (Floating a, Eq a) => Num (Vector a) where
	-- TODO: Helper method to reduce boilerplate for component-wise operations
	(+) = dotwise (+)
	(-) = dotwise (-)
	(*) (Vector x y z) (Vector x' y' z') = Vector (x*) -- TODO: Is this really correct?
	fromInteger n = Vector (fromInteger n) 0 0
	signum v@(Vector x y z) = Vector (x/mag v) (y/mag v) (z/mag v) -- TODO: Proper way of implementing this function for vectors
	abs v                   = Vector (mag v)   (0)       (0)
