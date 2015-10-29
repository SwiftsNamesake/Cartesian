-- |
-- Module      : Cartesian.Plane.Utilities
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created September 8 2015

-- TODO | - Uses lenses for Complex type (?)
--        -

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Cartesian.Plane.Utilities where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.Complex



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------
-- | Applies a function to each component in a vector
dotmap :: (a -> b) -> Complex a -> Complex b
dotmap f (re:+im) = f re :+ f im


-- |
dotwise :: (a -> a -> b) -> Complex a -> Complex a -> Complex b
dotwise f (re:+im) (re':+im') = f re re':+ f im im'


-- | Negates the real component (X)
flipx :: Complex Double -> Complex Double
flipx (x:+y) = (-x):+y


-- | Negates the imaginary component (Y)
flipy :: Complex Double -> Complex Double
flipy (x:+y) = x:+(-y)


-- | Creates a number on the real line (where the imaginary part is 0)
real :: Double -> Complex Double
real = (:+ 0)


-- | Creates a number on the imaginary line (where the real part is 0)
imag :: Double -> Complex Double
imag = (0 :+)
