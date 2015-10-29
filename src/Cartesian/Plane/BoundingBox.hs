-- |
-- Module      : Cartesian.Plane.BoundingBox
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created September 7 2015

-- TODO | - Corner lenses
--        -

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Cartesian.Plane.BoundingBox (module Cartesian.Plane.Types,
                                    module Cartesian.Plane.BoundingBox,
                                    module Cartesian.Plane.BoundingBox.Lenses) where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.Complex
import Data.Functor ((<$>))
import Data.List (sort)

import Control.Lens

import Cartesian.Plane.Types
import Cartesian.Plane.BoundingBox.Lenses
import Cartesian.Plane.Utilities



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------

-- Convenience constructors ----------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Better name (?)
-- TODO: Don't make assumptions about WHICH corners they are (✓)
fromCorners :: RealFloat f => Complex f -> Complex f -> BoundingBox f
fromCorners nw@(n:+w) se@(s:+e) = let size = dotmap abs (se-nw) in BoundingBox { _centre=dotwise min nw se+size*(0.5:+0.0), _size=size }


-- | Creates a bounding box from a topleft and size vector.
fromCornerAndSize :: RealFloat f => Complex f -> Complex f -> BoundingBox f
fromCornerAndSize nw size' = BoundingBox { _centre=nw+size'*0.5, _size=size' }


-- | Top Left Bottom Right
fromSides :: RealFloat f => f -> f -> f -> f -> BoundingBox f
fromSides top left bottom right = fromCorners (left:+top) (right:+bottom)

-- Booleans --------------------------------------------------------------------------------------------------------------------------------

-- |
intersect :: (RealFloat f, Ord f) => BoundingBox f -> BoundingBox f -> Maybe (BoundingBox f)
intersect a b = do
  (left', right')  <- overlap (a^.left, a^.right)  (b^.left, b^.right)
  (top',  bottom') <- overlap (a^.top,  a^.bottom) (b^.top,  b^.bottom)
  return $ fromSides top' left' bottom' right'
  where
    overlap (a, b) (c, d)
      | min (a, b) (c, d) == (a', b') = Just (b', c')
      | otherwise                     = Nothing
      where [a', b', c', d'] = sort [a, b, c, d]
