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
--        - This shouldn't be a separate module
--        - Use classes (eg. Foldable, Applicative) to implement generically (...)

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
                                    module Cartesian.Plane.Lenses) where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.Complex
import Data.Functor ((<$>))
import Data.List (sort)

import Control.Lens
import Control.Applicative ((<$>), (<*>))

import Cartesian.Internal.Types
import Cartesian.Internal.Lenses
import Cartesian.Internal.Instances
-- import Cartesian.Internal.Core

import Cartesian.Plane.Types
import Cartesian.Plane.Lenses
-- import Cartesian.Plane.Utilities



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------

-- Convenience constructors ----------------------------------------------------------------------------------------------------------------

-- TODO: Generalise constructors, move to Internal.Core

-- | Creates a bounding box from two opposite corners
-- TODO: Better name (?)
-- TODO: Don't make assumptions about WHICH corners they are (✓)
-- TODO: Should we care about degenerate cases (such as 'a' and 'b' being identical)
fromCorners :: (Applicative v, Num n) => v n -> v n -> BoundingBox (v n)
fromCorners a b = BoundingBox { cornerOf = min <$> a <*> b,
                                sizeOf   = abs <$> liftA2 (-) b a }


-- | Top Left Bottom Right
-- fromSides :: RealFloat f => f -> f -> f -> f -> BoundingBox f
-- fromSides top left bottom right = fromCorners (left:+top) (right:+bottom)

-- Booleans --------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: 
intersect :: (Applicative v, Ord f) => BoundingBox (v f) -> BoundingBox (v f) -> Maybe (BoundingBox (v f))
intersect a b = do
  overlaps <- uncurry overlap <$> (zipA (a^.extents) (b^.extents))
  BoundingBox <$> newCorner <*> newSize
  where
    bounds (from, len) = (from, from+len) -- From (begin, length) to (begin, end)
    zipA       = liftA2 (,)
    unzipA v   = (fst <$> v, snd <$> v)

