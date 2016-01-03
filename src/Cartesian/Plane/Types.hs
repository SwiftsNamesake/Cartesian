-- |
-- Module      : Cartesian.Plane.Types
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created September 6 2015

-- TODO | - Rename or move out function definitions
--        - Move BoundingBox functions to separate module (so that you could write BBox.makeFrom...)

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Directives
--------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes      #-}



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Cartesian.Plane.Types (module Cartesian.Plane.Types,
                              module Cartesian.Internal.Types) where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.Complex
import Control.Lens

import Cartesian.Internal.Types
import Cartesian.Space.Types



--------------------------------------------------------------------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- type Domain


-- |
-- TODO: Rename (?)
data Vector2D f = Vector2D f f deriving (Eq, Show) -- TODO: Constraints on argument types (cf. GADT) (?)


-- |
-- TODO: Rename (eg. 'Shape') (?)
type Polygon f = [Vector2D f]


-- |
data Linear f = Linear { intercept :: f, slope :: f }


-- |
-- TODO: Use existing type instead (?)
-- data Side = SideLeft | SideRight | SideTop | SideBottom


instance HasX (Vector2D f) f where
  x = lens (\(Vector2D x' _) -> x') (\(Vector2D _ y') x' -> Vector2D x' y')

instance HasY (Vector2D f) f where
  y = lens (\(Vector2D _ y') -> y') (\(Vector2D x' _) y' -> Vector2D x' y')

-- instance HasZ (Vector2D f) f where
  -- z = lens (const 0) const
