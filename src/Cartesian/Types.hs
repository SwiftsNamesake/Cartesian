-- |
-- Module      : Cartesian.Types
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created _ _ 2015

-- TODO | -
--        -

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------




--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Cartesian.Types (
  -- ^ Third party types
  V1(..), V2(..), V3(..), V4(..), Complex(..),
  
  -- ^ Synonyms
  BoxLens, Axis, Axes, Polygon,

  -- ^ Coordinate types
  Normalised, Absolute,

  -- ^ Types defined in this library
  BoundingBox(..), Line, Linear,

  -- ^ Classes
  HasX, HasY, HasZ) where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Linear.V1
import Linear.V2
import Linear.V3
import Linear.V4

import Data.Complex (Complex(..))

import Cartesian.Internal.Types