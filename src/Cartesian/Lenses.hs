-- |
-- Module      : Cartesian.Lenses
-- Description : Exports public lenses
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created September 24 2016

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
module Cartesian.Lenses (
  pinnedAxis, pinned,
  axis, axes, extents, side,
  corner, size,
  begin, end,
  width, height, depth,
  left, right, bottom, top, front, back,
  centre,
  x, y, z
) where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Cartesian.Internal.Types
import Cartesian.Internal.Lenses
