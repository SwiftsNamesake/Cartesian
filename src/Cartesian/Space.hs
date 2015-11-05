-- |
-- Module      :  Cartesian.Space
-- Copyright   :  (C) 2015 Jonatan H Sundqvist
-- License     :  MIT-style (see the file LICENSE)
-- Maintainer  :  Jonatan H Sundqvist <jonatanhsundqvist@gmail.com>
-- Stability   :  provisional
-- Portability :  Portable
--
-- Vector and coordinate system utilities.

--
-- Cartesian.hs
-- This module exports the API for the Cartesian project
--
-- Jonatan H Sundqvist
-- January 27 2015
--

-- TODO | - Haddock header, sections, full coverage
--        - Separate 2D and 3D modules (✓)
--        - Factor out common functionality for Space.hs and Plane.hs
--        - Use existing vector type (eg. Linear.V3)

-- SPEC | -
--        -



module Cartesian.Space where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.List (sort, minimumBy)
import Data.Ord  (comparing)

import Cartesian.Space.Types



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------
