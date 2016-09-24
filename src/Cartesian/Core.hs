-- |
-- Module      : Cartesian.Core
-- Description : Exports the core functionality of this package
-- Copyright   : (c) Jonatan H Sundqvist, 2016
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



------------------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
------------------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}



------------------------------------------------------------------------------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------------------------------------------------------------------------------
module Cartesian.Core (
  -- ^ Types
  module Cartesian.Types,
  
  -- ^ Lenses
  module Cartesian.Lenses,

  -- ^ Functions
  overlap, fromCorners, fromAxes, fromExtents, intersect) where



------------------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
------------------------------------------------------------------------------------------------------------------------------------------------------
import Cartesian.Internal.Core

import Cartesian.Types
import Cartesian.Lenses

