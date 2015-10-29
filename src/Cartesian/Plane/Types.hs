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
module Cartesian.Plane.Types where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.Complex
import Control.Lens



--------------------------------------------------------------------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Anchors (eg. C, N, S, E W and combinations thereof, perhaps represented as relative Vectors)
data BoundingBox f = BoundingBox { _centre :: Complex f, _size :: Complex f } deriving (Show)


-- |
-- TODO: Use existing type instead (?)
-- data Side = SideLeft | SideRight | SideTop | SideBottom
