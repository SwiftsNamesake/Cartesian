-- |
-- Module      : Cartesian.Internal.Instances
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2016
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created September 1 2016

-- TODO | - 
--        - 

-- SPEC | -
--        -



------------------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Directives
------------------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}



------------------------------------------------------------------------------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------------------------------------------------------------------------------
module Cartesian.Internal.Instances where



------------------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
------------------------------------------------------------------------------------------------------------------------------------------------------
import Cartesian.Internal.Core



------------------------------------------------------------------------------------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------------------------------------------------------------------------------------

-- This instance has caused me A LOT of headaches (overlapping...)
-- |
-- instance (Vector v, Floating f) => Num (v f) where
--   -- TODO: Helper method to reduce boilerplate for component-wise operations
--   (+) = dotwise (+) --
--   (-) = dotwise (-) --
--   -- A × B = ||A|| ||B|| sin angle n.
--   -- (*) a b     = mag a * mag b -- TODO: Is this really correct?
--   (*) a b = error "Vector multiplication is still a work in progress."
--   fromInteger = fromScalar . fromInteger             --
--   negate      = dotmap negate
--   signum v    = dotmap signum v        -- TODO: Proper way of implementing this function for vectors
--   abs         = fromScalar . magnitude --