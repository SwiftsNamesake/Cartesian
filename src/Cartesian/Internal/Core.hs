-- |
-- Module      : Cartesian.Internal.Core
-- Description : Defines basic functionality that is shared between Plane and Space
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created October 31 2015

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
module Cartesian.Internal.Core where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.List (sort)
import Control.Lens ((%~))

import Cartesian.Internal.Types
import Cartesian.Internal.Lenses



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------

-- | Finds the overlap between two ranges (lower bound, upper bound).
overlap :: (Ord n) => (n, n) -> (n, n) -> Maybe (n, n)
overlap (a, b) (c, d)
  | min (a, b) (c, d) /= (a', b') = Just (b', c')
  | otherwise                     = Nothing
  where [a', b', c', d'] = sort [a, b, c, d]


-- | Applies a function to each component in a vector
-- dotmap :: (a -> b) -> Complex a -> Complex b
-- dotmap f (re:+im) = f re :+ f im


-- |
-- dotwise :: (a -> a -> b) -> Complex a -> Complex a -> Complex b
-- dotwise f (re:+im) (re':+im') = f re re':+ f im im'
