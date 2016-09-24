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
