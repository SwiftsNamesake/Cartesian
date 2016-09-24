-- |
-- Module      : Cartesian.Internal.Types
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created October 31 2015

-- TODO | - Use TemplateHaskell (?)
--        - Strictness
--        - Performance, inlining

-- SPEC | -
--        -



------------------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
------------------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE FlexibleInstances      #-}


------------------------------------------------------------------------------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------------------------------------------------------------------------------
module Cartesian.Internal.Types where



------------------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
------------------------------------------------------------------------------------------------------------------------------------------------------
import Control.Lens (Simple, Lens, lens)

import Data.Complex (Complex(..))

import Linear.V1
import Linear.V2
import Linear.V3
import Linear.V4



------------------------------------------------------------------------------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------------------------------------------------------------------------------

-- Synonyms ------------------------------------------------------------------------------------------------------------------------------------------

-- TODO: Add some aliased lenses for these aliased types (?)

-- | A lens focusing on a single [vector-]component in a BoundingBox
type BoxLens v v' f f' = Lens (BoundingBox (v f)) (BoundingBox (v' f')) f f'


-- | An axis represented as (begin, length)
type Axis a = (a, a)


-- | A vector where each component represents a single axis (cf. 'Axis')
type Axes v a = v (Axis a)


-- |
-- type Domain


-- |
-- TODO: Rename (eg. 'Shape') (?)
type Polygon m v f = m (v f)


-- | Coordinate system wrappers
newtype Normalised v = Normalised { absolute   :: v } -- 
newtype Absolute   v = Absoloute  { normalised :: v } -- 

-- Types ---------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Anchors (eg. C, N, S, E W and combinations thereof, perhaps represented as relative Vectors)
-- TODO: Define some standard instances (eg. Functor, Applicative)
data BoundingBox v = BoundingBox { cornerOf :: v, sizeOf :: v } deriving (Show, Eq)


-- |
-- TODO: Use record (eg. from, to) (?)
data Line v = Line v v deriving (Show, Eq)


-- |
data Linear f = Linear { interceptOf :: f, slopeOf :: f } deriving (Show, Eq)


-- |
data Inclusivity r = Inclusive r | Exclusive r -- TODO: Rename (?)
data Interval r    = Interval (Inclusivity r) (Inclusivity r)


-- |
-- TODO: Use existing type instead (?)
-- data Side = SideLeft | SideRight | SideTop | SideBottom

-- Classes -------------------------------------------------------------------------------------------------------------------------------------------

-- TODO: How do you generate lenses for non-record types (?)
class HasX a f | a -> f where { x :: Simple Lens a f }
class HasY a f | a -> f where { y :: Simple Lens a f }
class HasZ a f | a -> f where { z :: Simple Lens a f }

-- Instances -----------------------------------------------------------------------------------------------------------------------------------------


instance HasX (V1 f) f where
  x = lens (\(V1 x') -> x') (\_ x' -> V1 x')


instance HasX (V2 f) f where
  x = lens (\(V2 x' _) -> x') (\(V2 _ y') x' -> V2 x' y')


instance HasY (V2 f) f where
  y = lens (\(V2 _ y') -> y') (\(V2 x' _) y' -> V2 x' y')


instance HasX (V3 f) f where
  x = lens (\(V3 x' _ _) -> x') (\(V3 _ y' z') x' -> V3 x' y' z')


instance HasY (V3 f) f where
  y = lens (\(V3 _ y' _) -> y') (\(V3 x' _ z') y' -> V3 x' y' z')


instance HasZ (V3 f) f where
  z = lens (\(V3 _ _ z') -> z') (\(V3 x' y' _) z' -> V3 x' y' z')


instance HasX (V4 f) f where
  x = lens (\(V4 x' _ _ _) -> x') (\(V4 _ y' z' w') x' -> V4 x' y' z' w')


instance HasY (V4 f) f where
  y = lens (\(V4 _ y' _ _) -> y') (\(V4 x' _ z' w') y' -> V4 x' y' z' w')


instance HasZ (V4 f) f where
  z = lens (\(V4 _ _ z' _) -> z') (\(V4 x' y' _ w') z' -> V4 x' y' z' w')


instance HasX (Complex f) f where
  x = lens (\(x':+_) -> x') (\(_:+y') x' -> x':+y')


instance HasY (Complex f) f where
  y = lens (\(_':+y') -> y') (\(x':+_) y' -> x':+y')

