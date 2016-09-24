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

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes             #-}


--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Cartesian.Internal.Types where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Control.Lens (Lens)



--------------------------------------------------------------------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------------------------------------------------------------------

-- Synonyms --------------------------------------------------------------------------------------------------------------------------------

-- TODO: Add some aliased lenses for these aliased types (?)

-- | A lens focusing on a single [vector-]component in a BoundingBox
type BoxLens v v' f f' = Lens (BoundingBox (v f)) (BoundingBox (v' f')) f f'


-- | An axis represented as (begin, length)
type Axis a = (a, a)


-- | A vector where each component represents a single axis (cf. 'Axis')
type Axes v a = v (Axis a)


-- | Coordinate system wrappers
newtype Normalised v = Normalised { absolute   :: v } -- 
newtype Absolute   v = Absoloute  { normalised :: v } -- 

-- Types -----------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Anchors (eg. C, N, S, E W and combinations thereof, perhaps represented as relative Vectors)
-- TODO: Define some standard instances (eg. Functor, Applicative)
data BoundingBox v = BoundingBox { cornerOf :: v, sizeOf :: v } deriving (Show, Eq)


-- |
-- TODO: Use record (eg. from, to) (?)
data Line v = Line v v deriving (Show, Eq)

-- Classes ---------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Use GADT instead (?)
-- TODO: Reduce boilerplate, figure out deriving, choose interface carefully
-- TODO: Figure out how to deal with parameter (fromScalar requires a Num constraint on f, maybe use 'subclass')
class Vector v where
  fromScalar :: Num f => f -> v f
  vfold :: (f' -> f  -> f')  -> f'  -> v f  -> f'
  vzip  :: (f  -> f' -> f'') -> v f -> v f' -> v f''


class HasX a f | a -> f where { x :: Lens a a f f }
class HasY a f | a -> f where { y :: Lens a a f f }
class HasZ a f | a -> f where { z :: Lens a a f f }
