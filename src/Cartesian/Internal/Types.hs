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

-- type SideLens = (Fractional f, HasX v f) => Lens (BoundingBox v) (BoundingBox v) f f
type SideLens v f = Lens (BoundingBox v) (BoundingBox v) f f

-- Types -----------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Anchors (eg. C, N, S, E W and combinations thereof, perhaps represented as relative Vectors)
data BoundingBox v = BoundingBox { centreOf :: v, sizeOf :: v }


-- |
-- TODO: Use record (eg. from, to) (?)
data Line v = Line v v

-- Classes ---------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Use GADT instead (?)
-- TODO: Reduce boilerplate, figure out deriving, choose interface carefully
-- TODO: Figure out how to deal with parameter (fromScalar requires a Num constraint on f, maybe use 'subclass')
class Vector v where
  fromScalar :: Num f => f -> v f
  vfold :: Num f => (f' -> f  -> f')  -> f'  -> v f  -> f'
  vzip  :: Num f => (f  -> f' -> f'') -> v f -> v f' -> v f''


class HasX a f | a -> f where { x :: Lens a a f f }
class HasY a f | a -> f where { y :: Lens a a f f }
class HasZ a f | a -> f where { z :: Lens a a f f }
