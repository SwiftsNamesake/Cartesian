-- |
-- Module      : Cartesian.Space.Types
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created October 31 2015

-- TODO | - Use Linear.V3 instead of defining my own vector type (?)
--        -

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Cartesian.Space.Types (module Cartesian.Space.Types,
                              BoundingBox(..)) where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Control.Lens

import Linear.V3

import Cartesian.Internal.Types
import Cartesian.Internal.Lenses
import Cartesian.Internal.Core



--------------------------------------------------------------------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------------------------------------------------------------------

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
data Vector3D f = Vector3D f f f deriving (Show, Eq) -- TODO: Constraints on argument types (cf. GADT) (?)

-- Instances -------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Refactor. A lot.
instance Vector Vector3D where
  fromScalar s = Vector3D s 0 0
  vfold f a (Vector3D x' y' z')                        = f (f (f a x') y') z'
  vzip  f   (Vector3D x' y' z') (Vector3D x'' y'' z'') = Vector3D (f x' x'') (f y' y'') (f z' z'')


-- |
-- instance (Floating v, Eq v) => Num (Vector3D v) where
--   -- TODO: Helper method to reduce boilerplate for component-wise operations
--   (+) = dotwise (+)
--   (-) = dotwise (-)
--   (*) (Vector3D x y z) (Vector3D x' y' z') = undefined -- TODO: Is this really correct?
--   fromInteger x = Vector3D (fromInteger x) 0 0
--   signum v = dotmap (/mag v) v -- TODO: Proper way of implementing this function for vectors
--   abs    (Vector3D x' y' z') = Vector3D (sqrt $ (x'**2) + (y'**2) + (z'**2)) (0) (0)

--------------------------------------------------------------------------------------------------------------------------------------------

instance HasX (Vector3D f) f where
  x = lens (\(Vector3D x' _ _) -> x') (\(Vector3D _ y' z') x' -> Vector3D x' y' z')

instance HasY (Vector3D f) f where
  y = lens (\(Vector3D _ y' _) -> y') (\(Vector3D x' _ z') y' -> Vector3D x' y' z')

instance HasZ (Vector3D f) f where
  z = lens (\(Vector3D _ _ z') -> z') (\(Vector3D x' y' _) z' -> Vector3D x' y' z')

--------------------------------------------------------------------------------------------------------------------------------------------

instance HasX (V3 f) f where
  x = lens (\(V3 x' _ _) -> x') (\(V3 _ y' z') x' -> V3 x' y' z')

instance HasY (V3 f) f where
  y = lens (\(V3 _ y' _) -> y') (\(V3 x' _ z') y' -> V3 x' y' z')

instance HasZ (V3 f) f where
  z = lens (\(V3 _ _ z') -> z') (\(V3 x' y' _) z' -> V3 x' y' z')
