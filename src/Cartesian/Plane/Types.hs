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
--        - Use V2 instead (would reduce boilerplate and the hassle of conversions)
--        - Strictness annotations

-- SPEC | -
--        -



------------------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Directives
------------------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}



------------------------------------------------------------------------------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------------------------------------------------------------------------------
module Cartesian.Plane.Types (module Cartesian.Plane.Types,
                              module Cartesian.Internal.Types) where



------------------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
------------------------------------------------------------------------------------------------------------------------------------------------------
import Data.Complex
import Control.Lens

import Linear.V2

import Cartesian.Internal.Types
import Cartesian.Space.Types



------------------------------------------------------------------------------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- type Domain


-- |
-- TODO: Rename (?)
data Vector2D f = Vector2D f f deriving (Eq, Show) -- TODO: Constraints on argument types (cf. GADT) (?)


-- |
-- TODO: Rename (eg. 'Shape') (?)
type Polygon f = [Vector2D f]


-- |
data Linear f = Linear { interceptOf :: f, slopeOf :: f } deriving (Show, Eq)


-- |
-- TODO: Use existing type instead (?)
-- data Side = SideLeft | SideRight | SideTop | SideBottom

-- Instances -------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Refactor. A lot.
instance Vector Vector2D where
  fromScalar s = Vector2D s 0
  vfold f a (Vector2D x' y')                    = f (f a x') y'
  vzip  f   (Vector2D x' y') (Vector2D x'' y'') = Vector2D (f x' x'') (f y' y'')

instance HasX (Vector2D f) f where
  x = lens
        (\(Vector2D x' _)     -> x')
        (\(Vector2D _  y') x' -> Vector2D x' y')

instance HasY (Vector2D f) f where
  y = lens
        (\(Vector2D _  y')   -> y')
        (\(Vector2D x' _) y' -> Vector2D x' y')

------------------------------------------------------------------------------------------------------------------------------------------------------

instance Functor Vector2D where
  fmap f (Vector2D x y) = Vector2D (f x) (f y) 


instance Applicative Vector2D where
  pure a = Vector2D a a
  Vector2D f g <*> Vector2D x y = Vector2D (f x) (g y)

------------------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Refactor. A lot.
instance Vector V2 where
  fromScalar s = V2 s 0
  vfold f a (V2 x' y')              = f (f a x') y'
  vzip  f   (V2 x' y') (V2 x'' y'') = V2 (f x' x'') (f y' y'')

instance HasX (V2 f) f where
  x = lens
        (\(V2 x' _)     -> x')
        (\(V2 _  y') x' -> V2 x' y')

instance HasY (V2 f) f where
  y = lens
        (\(V2 _  y')   -> y')
        (\(V2 x' _) y' -> V2 x' y')
