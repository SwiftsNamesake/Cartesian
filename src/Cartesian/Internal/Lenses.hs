-- |
-- Module      : Cartesian.Internal.Lenses
-- Description :
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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE RankNTypes            #-}



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Cartesian.Internal.Lenses where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.List (isSuffixOf)
import Control.Lens

-- import Language.Haskell.TH

import Cartesian.Internal.Types



--------------------------------------------------------------------------------------------------------------------------------------------
-- Lenses
--------------------------------------------------------------------------------------------------------------------------------------------

-- Vector ----------------------------------------------------------------------------------------------------------------------------------

-- | Focus on the X component
x :: HasX v f => Lens v v f f
x = lens getX setX


-- | Focus on the Y component
y :: HasY v f => Lens v v f f
y = lens getY setY


-- | Focus on the Z component
z :: HasZ v f => Lens v v f f
z = lens getZ setZ

-- BoundingBox -----------------------------------------------------------------------------------------------------------------------------

-- TODO: Relative lenses (eg. padding)
-- TODO: Validate (eg. make sure that left < right)
-- TODO: Type-changing lenses (?)


makeLensesWith ''BoundingBox (lensRules & lensField .~ stripSuffix "Of") -- TODO: 'Of'
  where
    stripSuffix su xs
      | su `isSuffixOf` field = Just $ reverse . drop (length suffix) . reverse $ xs
      | otherwise             = Nothing


-- |
-- TODO: Rename (?)
offset :: (Fractional f) => (Getter v f) -> (f -> f -> f) -> BoundingBox v -> f
offset axis towards box = towards (box^.centre.c) (0.5 * box^.size.c)

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- pad :: f -> (Getter v f) -> f -> BoundingBox v
-- pad by axis direction = _


-- | Moves one side of a BoundingBox along the given 'axis' so that its new position is at 'to'.
--  The 'towards' parameter is expected to be either (-) and (+), indicating which side along
--  the axis we're dealing with.
-- TODO: Turn this into a lens function (?)
-- TODO: Polish description
side :: (Fractional f) => Getter v f -> (f -> f -> f) -> Lens (BoundingBox v) (BoundingBox v) f f
side axis towards = lens get set
  where
    get = offset axis towards
    set box to = BoundingBox { _size=s.axis .~ abs (to - towards centre' (-s*0.5)), _centre=c.axis .~ _ }
    centre' = box^.centre.axis --
    size'   = box^.size.axis   --

--------------------------------------------------------------------------------------------------------------------------------------------

width :: (HasX v f) => Lens (BoundingBox v) (BoundingBox v) f f
width = size.x


height :: (HasY v f) => Lens (BoundingBox v) (BoundingBox v) f f
height = size.y


depth :: (HasZ v f) => Lens (BoundingBox v) (BoundingBox v) f f
depth = size.z

-- So much boilerplate it makes me cry -----------------------------------------------------------------------------------------------------

-- type SideLens = (Fractional f, HasX v f) => Lens (BoundingBox v) (BoundingBox v) f f

left :: (Fractional f, HasX v f) => Lens (BoundingBox v) (BoundingBox v) f f
left = side x (-)


right :: (Fractional f, HasX v f) => Lens (BoundingBox v) (BoundingBox v) f f
right = side x (+)


bottom :: (Fractional f, HasY v f) => Lens (BoundingBox v) (BoundingBox v) f f
bottom = side y (-)


top :: (Fractional f, HasY v f) => Lens (BoundingBox v) (BoundingBox v) f f
top = side y (+)


front :: (Fractional f, HasZ v f) => Lens (BoundingBox v) (BoundingBox v) f f
front = side z (-)


back :: (Fractional f, HasZ v f) => Lens (BoundingBox v) (BoundingBox v) f f
back = side z (+)
