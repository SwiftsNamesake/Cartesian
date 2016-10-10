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

-- TODO | - QuickCheck, performance (inlining?)
--        - Use classes for each lens (to avoid naming conflicts) (?)

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE TupleSections          #-}



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Cartesian.Internal.Lenses where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Control.Lens (makeLensesWith, lensRules, lensField, lens,
                     Simple, Lens,
                     (^.), (.~), (&),
                     _1, _2,
                     DefName(TopName))

import Language.Haskell.TH

-- import Cartesian.Internal.Core
import Cartesian.Internal.Types
import Cartesian.Internal.Utils



--------------------------------------------------------------------------------------------------------------------------------------------
-- Lenses
--------------------------------------------------------------------------------------------------------------------------------------------

-- Vector ----------------------------------------------------------------------------------------------------------------------------------

-- BoundingBox -----------------------------------------------------------------------------------------------------------------------------

-- TODO: Relative lenses (eg. padding)
-- TODO: Validate (eg. make sure that left < right)
-- TODO: Type-changing lenses (?)


-- | Ugh...
makeLensesWith (lensRules & lensField .~ (\_ _ name -> [TopName (mkName $ dropSuffix "Of" (nameBase name))])) (''BoundingBox) -- TODO: 'Of'

--------------------------------------------------------------------------------------------------------------------------------------------


-- |
-- pad :: (Getter v f) -> f -> f -> BoundingBox v
-- pad axis by direction = _


-- | Like pinned, except it operates on a single axis and only focuses on the position (not size)
--
-- TODO: Change the type to make it play more nicely with 'pinned' (?)
-- TODO: - What's the proper way of 'lifting' lenses (such as 'pinnedAxis'), so they work on multiple fields.
--         This is not mucher better than it used to be when we didn't have the 'pinnedAxis' helper...
pinnedAxis :: Num n => n -> Simple Lens (Axis n) (Axis n)
pinnedAxis to = lens get set
  where
    get   (begin', len)   = (begin' + to*len, len)
    set _ (begin', len) = (begin' - to*len, len)


-- | Creates a lens where a pin is placed on a given point ('to'), so that
--   the box can be placed or resized relative to the pin. It is also useful for 
--   retrieving points within the box (such as the centre).
--
--   The pin is assumed to be normalised with respect to the corner and size of the box.
--
-- @
-- let box = BoundingBox { cornerOf = V2 10 24, sizeOf = V2 6 18 }
-- 
-- box^.pinned (V2 0.5 0.5) -- Anchored to the centre
-- > V2 (13.0,6.0) (33.0,18.0)
-- @
--
pinned :: (Applicative v, Num n) => v n -> Simple Lens (BoundingBox (v n)) (Axes v n)
pinned to f = axes (fmap undo . f . as) -- _.traverse._ to
  where
    toPinned   (pin, (begin', len)) = (begin' + pin*len, len)
    fromPinned (pin, (begin', len)) = (begin' - pin*len, len)
    
    as   = fmap toPinned . zipA to
    undo = fmap fromPinned . zipA to


-- | Focuses on a single axis of the box
axis :: (Applicative v, Num n) => Simple Lens (Axes v n) (Axis n) -> Simple Lens (BoundingBox (v n)) (Axis n)
axis which = axes.which
  -- where
  --   get box = (box^.corner.which, box^.size.which)
  --   set box new = box & corner.which .~ (new^._1)
  --                     & size.which   .~ (new^._2)


-- | 
axes :: (Applicative v) => Lens (BoundingBox (v a)) (BoundingBox (v b)) (Axes v a) (Axes v b)
axes f box = uncurry BoundingBox <$> newVecs
  where
    newAxes = f $ zipA (box^.corner) (box^.size)
    newVecs = unzipA <$> newAxes


-- | 
extents :: (Applicative v, Num a, Num b) => Lens (BoundingBox (v a)) (BoundingBox (v b)) (Axes v a) (Axes v b)
extents f = axes (fmap (fmap unbounds) . f . fmap bounds)
  where
    bounds   (from, len) = (from, from+len) -- From (begin, length) to (begin, end)
    unbounds (from, to)  = (from, to-from) -- From (begin, length) to (begin, end)


-- | 
-- TODO: Turn this into a lens function (?)
-- TODO: Polish description
-- TODO: Loosen constraint on n (✓)
-- axes which.pinned (V1 step).x._1 -- lens get set
side :: (Applicative v, Num n) => Simple Lens (Axes v n) (Axis n) -> Simple Lens (Axis n) n -> Simple BoxLens v n
side axis' endpoint' = extents.axis'.endpoint'

-- TODO: sides, vertices

-- Lines -----------------------------------------------------------------------------------------------------------------------------------

-- TODO: Use type class (?)

begin :: Lens (Line v) (Line v) v v
begin = lens (\(Line a _) -> a) (\(Line _ b) a -> Line a b)


end :: Lens (Line v) (Line v) v v
end = lens (\(Line _ b) -> b) (\(Line a _) b -> Line a b)

--------------------------------------------------------------------------------------------------------------------------------------------

width :: (HasX (v f) f) => Simple Lens (BoundingBox (v f)) f
width = size.x


height :: (HasY (v f) f) => Simple BoxLens v f
height = size.y


depth :: (HasZ (v f) f) => Simple BoxLens v f
depth = size.z

-- Sides (so much boilerplate it makes me cry) ---------------------------------------------------------------------------------------------

left :: (Applicative v, HasX (Axes v n) (Axis n), Num n) => Simple BoxLens v n
left = side x _1


right :: (Applicative v, HasX (Axes v n) (Axis n), Num n) => Simple BoxLens v n
right = side x _2


-- NOTE: Y-axis points upwards (cf. README.md)
bottom :: (Applicative v, HasY (Axes v n) (Axis n), Num n) => Simple BoxLens v n
bottom = side y _1


-- Note: Y-axis points upwards (cf. README.md)
top :: (Applicative v, HasY (Axes v n) (Axis n), Num n) => Simple BoxLens v n
top = side y _2


-- NOTE: Z-axis points inwards (forwards) (cf. README.md)
front :: (Applicative v, HasZ (Axes v n) (Axis n), Num n) => Simple BoxLens v n
front = side z _1


-- NOTE: Z-axis points inwards (forwards) (cf. README.md)
back :: (Applicative v, HasZ (Axes v n) (Axis n), Num n) => Simple BoxLens v n
back = side z _2

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: This should probably yield a vector (rename or redesign)
centre :: (Applicative v, Fractional f) => Simple Lens (BoundingBox (v f)) (Axes v f)
centre = pinned (pure $ 1/2)