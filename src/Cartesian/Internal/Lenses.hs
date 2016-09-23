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



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Cartesian.Internal.Lenses where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Control.Applicative (liftA2, (<$>), (<*>))
import Control.Lens (makeLensesWith, lensRules, lensField, lens,
                     Simple, Lens, Getter,
                     (^.), (.~), (+~), (%~), (&),
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


-- | Like pinned, except it operates on a single axis
-- TODO: Change the type to make it play more nicely with 'pinned' (?)
pinnedAxis :: Num n => n -> Simple Lens (n, n) n
pinnedAxis to = lens get set
  where
    get (begin, len)     = begin + (to * len)
    set (begin, len) new = (new - (to * len), len)


-- | Creates a lens where a pin is placed on a given point ('to'), so that
--   the box can be placed relative to the pin. It is also useful for 
--   retrieving points within the box (such as the centre).
--
-- @
-- let box = BoundingBox { cornerOf = V2 10 24, sizeOf = V2 6 18 }
-- 
-- -- The pin is normalised with respect to the corner and size of the box
-- box^.pinned (V2 0.5 0.5) -- Anchored to the centre
-- > error "ADD RESULTING VALUE HERE"
-- @
--
-- TODO: What's the proper way of 'lifting' lenses (such as 'pinnedAxis'), so they work on multiple fields.
--       This is not mucher better than it used to be when we didn't have the 'pinnedAxis' helper...
--
--       Maybe I should look into Traversable. Also, how do you 'zip' a structure
pinned :: (Applicative v, Num n) => v n -> Simple Lens (BoundingBox (v n)) (v n)
pinned to = undefined -- _.traverse._ to
  -- where
  --   singleAxis f axis to' = f axis (pinnedAxis to')
  --   get box     = pure (singleAxis (^.)) <*> axes box <*> to
  --   set box new = box^.corner .~ _


-- | Focuses on a single axis of the box
axis :: (Applicative v, Num n) => Simple Lens (v n) n -> Simple Lens (BoundingBox (v n)) (n, n)
axis which = lens get set
  where
    get box = (box^.corner.which, box^.size.which)
    set box new = box & corner.which .~ (new^._1)
                      & size.which   .~ (new^._2)


-- | Just a simple little traversal to help me wrap my head around it
-- components :: forall f v a b. (Applicative f, Applicative v) => (a -> f b) -> BoundingBox (v a) -> f (BoundingBox (v b))
-- components f box = let (corner', size') = (box^.corner, box^.size) in BoundingBox <$> (f <$> corner') <*> (f <$> size')


-- |
-- zipped :: (Applicative f, Applicative v, Traversable v) => v a -> ((v a, v a) -> f b)


-- | A lens that - given a transform function 'as' and its inverse 'undo' - focuses on a single vector of axis pairs (begin, length)
--   The transform allows us to customise the representation of each axis.
-- TODO: Pass in a vector so that can actually do something useful...
-- TODO: Can I do this without Traversable (?)
-- TODO: Simplify the signature (with type synonyms)
-- TODO: Maybe it'd be easier to simply compose 'f' argument with 'as' and 'undo'
-- type Traversal s t a b = forall f. Applicative f       => ((a)    -> f (b))    -> s                 -> f (t)
-- axes :: forall f v a b. (Applicative f, Applicative v) => ((a, a) -> f (b, b)) -> BoundingBox (v a) -> f (BoundingBox (v b))
-- axes :: forall f v a b. (Applicative f, Applicative v) => (v (a, a) -> f (v (b, b))) -> BoundingBox (v a) -> f (BoundingBox (v b))
-- type Lens s t a b = forall (f :: * -> *). Functor f => (a -> f b) -> s -> f t
axes :: (Applicative v) => Lens (BoundingBox (v a)) (BoundingBox (v b)) (Axes v a) (Axes v b)
axes f box = uncurry BoundingBox <$> newVecs
  where
    -- 
    -- newAxes :: Functor f => f (v (c, c))
    newAxes  = f $ zipA (box^.corner) (box^.size)
    
    -- We then 'unzip' the vector of pairs to obtain a pair of vectors
    -- newVecs :: Functor f => (v c, v c)
    newVecs = unzipA <$> newAxes

    zipA     = liftA2 (,)
    unzipA v = (fst <$> v, snd <$> v)


-- | 
extents :: (Applicative v, Num a, Num b) => Lens (BoundingBox (v a)) (BoundingBox (v b)) (Axes v a) (Axes v b)
extents f = axes (fmap (fmap unbounds) . f . fmap bounds)
  where
    bounds   (from, len) = (from, from+len) -- From (begin, length) to (begin, end)
    unbounds (from, to)  = (from, to-from) -- From (begin, length) to (begin, end)




-- | Moves one side of a BoundingBox along the given 'axis' so that its new position is at 'to'. The 'step' parameter is expected to be
--   either 0 and 1, indicating which side along the axis we're dealing with.
-- TODO: Turn this into a lens function (?)
-- TODO: Polish description
-- TODO: Loosen constraint on n (✓)
side :: (Applicative v, Num n) => Simple Lens (v n) n -> n -> BoxLens v n
side which step = axis which.pinnedAxis step -- lens get set
  -- where
  --   get box     = box^.axis which.pinnedAxis step
  --   set box new = box & axis which %~ (\(begin, len) -> ()) -- TODO: Refactor. And then refactor some more.

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


height :: (HasY (v f) f) => BoxLens v f
height = size.y


depth :: (HasZ (v f) f) => BoxLens v f
depth = size.z

-- Sides (so much boilerplate it makes me cry) ---------------------------------------------------------------------------------------------

left :: (Applicative v, HasX (v n) n, Num n) => BoxLens v n
left = side x 0


right :: (Applicative v, HasX (v n) n, Num n) => BoxLens v n
right = side x 1


-- NOTE: Y-axis points upwards (cf. README.md)
bottom :: (Applicative v, HasY (v n) n, Num n) => BoxLens v n
bottom = side y 0


-- Note: Y-axis points upwards (cf. README.md)
top :: (Applicative v, HasY (v n) n, Num n) => BoxLens v n
top = side y 1


-- NOTE: Z-axis points inwards (forwards) (cf. README.md)
front :: (Applicative v, HasZ (v n) n, Num n) => BoxLens v n
front = side z 0


-- NOTE: Z-axis points inwards (forwards) (cf. README.md)
back :: (Applicative v, HasZ (v n) n, Num n) => BoxLens v n
back = side z 1

--------------------------------------------------------------------------------------------------------------------------------------------

centre :: (Applicative v, Fractional f) => Simple Lens (BoundingBox (v f)) (v f)
centre = pinned (pure 0.5)
