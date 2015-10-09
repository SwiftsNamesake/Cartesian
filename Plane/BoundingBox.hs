-- |
-- Module      : Southpaw.Cartesian.Plane.BoundingBox
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created September 7 2015

-- TODO | - Corner lenses
--        -

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE RankNTypes #-}



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Southpaw.Cartesian.Plane.BoundingBox (module Southpaw.Cartesian.Plane.Types,
                                             module Southpaw.Cartesian.Plane.BoundingBox) where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.Complex
import Data.Functor ((<$>))
import Control.Lens

import Southpaw.Cartesian.Plane.Types
import Southpaw.Cartesian.Plane.Utilities



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------
-- Bounding boxes --------------------------------------------------------------------------------------------------------------------------
-- |
-- TODO: Better name (?)
-- TODO: Don't make assumptions about WHICH corners they are (✓)
fromCorners :: RealFloat f => Complex f -> Complex f -> BoundingBox f
fromCorners nw@(n:+w) se@(s:+e) = let size = dotmap abs (se-nw) in BoundingBox { _centre=dotwise min nw se+size*(0.5:+0.0), _size=size }


-- | Creates a bounding box from a topleft and size vector.
fromCornerAndSize :: RealFloat f => Complex f -> Complex f -> BoundingBox f
fromCornerAndSize nw size' = BoundingBox { _centre=nw+size'*0.5, _size=size' }


-- | Top Left Bottom Right
fromSides :: RealFloat f => f -> f -> f -> f -> BoundingBox f
fromSides top left bottom right = fromCorners (left:+top) (right:+bottom)


-- Lenses ----------------------------------------------------------------------------------------------------------------------------------
-- |
-- TODO: Make sure invariants remain true (eg. left < right)
-- TODO: Make coordinate-system independent (eg. direction of axes)
makeBoundingBoxSideLens :: RealFloat f => (BoundingBox f -> f) -> (BoundingBox f -> f -> (f, f, f, f)) -> Lens (BoundingBox f) (BoundingBox f) f f
makeBoundingBoxSideLens oldside newsides f s@(BoundingBox { _centre=(cx:+cy), _size=(dx:+dy) }) = assemble <$> f (oldside s)
  where
    assemble newside = let (nleft, nright, ntop, nbottom) = newsides s newside
                           newsize                        = (nright-nleft):+(nbottom-ntop)
                       in BoundingBox { _centre=(nleft:+ntop)+(newsize*(0.5:+0.0)), _size=newsize }


-- Core ------------------------------------------------------------------------------------------------------------------------------------
-- |
centre :: RealFloat f => Lens (BoundingBox f) (BoundingBox f) (Complex f) (Complex f)
centre f s = let assemble new = s { _centre=new } in assemble <$> f (_centre s)


-- |
size :: RealFloat f => Lens (BoundingBox f) (BoundingBox f) (Complex f) (Complex f)
size f s = let assemble new = s { _size=new } in assemble <$> f (_size s)


-- Sides (absolute) ------------------------------------------------------------------------------------------------------------------------
-- |
left :: RealFloat f => Lens (BoundingBox f) (BoundingBox f) f f
left = makeBoundingBoxSideLens
         (\(BoundingBox { _centre=cx:+_,  _size=dx:+_  }) -> cx - dx/2)
         (\(BoundingBox { _centre=cx:+cy, _size=dx:+dy }) newside -> (newside, cx+dx/2, cy-dy/2, cy+dy/2))


-- |
right :: RealFloat f => Lens (BoundingBox f) (BoundingBox f) f f
right = makeBoundingBoxSideLens
          (\(BoundingBox { _centre=cx:+_,  _size=dx:+_  })         -> cx + dx/2)
          (\(BoundingBox { _centre=cx:+cy, _size=dx:+dy }) newside -> (cx-dx/2, newside, cy-dy/2, cy+dy/2))


-- |
top :: RealFloat f => Lens (BoundingBox f) (BoundingBox f) f f
top = makeBoundingBoxSideLens
        (\(BoundingBox { _centre=_:+cy,  _size=_:+dy  }) -> cy - dy/2)
        (\(BoundingBox { _centre=cx:+cy, _size=dx:+dy }) newside -> (cx-dx/2, cx+dx/2, newside, cy+dy/2))


-- |
bottom :: RealFloat f => Lens (BoundingBox f) (BoundingBox f) f f
bottom = makeBoundingBoxSideLens
           (\(BoundingBox { _centre=_:+cy,  _size=_:+dy  }) -> cy + dy/2)
           (\(BoundingBox { _centre=cx:+cy, _size=dx:+dy }) newside -> (cx-dx/2, cx+dx/2, cy-dy/2, newside))


-- Sides (relative) ------------------------------------------------------------------------------------------------------------------------
-- |
leftpad :: RealFloat f => Lens (BoundingBox f) (BoundingBox f) f f
leftpad = makeBoundingBoxSideLens
            (\(BoundingBox { _centre=cx:+_,  _size=dx:+_  }) -> cx - dx/2)
            (\(BoundingBox { _centre=cx:+cy, _size=dx:+dy }) newside -> (cx-dx/2+newside, cx+dx/2, cy-dy/2, cy+dy/2))


-- |
rightpad :: RealFloat f => Lens (BoundingBox f) (BoundingBox f) f f
rightpad = makeBoundingBoxSideLens
             (\(BoundingBox { _centre=cx:+_,  _size=dx:+_  })         -> cx + dx/2)
             (\(BoundingBox { _centre=cx:+cy, _size=dx:+dy }) newside -> (cx-dx/2, cx+dx/2+newside, cy-dy/2, cy+dy/2))


-- |
toppad :: RealFloat f => Lens (BoundingBox f) (BoundingBox f) f f
toppad = makeBoundingBoxSideLens
          (\(BoundingBox { _centre=_:+cy,  _size=_:+dy  }) -> cy - dy/2)
          (\(BoundingBox { _centre=cx:+cy, _size=dx:+dy }) newside -> (cx-dx/2, cx+dx/2, cy-dy/2+newside, cy+dy/2))


-- |
bottompad :: RealFloat f => Lens (BoundingBox f) (BoundingBox f) f f
bottompad = makeBoundingBoxSideLens
              (\(BoundingBox { _centre=_:+cy,  _size=_:+dy  }) -> cy + dy/2)
              (\(BoundingBox { _centre=cx:+cy, _size=dx:+dy }) newside -> (cx-dx/2, cx+dx/2, cy-dy/2, cy+dy/2+newside))
