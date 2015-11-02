-- |
-- Module      : Cartesian.Plane.Lenses
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created October 21 2015

-- TODO | -
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
module Cartesian.Plane.Lenses where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.Complex
import Data.Functor ((<$>))
import Control.Lens

import Cartesian.Plane.Types



--------------------------------------------------------------------------------------------------------------------------------------------
-- Lenses
--------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Make sure invariants remain true (eg. left < right)
-- TODO: Make coordinate-system independent (eg. direction of axes)
makeBoundingBoxSideLens :: RealFloat f => (BoundingBox f -> f) -> (BoundingBox f -> f -> (f, f, f, f)) -> Lens (BoundingBox f) (BoundingBox f) f f
makeBoundingBoxSideLens oldside newsides f s@(BoundingBox { _centre=(cx:+cy), _size=(dx:+dy) }) = assemble <$> f (oldside s)
  where
    assemble newside = let (nleft, nright, ntop, nbottom) = newsides s newside
                           newsize                        = (nright-nleft):+(nbottom-ntop)
                       in BoundingBox { _centre=(nleft:+ntop)+(newsize*(0.5:+0.0)), _size=newsize }

-- Core lenses -----------------------------------------------------------------------------------------------------------------------------

-- |
centre :: RealFloat f => Lens (BoundingBox f) (BoundingBox f) (Complex f) (Complex f)
centre f s = let assemble new = s { _centre=new } in assemble <$> f (_centre s)


-- |
size :: RealFloat f => Lens (BoundingBox f) (BoundingBox f) (Complex f) (Complex f)
size f s = let assemble new = s { _size=new } in assemble <$> f (_size s)

-- Side lenses (absolute) ------------------------------------------------------------------------------------------------------------------

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

-- Side lenses (relative) ------------------------------------------------------------------------------------------------------------------

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
