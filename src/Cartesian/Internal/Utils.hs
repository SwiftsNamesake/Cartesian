-- |
-- Module      : Cartesian.Internal.Utils
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created November 2 2015

-- TODO | -
--        -

-- SPEC | -
--        -



------------------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
------------------------------------------------------------------------------------------------------------------------------------------------------




------------------------------------------------------------------------------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------------------------------------------------------------------------------
module Cartesian.Internal.Utils where



------------------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
------------------------------------------------------------------------------------------------------------------------------------------------------
import Data.List           (isSuffixOf)
import Control.Monad       (mfilter)
import Control.Applicative (liftA2)



------------------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
------------------------------------------------------------------------------------------------------------------------------------------------------

-- | Removes the given suffix if it exists, does nothing otherwise
-- TODO: Move to utils module or use existing implementation
-- TODO: Refactor
dropSuffix :: (Eq a) => [a] -> [a] -> [a]
dropSuffix su xs = maybe xs (take (length xs - length su)) $ mfilter (su `isSuffixOf`) (Just xs)


-- |
-- TODO: Type for distinguishing inclusive and exclusive values
between :: Ord a => a -> a -> a -> Bool
between mini maxi a = mini <= a && a <= maxi

------------------------------------------------------------------------------------------------------------------------------------------------------

zipA :: (Applicative f) => f a -> f b -> f (a, b)
zipA = liftA2 (,)

unzipA :: (Applicative f) => f (a, b) -> (f a, f b)
unzipA v = (fst <$> v, snd <$> v)
