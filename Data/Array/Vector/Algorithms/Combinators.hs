{-# LANGUAGE Rank2Types, TypeOperators #-}

-- ---------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Vector.Algorithms.Combinators
-- Copyright   : (c) 2008 Dan Doel
-- Maintainer  : Dan Doel <dan.doel@gmail.com>
-- Stability   : Experimental
-- Portability : Non-portable (rank-2 types)
--
-- The purpose of this module is to supply various combinators for commonly
-- used idioms for the algorithms in this package. Examples at the time of
-- this writing include running an algorithm keyed on some function of the
-- elements (but only computing said function once per element), and safely
-- applying the algorithms on mutable arrays to immutable arrays.

module Data.Array.Vector.Algorithms.Combinators ( apply ) where

import Control.Monad.ST

import Data.Ord

import Data.Array.Vector
import Data.Array.Vector.Algorithms.Common

-- | Safely applies a mutable array algorithm to an immutable array.
apply :: (UA e) => (forall s. MUArr e s -> ST s ()) -> UArr e -> UArr e
apply algo v = newU (lengthU v) (\arr -> copyMU arr 0 v >> algo arr)

-- | Uses a function to compute a key for each element which the
-- algorithm should use in lieu of the actual element. For instance:
--
-- > schwartzian sortBy f arr
--
-- should produce the same results as:
--
-- > sortBy (comparing f) arr
--
-- with the schwartzian transform being more efficient (for certain keys), 
-- because each key is computed only once.
schwartzian :: (UA e, UA k, Ord k)
            => (forall e'. Comparison e' -> MUArr e' s -> ST s ())
            -> (e -> k)
            -> MUArr e s
            -> ST s ()
schwartzian algo f arr = do
  keys <- newMU (lengthMU arr)
  fill len keys
  algo (comparing fstS) (unsafeZipMU keys arr)
 where
 len = lengthMU arr
 fill k keys
   | k < 0     = return ()
   | otherwise = readMU arr k >>= writeMU keys k . f >> fill (k-1) keys