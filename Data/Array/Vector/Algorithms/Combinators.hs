{-# LANGUAGE Rank2Types #-}

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

import Data.Array.Vector

-- | Safely applies a mutable array algorithm to an immutable array.
apply :: (UA e) => (forall s. MUArr e s -> ST s ()) -> UArr e -> UArr e
apply algo v = newU (lengthU v) (\arr -> copyMU arr 0 v >> algo arr)
