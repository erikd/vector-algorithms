{-# LANGUAGE Rank2Types #-}

-- ---------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Vector.Algorithms.Immutable
-- Copyright   : (c) 2008 Dan Doel
-- Maintainer  : Dan Doel <dan.doel@gmail.com>
-- Stability   : Experimental
-- Portability : Non-portable (rank-2 types)
--
-- The purpose of this module is to apply the algorithms on mutable arrays
-- in other packages to immutable arrays. The idea is to copy the immutable
-- array into a mutable intermediate, perform the algorithm on the mutable
-- array, and freeze it, yielding a new immutable array.

module Data.Array.Vector.Algorithms.Immutable ( apply ) where

import Control.Monad.ST

import Data.Array.Vector

-- | Safely applies a mutable array algorithm to an immutable array.
apply :: (UA e) => (forall s. MUArr e s -> ST s ()) -> UArr e -> UArr e
apply algo v = newU (lengthU v) (\arr -> copyMU arr 0 v >> algo arr)
