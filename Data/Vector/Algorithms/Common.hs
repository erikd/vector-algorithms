-- ---------------------------------------------------------------------------
-- |
-- Module      : Data.Vector.Algorithms.Common
-- Copyright   : (c) 2008-2010 Dan Doel
-- Maintainer  : Dan Doel
-- Stability   : Experimental
-- Portability : Portable
--
-- Common operations and utility functions for all sorts

module Data.Vector.Algorithms.Common where

import Prelude hiding (read)

import Control.Monad.Primitive

import Data.Vector.Generic.Mutable

-- | A type of comparisons between two values of a given type.
type Comparison e = e -> e -> Ordering

-- | Swaps the elements at two positions in an array.
swap :: (PrimMonad m, MVector v e) => v (PrimState m) e -> Int -> Int -> m ()
swap arr i j = do ei <- read arr i
                  read arr j >>= write arr i
                  write arr j ei
{-# INLINE swap #-}

{-
mcopyMU :: (UA e) => MUArr e s -> MUArr e s -> Int -> Int -> Int -> ST s ()
mcopyMU from to iFrom iTo len = go 0
 where
 go n | n < len   = readMU from (iFrom + n) >>= writeMU to (iTo + n) >> go (n+1)
      | otherwise = return ()
{-# INLINE mcopyMU #-}
-}
