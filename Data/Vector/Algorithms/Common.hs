-- ---------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Vector.Algorithms.Common
-- Copyright   : (c) 2008 Dan Doel
-- Maintainer  : Dan Doel
-- Stability   : Experimental
-- Portability : Portable
--
-- Common operations and utility functions for all sorts

module Data.Array.Vector.Algorithms.Common where

import Control.Monad.ST

import Data.Array.Vector

-- | A type of comparisons between two values of a given type.
type Comparison e = e -> e -> Ordering

-- | Swaps the elements at two positions in an array.
swap :: (UA e) => MUArr e s -> Int -> Int -> ST s ()
swap arr i j = do ei <- readMU arr i
                  readMU arr j >>= writeMU arr i
                  writeMU arr j ei
{-# INLINE swap #-}

mcopyMU :: (UA e) => MUArr e s -> MUArr e s -> Int -> Int -> Int -> ST s ()
mcopyMU from to iFrom iTo len = go 0
 where
 go n | n < len   = readMU from (iFrom + n) >>= writeMU to (iTo + n) >> go (n+1)
      | otherwise = return ()
{-# INLINE mcopyMU #-}
