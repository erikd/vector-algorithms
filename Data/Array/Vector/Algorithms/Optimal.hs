
-- ---------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Vector.Algorithms.Optimal
-- Copyright   : (c) 2008 Dan Doel
-- Maintainer  : Dan Doel
-- Stability   : Experimental
-- Portability : Portable
--
-- Optimal sorts for very small array sizes, or for small numbers of
-- particular indices in a larger array (to be used, for instance, for
-- sorting a median of 3 values into the lowest position in an array
-- for a media-of-3 quicksort).

-- The code herein was adapted from a C algorithm for optimal sorts
-- of small arrays. The original code was produced for the article
-- /Sorting Revisited/ by Paul Hsieh, available here:
--
--   http://www.azillionmonkeys.com/qed/sort.html
--
-- The LICENSE file contains the relevant copyright information for
-- the reference C code.

module Data.Array.Vector.Algorithms.Optimal
       ( sort2ByIndex
       , sort2ByOffset
       , sort3ByIndex
       , sort3ByOffset
       , sort4ByIndex
       , sort4ByOffset
       ) where

import Control.Monad.ST

import Data.Array.Vector

import Data.Array.Vector.Algorithms.Common
-- | Sorts the elements at the positions 'off' and 'off + 1' in the given
-- array using the comparison.
sort2ByOffset :: (UA e) => Comparison e -> MUArr e s -> Int -> ST s ()
sort2ByOffset cmp a off = sort2ByIndex cmp a off (off + 1)
{-# INLINE sort2ByOffset #-}

-- | Sorts the elements at the two given indices using the comparison. This
-- is essentially a compare-and-swap, although the first index is assumed to
-- be the 'lower' of the two.
sort2ByIndex :: (UA e) => Comparison e -> MUArr e s -> Int -> Int -> ST s ()
sort2ByIndex cmp a i j = do
  a0 <- readMU a i
  a1 <- readMU a j
  case cmp a0 a1 of
    GT -> writeMU a i a1 >> writeMU a j a0
    _  -> return ()
{-# INLINE sort2ByIndex #-}

-- | Sorts the three elements starting at the given offset in the array.
sort3ByOffset :: (UA e) => Comparison e -> MUArr e s -> Int -> ST s ()
sort3ByOffset cmp a off = sort3ByIndex cmp a off (off + 1) (off + 2)
{-# INLINE sort3ByOffset #-}

-- | Sorts the elements at the three given indices. The indices are assumed
-- to be given from lowest to highest, so if 'l < m < u' then
-- 'sort3ByIndex cmp a m l u' essentially sorts the median of three into the
-- lowest position in the array.
sort3ByIndex :: (UA e) => Comparison e -> MUArr e s -> Int -> Int -> Int -> ST s ()
sort3ByIndex cmp a i j k = do
  a0 <- readMU a i
  a1 <- readMU a j
  a2 <- readMU a k
  case cmp a0 a1 of
    GT -> case cmp a0 a2 of
            GT -> case cmp a2 a1 of
                    GT -> do writeMU a i a1
                             writeMU a j a2
                             writeMU a k a0
                    _  -> do writeMU a i a2
                             writeMU a k a0
            _  -> do writeMU a i a1
                     writeMU a j a0
    _  -> case cmp a1 a2 of
            GT -> case cmp a2 a0 of
                    GT -> do writeMU a j a2
                             writeMU a k a1
                    _  -> do writeMU a i a2
                             writeMU a k a1
                             writeMU a j a0
            _  -> return ()
{-# INLINE sort3ByIndex #-}

-- | Sorts the four elements beginning at the offset.
sort4ByOffset :: (UA e) => Comparison e -> MUArr e s -> Int -> ST s ()
sort4ByOffset cmp a off = sort4ByIndex cmp a off (off + 1) (off + 2) (off + 3)
{-# INLINE sort4ByOffset #-}

-- The horror...

-- | Sorts the elements at the four given indices. Like the 2 and 3 element
-- versions, this assumes that the indices are given in increasing order, so
-- it can be used to sort medians into particular positions and so on.
sort4ByIndex :: (UA e) => Comparison e -> MUArr e s -> Int -> Int -> Int -> Int -> ST s ()
sort4ByIndex cmp a i j k l = do
  a0 <- readMU a i
  a1 <- readMU a j
  a2 <- readMU a k
  a3 <- readMU a l
  case cmp a0 a1 of
    LT -> case cmp a1 a2 of
            LT -> case cmp a1 a3 of
                    LT -> case cmp a2 a3 of
                            GT -> do writeMU a k a3
                                     writeMU a l a2
                            _  -> return ()
                    _  -> do case cmp a0 a3 of
                               LT -> writeMU a j a3
                               _  -> do writeMU a j a0
                                        writeMU a i a3
                             writeMU a l a2
                             writeMU a k a1
            _  -> case cmp a0 a2 of
                    LT -> case cmp a2 a3 of
                            LT -> case cmp a1 a3 of
                                    LT -> do writeMU a j a2
                                             writeMU a k a1
                                    _  -> do writeMU a l a1
                                             writeMU a j a2
                                             writeMU a k a3
                            _  -> case cmp a0 a3 of
                                    LT -> do writeMU a l a1
                                             writeMU a j a3
                                    _  -> do writeMU a i a3
                                             writeMU a l a1
                                             writeMU a j a0
                    _  -> case cmp a0 a3 of
                            LT -> do writeMU a i a2
                                     case cmp a1 a3 of
                                       LT -> writeMU a k a1
                                       _  -> do writeMU a k a3
                                                writeMU a l a1
                                     writeMU a j a0
                            _  -> case cmp a2 a3 of
                                    LT -> do writeMU a i a2
                                             writeMU a k a0
                                             writeMU a j a3
                                             writeMU a l a1
                                    _  -> do writeMU a j a2
                                             writeMU a k a0
                                             writeMU a i a3
                                             writeMU a l a1
    _  -> case cmp a0 a2 of
            LT -> case cmp a0 a3 of
                    LT -> do writeMU a i a1
                             writeMU a j a0
                             case cmp a2 a3 of
                               GT -> do writeMU a k a3
                                        writeMU a l a2
                               _  -> return ()
                    _  -> do case cmp a1 a3 of
                               LT -> do writeMU a i a1
                                        writeMU a j a3
                               _  -> writeMU a i a3
                             writeMU a l a2
                             writeMU a k a0
            _  -> case cmp a1 a2 of
                    LT -> case cmp a2 a3 of
                            LT -> do writeMU a i a1
                                     writeMU a j a2
                                     case cmp a0 a3 of
                                       LT -> writeMU a k a0
                                       _  -> do writeMU a k a3
                                                writeMU a l a0
                            _  -> do case cmp a1 a3 of
                                       LT -> do writeMU a i a1
                                                writeMU a j a3
                                       _  -> writeMU a i a3
                                     writeMU a l a0
                    _  -> case cmp a1 a3 of
                            LT -> do writeMU a i a2
                                     case cmp a0 a3 of
                                       LT -> writeMU a k a0
                                       _  -> do writeMU a k a3
                                                writeMU a l a0
                            _  -> case cmp a2 a3 of
                                    LT -> do writeMU a i a2
                                             writeMU a k a1
                                             writeMU a j a3
                                             writeMU a l a0
                                    _  -> do writeMU a i a3
                                             writeMU a l a0
                                             writeMU a j a2
                                             writeMU a k a1
{-# INLINE sort4ByIndex #-}