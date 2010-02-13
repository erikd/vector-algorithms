-- ---------------------------------------------------------------------------
-- |
-- Module      : Data.Vector.Algorithms.Merge
-- Copyright   : (c) 2008-2010 Dan Doel
-- Maintainer  : Dan Doel <dan.doel@gmail.com>
-- Stability   : Experimental
-- Portability : Portable
--
-- This module implements a simple top-down merge sort. The temporary buffer
-- is preallocated to 1/2 the size of the input array, and shared through
-- the entire sorting process to ease the amount of allocation performed in
-- total. This is a stable sort.

module Data.Vector.Algorithms.Merge
       ( sort
       , sortBy
       , sortByBounds
       , Comparison
       ) where

import Prelude hiding (read, length)

import Control.Monad.Primitive

import Data.Bits
import Data.Vector.Generic.Mutable

import Data.Vector.Algorithms.Common (Comparison, copyOffset)

import qualified Data.Vector.Algorithms.Optimal   as O
import qualified Data.Vector.Algorithms.Insertion as I

-- | Sorts an array using the default comparison.
sort :: (PrimMonad m, MVector v e, Ord e) => v (PrimState m) e -> m ()
sort = sortBy compare
{-# INLINE sort #-}

-- | Sorts an array using a custom comparison.
sortBy :: (PrimMonad m, MVector v e) => Comparison e -> v (PrimState m) e -> m ()
sortBy cmp arr = sortByBounds cmp arr 0 (length arr)
{-# INLINE sortBy #-}

-- | Sorts a portion of an array [l,u) using a custom comparison.
sortByBounds :: (PrimMonad m, MVector v e)
             => Comparison e -> v (PrimState m) e -> Int -> Int -> m ()
sortByBounds cmp vec l u
  | len < 1   = return ()
  | len == 2  = O.sort2ByOffset cmp vec l
  | len == 3  = O.sort3ByOffset cmp vec l
  | len == 4  = O.sort4ByOffset cmp vec l
  | otherwise = do tmp <- new len
                   mergeSortWithBuf cmp (unsafeSlice l len vec) tmp
 where
 len  = u - l
-- size = (u + l) `div` 2 - l
{-# INLINE sortByBounds #-}

{-
mergeSortWithBuf :: (PrimMonad m, MVector v e)
                 => Comparison e -> v (PrimState m) e -> v (PrimState m) e
                 -> Int -> Int -> m ()
mergeSortWithBuf cmp arr tmp = loop
 where
 loop l u
   | len < threshold = I.sortByBounds cmp arr l u
   | otherwise       = do loop l mid
                          loop mid u
                          merge cmp arr tmp l mid u
  where
  len = u - l
  mid = (u + l) `shiftR` 1
{-# INLINE mergeSortWithBuf #-}
-}

mergeSortWithBuf :: (PrimMonad m, MVector v e)
                 => Comparison e -> v (PrimState m) e -> v (PrimState m) e -> m ()
mergeSortWithBuf cmp src buf
  | length src < threshold = I.sortByBounds cmp src 0 (length src)
  | otherwise              = do mergeSortWithBuf cmp srcL bufL
                                mergeSortWithBuf cmp srcU bufU
                                merge cmp src buf mid
 where
 len = length src
 mid = len `shiftR` 1

 srcL = unsafeSlice 0   mid         src
 srcU = unsafeSlice mid (len - mid) src
 
 bufL = unsafeSlice 0   mid         buf
 bufU = unsafeSlice mid (len - mid) buf

{-
merge :: (PrimMonad m, MVector v e)
      => Comparison e -> v (PrimState m) e -> v (PrimState m) e
      -> Int -> Int -> Int -> m ()
merge cmp arr tmp l m u = do copyOffset arr tmp l 0 uTmp
                             eTmp <- read tmp 0
                             eArr <- read arr m
                             loop 0 eTmp m eArr l
 where
 uTmp = m - l
 uArr = u
 loop iTmp eTmp iArr eArr iIns
   | iTmp >= uTmp = return ()
   | iArr >= uArr = copyOffset tmp arr iTmp iIns (uTmp - iTmp)
   | otherwise    = case cmp eArr eTmp of
                      LT -> do write arr iIns eArr
                               eArr <- read arr (iArr+1)
                               loop iTmp eTmp (iArr+1) eArr (iIns+1)
                      _  -> do write arr iIns eTmp
                               eTmp <- read tmp (iTmp+1)
                               loop (iTmp+1) eTmp iArr eArr (iIns+1)
{-# INLINE merge #-}
-}

merge :: (PrimMonad m, MVector v e)
      => Comparison e -> v (PrimState m) e -> v (PrimState m) e
      -> Int -> m ()
merge cmp src buf mid = do unsafeCopy tmp lower
                           eTmp <- unsafeRead tmp 0
                           eUpp <- unsafeRead upper 0
                           loop tmp 0 eTmp upper 0 eUpp 0
 where
 lower = unsafeSlice 0   mid                src
 upper = unsafeSlice mid (length src - mid) src
 tmp   = unsafeSlice 0   mid                buf

 loop low iLow eLow high iHigh eHigh iIns
   | iLow  >= length low  = return ()
   | iHigh >= length high = unsafeCopy (unsafeSlice iIns (length low - iLow) src)
                                       (unsafeSlice iLow (length low - iLow) low)
   | otherwise            = case cmp eHigh eLow of
                             LT -> do unsafeWrite src iIns eHigh
                                      eHigh <- unsafeRead high (iHigh + 1)
                                      loop low iLow eLow high (iHigh + 1) eHigh (iIns + 1)
                             _  -> do unsafeWrite src iIns eLow
                                      eLow <- unsafeRead low (iLow + 1)
                                      loop low (iLow + 1) eLow high iHigh eHigh (iIns + 1)
{-# INLINE merge #-}

threshold :: Int
threshold = 25
{-# INLINE threshold #-}
