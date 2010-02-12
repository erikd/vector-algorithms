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
sortByBounds cmp arr l u
  | len < 1   = return ()
  | len == 2  = O.sort2ByOffset cmp arr l
  | len == 3  = O.sort3ByOffset cmp arr l
  | len == 4  = O.sort4ByOffset cmp arr l
  | otherwise = do tmp <- new size
                   mergeSortWithBuf cmp arr tmp l u
 where
 len  = u - l
 size = (u + l) `div` 2 - l
{-# INLINE sortByBounds #-}

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

threshold :: Int
threshold = 25
{-# INLINE threshold #-}
