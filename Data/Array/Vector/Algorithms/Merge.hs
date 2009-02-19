-- ---------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Vector.Algorithms.Merge
-- Copyright   : (c) 2008 Dan Doel
-- Maintainer  : Dan Doel <dan.doel@gmail.com>
-- Stability   : Experimental
-- Portability : Portable
--
-- This module implements a simple top-down merge sort. The temporary buffer
-- is preallocated to 1/2 the size of the input array, and shared through
-- the entire sorting process to ease the amount of allocation performed in
-- total. This is a stable sort.

module Data.Array.Vector.Algorithms.Merge
       ( sort
       , sortBy
       , sortByBounds
       , Comparison
       ) where

import Control.Monad.ST

import Data.Bits
import Data.Array.Vector
import Data.Array.Vector.Algorithms.Common

import qualified Data.Array.Vector.Algorithms.Optimal   as O
import qualified Data.Array.Vector.Algorithms.Insertion as I

-- | Sorts an array using the default comparison.
sort :: (Ord e, UA e) => MUArr e s -> ST s ()
sort = sortBy compare
{-# INLINE sort #-}

-- | Sorts an array using a custom comparison.
sortBy :: (UA e) => Comparison e -> MUArr e s -> ST s ()
sortBy cmp arr = sortByBounds cmp arr 0 (lengthMU arr)
{-# INLINE sortBy #-}

-- | Sorts a portion of an array [l,u) using a custom comparison.
sortByBounds :: (UA e) => Comparison e -> MUArr e s -> Int -> Int -> ST s ()
sortByBounds cmp arr l u
  | len < 1   = return ()
  | len == 2  = O.sort2ByOffset cmp arr l
  | len == 3  = O.sort3ByOffset cmp arr l
  | len == 4  = O.sort4ByOffset cmp arr l
  | otherwise = do tmp <- newMU size
                   mergeSortWithBuf cmp arr tmp l u
 where
 len  = u - l
 size = (u + l) `div` 2 - l
{-# INLINE sortByBounds #-}

mergeSortWithBuf :: (UA e) => Comparison e -> MUArr e s -> MUArr e s -> Int -> Int -> ST s ()
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

merge :: (UA e) => Comparison e -> MUArr e s -> MUArr e s -> Int -> Int -> Int -> ST s ()
merge cmp arr tmp l m u = do memcpyOffMU arr tmp l 0 uTmp
                             eTmp <- readMU tmp 0
                             eArr <- readMU arr m
                             loop 0 eTmp m eArr l
 where
 uTmp = m - l
 uArr = u
 loop iTmp eTmp iArr eArr iIns
   | iTmp >= uTmp = return ()
   | iArr >= uArr = memcpyOffMU tmp arr iTmp iIns (uTmp - iTmp)
   | otherwise    = case cmp eArr eTmp of
                      LT -> do writeMU arr iIns eArr
                               eArr <- readMU arr (iArr+1)
                               loop iTmp eTmp (iArr+1) eArr (iIns+1)
                      _  -> do writeMU arr iIns eTmp
                               eTmp <- readMU tmp (iTmp+1)
                               loop (iTmp+1) eTmp iArr eArr (iIns+1)
{-# INLINE merge #-}

threshold :: Int
threshold = 25
{-# INLINE threshold #-}