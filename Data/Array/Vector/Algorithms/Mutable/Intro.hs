{-# LANGUAGE TypeOperators, BangPatterns #-}

-- ---------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Vector.Algorithms.Mutable.Intro
-- Copyright   : (c) 2008 Dan Doel
-- Maintainer  : Dan Doel <dan.doel@gmail.com>
-- Stability   : Experimental
-- Portability : Non-portable (type operators, bang patterns)
--
-- This module implements various algorithms based on the introsort algorithm,
-- originally described by David R. Musser in the paper /Introspective Sorting
-- and Selection Algorithms/. It is also in widespread practical use, as the
-- standard unstable sort used in the C++ Standard Template Library.
--
-- Introsort is at its core a quicksort. The version implemented here has the
-- following optimizations that make it perform better in practice:
--
--   * Small segments of the array are left unsorted until a final insertion
--     sort pass. This is faster than recursing all the way down to
--     one-element arrays.
--
--   * The pivot for segment [l,u) is chosen as the median of the elements at
--     l, u-1 and (u+l)/2. This yields good behavior on mostly sorted (or
--     reverse-sorted) arrays.
--
--   * The algorithm tracks its recursion depth, and if it decides it is
--     taking too long (depth greater than 2 * lg n), it switches to a heap
--     sort to maintain O(n lg n) worst case behavior. (This is what makes the
--     algorithm introsort).

module Data.Array.Vector.Algorithms.Mutable.Intro
       ( -- * Sorting
         sort
       , sortBy
       , sortByBounds ) where

import Control.Monad
import Control.Monad.ST

import Data.Array.Vector
import Data.Array.Vector.Algorithms.Common
import Data.Bits

import qualified Data.Array.Vector.Algorithms.Mutable.Insertion as I
import qualified Data.Array.Vector.Algorithms.Mutable.Optimal   as O
import qualified Data.Array.Vector.Algorithms.Mutable.TriHeap   as H

-- | Sorts an entire array using the default ordering.
sort :: (UA e, Ord e) => MUArr e s -> ST s ()
sort = sortBy compare
{-# INLINE sort #-}

-- | Sorts an entire array using a custom ordering.
sortBy :: (UA e) => Comparison e -> MUArr e s -> ST s ()
sortBy cmp a = sortByBounds cmp a 0 (lengthMU a)
{-# INLINE sortBy #-}

-- | Sorts a portion of an array [l,u) using a custom ordering
sortByBounds :: (UA e) => Comparison e -> MUArr e s -> Int -> Int -> ST s ()
sortByBounds cmp a l u
  | len < 2   = return ()
  | len == 2  = O.sort2ByOffset cmp a l
  | len == 3  = O.sort3ByOffset cmp a l
  | len == 4  = O.sort4ByOffset cmp a l
  | otherwise = sort (ilg len) l u >> I.sortByBounds cmp a l u
 where
 len = u - l
 sort 0 l u = H.sortByBounds cmp a l u
 sort d l u
   | len < threshold = return ()
   | otherwise = do O.sort3ByIndex cmp a c l (u-1) -- sort the median into the lowest position
                    p <- readMU a l
                    mid <- partitionBy cmp a p (l+1) u
                    swap a l (mid - 1)
                    sort (d-1) mid u
                    sort (d-1) l   (mid - 1)
  where
  len = u - l
  c   = (u + l) `div` 2
{-# INLINE sortByBounds #-}

partitionBy :: (UA e) => Comparison e -> MUArr e s -> e -> Int -> Int -> ST s Int
partitionBy cmp a = partUp
 where
 partUp p l u
   | l < u = do e <- readMU a l
                case cmp e p of
                  LT -> partUp p (l+1) u
                  _  -> partDown p l (u-1)
   | otherwise = return l
 partDown p l u
   | l < u = do e <- readMU a u
                case cmp p e of
                  LT -> partDown p l (u-1)
                  _  -> swap a l u >> partUp p (l+1) u
   | otherwise = return l
{-# INLINE partitionBy #-}

-- computes the number of recursive calls after which heapsort should
-- be invoked given the lower and upper indices of the array to be sorted
ilg :: Int -> Int
ilg m = 2 * loop m 0
 where
 loop 0 !k = k - 1
 loop n !k = loop (n `shiftR` 1) (k+1)

-- the size of array at which the introsort algorithm switches to insertion sort
threshold :: Int
threshold = 18
{-# INLINE threshold #-}