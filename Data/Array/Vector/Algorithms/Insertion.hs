
-- ---------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Vector.Algorithms.Insertion
-- Copyright   : (c) 2008 Dan Doel
-- Maintainer  : Dan Doel
-- Stability   : Experimental
-- Portability : Portable
--
-- A simple insertion sort. Though it's O(n^2), its iterative nature can be
-- beneficial for small arrays. It is used to sort small segments of an array
-- by some of the more heavy-duty, recursive algorithms.

module Data.Array.Vector.Algorithms.Insertion
       ( sort
       , sortBy
       , sortByBounds
       , sortByBounds'
       , Comparison
       ) where


import Control.Monad.ST

import Data.Array.Vector
import Data.Array.Vector.Algorithms.Common

import qualified Data.Array.Vector.Algorithms.Optimal as O

-- | Sorts an entire array using the default comparison for the type
sort :: (UA e, Ord e) => MUArr e s -> ST s ()
sort = sortBy compare
{-# INLINE sort #-}

-- | Sorts an entire array using a given comparison
sortBy :: (UA e) => Comparison e -> MUArr e s -> ST s ()
sortBy cmp a = sortByBounds cmp a 0 (lengthMU a)
{-# INLINE sortBy #-}

-- | Sorts the portion of an array delimited by [l,u)
sortByBounds :: (UA e) => Comparison e -> MUArr e s -> Int -> Int -> ST s ()
sortByBounds cmp a l u
  | len < 2   = return ()
  | len == 2  = O.sort2ByOffset cmp a l
  | len == 3  = O.sort3ByOffset cmp a l
  | len == 4  = O.sort4ByOffset cmp a l
  | otherwise = O.sort4ByOffset cmp a l >> sortByBounds' cmp a l (l + 4) u
 where
 len = u - l
{-# INLINE sortByBounds #-}

-- | Sorts the portion of the array delimited by [l,u) under the assumption
-- that [l,m) is already sorted.
sortByBounds' :: (UA e) => Comparison e -> MUArr e s -> Int -> Int -> Int -> ST s ()
sortByBounds' cmp a l m u = sort m
 where
 sort i
   | i < u     = do v <- readMU a i
                    insert cmp a l v i
                    sort (i+1)
   | otherwise = return ()
{-# INLINE sortByBounds' #-}

-- Given a sorted array in [l,u), inserts val into its proper position,
-- yielding a sorted [l,u]
insert :: (UA e) => Comparison e -> MUArr e s -> Int -> e -> Int -> ST s ()
insert cmp a l = loop
 where
 loop val j
   | j <= l    = writeMU a l val
   | otherwise = do e <- readMU a (j - 1)
                    case cmp val e of
                      LT -> writeMU a j e >> loop val (j - 1)
                      _  -> writeMU a j val
{-# INLINE insert #-}