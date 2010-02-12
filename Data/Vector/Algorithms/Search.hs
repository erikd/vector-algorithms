{-# LANGUAGE BangPatterns #-}

-- ---------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Vector.Algorithms.Search
-- Copyright   : (c) 2009 Dan Doel
-- Maintainer  : Dan Doel <dan.doel@gmail.com>
-- Stability   : Experimental
-- Portability : Non-portable (bang patterns)
--
-- This module implements several methods of searching for indicies to insert
-- elements into a sorted array.

module Data.Array.Vector.Algorithms.Search
       ( binarySearch
       , binarySearchBy
       , binarySearchByBounds
       , binarySearchL
       , binarySearchLBy
       , binarySearchLByBounds
       , Comparison
       ) where

import Control.Monad.ST

import Data.Bits

import Data.Array.Vector
import Data.Array.Vector.Algorithms.Common

-- | Finds an index in a givesn sorted array at which the given element could
-- be inserted while maintaining the sortedness of the array.
binarySearch :: (UA e, Ord e) => MUArr e s -> e -> ST s Int
binarySearch = binarySearchBy compare

-- | Finds an index in a given array, which must be sorted with respect to the
-- given comparison function, at which the given element could be inserted while
-- preserving the array's sortedness.
binarySearchBy :: (UA e) => Comparison e -> MUArr e s -> e -> ST s Int
binarySearchBy cmp arr e = binarySearchByBounds cmp arr e 0 (lengthMU arr)

-- | Given an array sorted with respect to a given comparison function in indices
-- in [l,u), finds an index in [l,u] at which the given element could be inserted
-- while preserving sortedness.
binarySearchByBounds :: (UA e) => Comparison e -> MUArr e s -> e -> Int -> Int -> ST s Int
binarySearchByBounds cmp arr e l u
  | u <= l    = return l
  | otherwise = do e' <- readMU arr k
                   case cmp e' e of
                     LT -> binarySearchByBounds cmp arr e (k+1) u
                     EQ -> return k
                     GT -> binarySearchByBounds cmp arr e l     k
 where k = (u + l) `shiftR` 1
{-# INLINE binarySearchByBounds #-}

-- | Finds the lowest index in a given sorted array at which the given element
-- could be inserted while maintaining the sortedness.
binarySearchL :: (UA e, Ord e) => MUArr e s -> e -> ST s Int
binarySearchL = binarySearchLBy compare
{-# INLINE binarySearchL #-}

-- | Finds the lowest index in a given array, which must be sorted with respect to 
-- the given comparison function, at which the given element could be inserted
-- while preserving the sortedness.
binarySearchLBy :: (UA e) => Comparison e -> MUArr e s -> e -> ST s Int
binarySearchLBy cmp arr e = binarySearchLByBounds cmp arr e 0 (lengthMU arr)
{-# INLINE binarySearchLBy #-}

-- | Given an array sorted with respect to a given comparison function on indices
-- in [l,u), finds the lowest index in [l,u] at which the given element could be
-- inserted while preserving sortedness.
binarySearchLByBounds :: (UA e) => Comparison e -> MUArr e s -> e -> Int -> Int -> ST s Int
binarySearchLByBounds cmp arr e !l !u
  | u <= l    = return l
  | otherwise = do e' <- readMU arr k
                   case cmp e' e of
                     LT -> binarySearchLByBounds cmp arr e (k+1) u
                     _  -> binarySearchLByBounds cmp arr e l     k
 where k = (u + l) `shiftR` 1
{-# INLINE binarySearchLByBounds #-}