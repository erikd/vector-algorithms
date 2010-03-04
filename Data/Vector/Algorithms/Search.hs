{-# LANGUAGE BangPatterns #-}

-- ---------------------------------------------------------------------------
-- |
-- Module      : Data.Vector.Algorithms.Search
-- Copyright   : (c) 2009-2010 Dan Doel
-- Maintainer  : Dan Doel <dan.doel@gmail.com>
-- Stability   : Experimental
-- Portability : Non-portable (bang patterns)
--
-- This module implements several methods of searching for indicies to insert
-- elements into a sorted array.

module Data.Vector.Algorithms.Search
       ( binarySearch
       , binarySearchBy
       , binarySearchByBounds
       , binarySearchL
       , binarySearchLBy
       , binarySearchLByBounds
       , Comparison
       ) where

import Prelude hiding (read, length)

import Control.Monad.Primitive

import Data.Bits

import Data.Vector.Generic.Mutable

import Data.Vector.Algorithms.Common (Comparison)

-- | Finds an index in a givesn sorted array at which the given element could
-- be inserted while maintaining the sortedness of the array.
binarySearch :: (PrimMonad m, MVector v e, Ord e)
             => v (PrimState m) e -> e -> m Int
binarySearch = binarySearchBy compare

-- | Finds an index in a given array, which must be sorted with respect to the
-- given comparison function, at which the given element could be inserted while
-- preserving the array's sortedness.
binarySearchBy :: (PrimMonad m, MVector v e)
               => Comparison e -> v (PrimState m) e -> e -> m Int
binarySearchBy cmp arr e = binarySearchByBounds cmp arr e 0 (length arr)

-- | Given an array sorted with respect to a given comparison function in indices
-- in [l,u), finds an index in [l,u] at which the given element could be inserted
-- while preserving sortedness.
binarySearchByBounds :: (PrimMonad m, MVector v e)
                     => Comparison e -> v (PrimState m) e -> e -> Int -> Int -> m Int
binarySearchByBounds cmp arr e l u
  | u <= l    = return l
  | otherwise = do e' <- unsafeRead arr k
                   case cmp e' e of
                     LT -> binarySearchByBounds cmp arr e (k+1) u
                     EQ -> return k
                     GT -> binarySearchByBounds cmp arr e l     k
 where k = (u + l) `shiftR` 1
{-# INLINE binarySearchByBounds #-}

-- | Finds the lowest index in a given sorted array at which the given element
-- could be inserted while maintaining the sortedness.
binarySearchL :: (PrimMonad m, MVector v e, Ord e) => v (PrimState m) e -> e -> m Int
binarySearchL = binarySearchLBy compare
{-# INLINE binarySearchL #-}

-- | Finds the lowest index in a given array, which must be sorted with respect to 
-- the given comparison function, at which the given element could be inserted
-- while preserving the sortedness.
binarySearchLBy :: (PrimMonad m, MVector v e)
                => Comparison e -> v (PrimState m) e -> e -> m Int
binarySearchLBy cmp arr e = binarySearchLByBounds cmp arr e 0 (length arr)
{-# INLINE binarySearchLBy #-}

-- | Given an array sorted with respect to a given comparison function on indices
-- in [l,u), finds the lowest index in [l,u] at which the given element could be
-- inserted while preserving sortedness.
binarySearchLByBounds :: (PrimMonad m, MVector v e)
                      => Comparison e -> v (PrimState m) e -> e -> Int -> Int -> m Int
binarySearchLByBounds cmp arr e !l !u
  | u <= l    = return l
  | otherwise = do e' <- unsafeRead arr k
                   case cmp e' e of
                     LT -> binarySearchLByBounds cmp arr e (k+1) u
                     _  -> binarySearchLByBounds cmp arr e l     k
 where k = (u + l) `shiftR` 1
{-# INLINE binarySearchLByBounds #-}

-- | Finds the greatest index in a given sorted array at which the given element
-- could be inserted while maintaining sortedness.
binarySearchR :: (PrimMonad m, MVector v e, Ord e) => v (PrimState m) e -> e -> m Int
binarySearchR = binarySearchRBy compare
{-# INLINE binarySearchR #-}

-- | Finds the greatest index in a given array, which must be sorted with respect to
-- the given comparison function, at which the given element could be inserted
-- while preserving the sortedness.
binarySearchRBy :: (PrimMonad m, MVector v e)
                => Comparison e -> v (PrimState m) e -> e -> m Int
binarySearchRBy cmp vec e = binarySearchRByBounds cmp vec e 0 (length vec)
{-# INLINE binarySearchRBy #-}

binarySearchRByBounds :: (PrimMonad m, MVector v e)
                      => Comparison e -> v (PrimState m) e -> e -> Int -> Int -> m Int
binarySearchRByBounds cmp vec e !l !u
  | u <= l    = return l
  | otherwise = do e' <- unsafeRead vec k
                   case cmp e' e of
                     GT -> binarySearchRByBounds cmp vec e l     k
                     _  -> binarySearchRByBounds cmp vec e (k+1) u
 where k = (u + l) `shiftR` 1
{-# INLINE binarySearchRByBounds #-}