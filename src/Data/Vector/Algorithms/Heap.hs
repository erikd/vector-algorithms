{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- ---------------------------------------------------------------------------
-- |
-- Module      : Data.Vector.Algorithms.Heap
-- Copyright   : (c) 2008-2015 Dan Doel
-- Maintainer  : Dan Doel <dan.doel@gmail.com>
-- Stability   : Experimental
-- Portability : Non-portable (type operators)
--
-- This module implements operations for working with a quaternary heap stored
-- in an unboxed array. Most heapsorts are defined in terms of a binary heap,
-- in which each internal node has at most two children. By contrast, a
-- quaternary heap has internal nodes with up to four children. This reduces
-- the number of comparisons in a heapsort slightly, and improves locality
-- (again, slightly) by flattening out the heap.

module Data.Vector.Algorithms.Heap
       ( -- * Sorting
         sort
       , sortBy
       , sortByBounds
         -- * Selection
       , select
       , selectBy
       , selectByBounds
         -- * Partial sorts
       , partialSort
       , partialSortBy
       , partialSortByBounds
         -- * Heap operations
       , heapify
       , pop
       , popTo
       , sortHeap
       , heapInsert
       , Comparison
       ) where

import Prelude hiding (read, length)

import Control.Monad
import Control.Monad.Primitive

import Data.Bits

import Data.Vector.Generic.Mutable

import Data.Vector.Algorithms.Common (Comparison)

import qualified Data.Vector.Algorithms.Optimal as O

-- | Sorts an entire array using the default ordering.
sort :: (PrimMonad m, MVector v e, Ord e) => v (PrimState m) e -> m ()
sort = sortBy compare
{-# INLINABLE sort #-}

-- | Sorts an entire array using a custom ordering.
sortBy :: (PrimMonad m, MVector v e) => Comparison e -> v (PrimState m) e -> m ()
sortBy cmp a = sortByBounds cmp a 0 (length a)
{-# INLINE sortBy #-}

-- | Sorts a portion of an array [l,u) using a custom ordering
sortByBounds
  :: (PrimMonad m, MVector v e)
  => Comparison e
  -> v (PrimState m) e
  -> Int -- ^ lower index, l
  -> Int -- ^ upper index, u
  -> m ()
sortByBounds cmp a l u
  | len < 2   = return ()
  | len == 2  = O.sort2ByOffset cmp a l
  | len == 3  = O.sort3ByOffset cmp a l
  | len == 4  = O.sort4ByOffset cmp a l
  | otherwise = heapify cmp a l u >> sortHeap cmp a l (l+4) u >> O.sort4ByOffset cmp a l
 where len = u - l
{-# INLINE sortByBounds #-}

-- | Moves the lowest k elements to the front of the array.
-- The elements will be in no particular order.
select
  :: (PrimMonad m, MVector v e, Ord e)
  => v (PrimState m) e
  -> Int -- ^ number of elements to select, k
  -> m ()
select = selectBy compare
{-# INLINE select #-}

-- | Moves the lowest (as defined by the comparison) k elements
-- to the front of the array. The elements will be in no particular
-- order.
selectBy
  :: (PrimMonad m, MVector v e)
  => Comparison e
  -> v (PrimState m) e
  -> Int -- ^ number of elements to select, k
  -> m ()
selectBy cmp a k = selectByBounds cmp a k 0 (length a)
{-# INLINE selectBy #-}

-- | Moves the 'lowest' k elements in the portion [l,u) of the
-- array into the positions [l,k+l). The elements will be in
-- no particular order.
selectByBounds
  :: (PrimMonad m, MVector v e)
  => Comparison e
  -> v (PrimState m) e
  -> Int -- ^ number of elements to select, k
  -> Int -- ^ lower index, l
  -> Int -- ^ upper index, u
  -> m ()
selectByBounds cmp a k l u
  | l + k <= u = heapify cmp a l (l + k) >> go l (l + k) (u - 1)
  | otherwise  = return ()
 where
 go l m u
   | u < m      = return ()
   | otherwise  = do el <- unsafeRead a l
                     eu <- unsafeRead a u
                     case cmp eu el of
                       LT -> popTo cmp a l m u
                       _  -> return ()
                     go l m (u - 1)
{-# INLINE selectByBounds #-}

-- | Moves the lowest k elements to the front of the array, sorted.
--
-- The remaining values of the array will be in no particular order.
partialSort
  :: (PrimMonad m, MVector v e, Ord e)
  => v (PrimState m) e
  -> Int -- ^ number of elements to sort, k
  -> m ()
partialSort = partialSortBy compare
{-# INLINE partialSort #-}

-- | Moves the lowest k elements (as defined by the comparison) to
-- the front of the array, sorted.
--
-- The remaining values of the array will be in no particular order.
partialSortBy
  :: (PrimMonad m, MVector v e)
  => Comparison e
  -> v (PrimState m) e
  -> Int -- ^ number of elements to sort, k
  -> m ()
partialSortBy cmp a k = partialSortByBounds cmp a k 0 (length a)
{-# INLINE partialSortBy #-}

-- | Moves the lowest k elements in the portion [l,u) of the array
-- into positions [l,k+l), sorted.
--
-- The remaining values in [l,u) will be in no particular order. Values outside
-- the range [l,u) will be unaffected.
partialSortByBounds
  :: (PrimMonad m, MVector v e)
  => Comparison e
  -> v (PrimState m) e
  -> Int -- ^ number of elements to sort, k
  -> Int -- ^ lower index, l
  -> Int -- ^ upper index, u
  -> m ()
partialSortByBounds cmp a k l u
  -- this potentially does more work than absolutely required,
  -- but using a heap to find the least 2 of 4 elements
  -- seems unlikely to be better than just sorting all of them
  -- with an optimal sort, and the latter is obviously index
  -- correct.
  | len <  2   = return ()
  | len == 2   = O.sort2ByOffset cmp a l
  | len == 3   = O.sort3ByOffset cmp a l
  | len == 4   = O.sort4ByOffset cmp a l
  | u <= l + k = sortByBounds cmp a l u
  | otherwise  = do selectByBounds cmp a k l u
                    sortHeap cmp a l (l + 4) (l + k)
                    O.sort4ByOffset cmp a l
 where
 len = u - l
{-# INLINE partialSortByBounds #-}

-- | Constructs a heap in a portion of an array [l, u), using the values therein.
--
-- Note: 'heapify' is more efficient than constructing a heap by repeated
-- insertion. Repeated insertion has complexity O(n*log n) while 'heapify' is able
-- to construct a heap in O(n), where n is the number of elements in the heap.
heapify
  :: (PrimMonad m, MVector v e)
  => Comparison e
  -> v (PrimState m) e
  -> Int -- ^ lower index, l
  -> Int -- ^ upper index, u
  -> m ()
heapify cmp a l u = loop $ (len - 1) `shiftR` 2
  where
 len = u - l
 loop k
   | k < 0     = return ()
   | otherwise = unsafeRead a (l+k) >>= \e ->
                   siftByOffset cmp a e l k len >> loop (k - 1)
{-# INLINE heapify #-}

-- | Given a heap stored in a portion of an array [l,u), swaps the
-- top of the heap with the element at u and rebuilds the heap.
pop
  :: (PrimMonad m, MVector v e)
  => Comparison e
  -> v (PrimState m) e
  -> Int -- ^ lower heap index, l
  -> Int -- ^ upper heap index, u
  -> m ()
pop cmp a l u = popTo cmp a l u u
{-# INLINE pop #-}

-- | Given a heap stored in a portion of an array [l,u) swaps the top
-- of the heap with the element at position t, and rebuilds the heap.
popTo
  :: (PrimMonad m, MVector v e)
  => Comparison e
  -> v (PrimState m) e
  -> Int -- ^ lower heap index, l
  -> Int -- ^ upper heap index, u
  -> Int -- ^ index to pop to, t
  -> m ()
popTo cmp a l u t = do al <- unsafeRead a l
                       at <- unsafeRead a t
                       unsafeWrite a t al
                       siftByOffset cmp a at l 0 (u - l)
{-# INLINE popTo #-}

-- | Given a heap stored in a portion of an array [l,u), sorts the
-- highest values into [m,u). The elements in [l,m) are not in any
-- particular order.
sortHeap
  :: (PrimMonad m, MVector v e)
  => Comparison e
  -> v (PrimState m) e
  -> Int -- ^ lower heap index, l
  -> Int -- ^ lower bound of final sorted portion, m
  -> Int -- ^ upper heap index, u
  -> m ()
sortHeap cmp a l m u = loop (u-1) >> unsafeSwap a l m
 where
 loop k
   | m < k     = pop cmp a l k >> loop (k-1)
   | otherwise = return ()
{-# INLINE sortHeap #-}

-- | Given a heap stored in a portion of an array [l,u) and an element e,
-- inserts the element into the heap, resulting in a heap in [l,u].
--
-- Note: it is best to only use this operation when incremental construction of
-- a heap is required. 'heapify' is capable of building a heap in O(n) time,
-- while repeated insertion takes O(n*log n) time.
heapInsert
  :: (PrimMonad m, MVector v e)
  => Comparison e
  -> v (PrimState m) e
  -> Int -- ^ lower heap index, l
  -> Int -- ^ upper heap index, u
  -> e -- ^ element to be inserted, e
  -> m ()
heapInsert cmp v l u e = sift (u - l)
 where
 sift k
   | k <= 0    = unsafeWrite v l e
   | otherwise = let pi = shiftR (k-1) 2
                  in unsafeRead v (l + pi) >>= \p -> case cmp p e of
                       LT -> unsafeWrite v (l + k) p >> sift pi
                       _  -> unsafeWrite v (l + k) e
{-# INLINE heapInsert #-}

-- Rebuilds a heap with a hole in it from start downwards. Afterward,
-- the heap property should apply for [start + off, len + off). val
-- is the new value to be put in the hole.
siftByOffset :: (PrimMonad m, MVector v e)
             => Comparison e -> v (PrimState m) e -> e -> Int -> Int -> Int -> m ()
siftByOffset cmp a val off start len = sift val start len
 where
 sift val root len
   | child < len = do (child', ac) <- maximumChild cmp a off child len
                      case cmp val ac of
                        LT -> unsafeWrite a (root + off) ac >> sift val child' len
                        _  -> unsafeWrite a (root + off) val
   | otherwise = unsafeWrite a (root + off) val
  where child = root `shiftL` 2 + 1
{-# INLINE siftByOffset #-}

-- Finds the maximum child of a heap node, given the indx of the first child.
maximumChild :: (PrimMonad m, MVector v e)
             => Comparison e -> v (PrimState m) e -> Int -> Int -> Int -> m (Int,  e)
maximumChild cmp a off child1 len
  | child4 < len = do ac1 <- unsafeRead a (child1 + off)
                      ac2 <- unsafeRead a (child2 + off)
                      ac3 <- unsafeRead a (child3 + off)
                      ac4 <- unsafeRead a (child4 + off)
                      return $ case cmp ac1 ac2 of
                                 LT -> case cmp ac2 ac3 of
                                         LT -> case cmp ac3 ac4 of
                                                 LT -> (child4, ac4)
                                                 _  -> (child3, ac3)
                                         _  -> case cmp ac2 ac4 of
                                                 LT -> (child4, ac4)
                                                 _  -> (child2, ac2)
                                 _  -> case cmp ac1 ac3 of
                                         LT -> case cmp ac3 ac4 of
                                                 LT -> (child4, ac4)
                                                 _  -> (child3, ac3)
                                         _  -> case cmp ac1 ac4 of
                                                 LT -> (child4, ac4)
                                                 _  -> (child1, ac1)
  | child3 < len = do ac1 <- unsafeRead a (child1 + off)
                      ac2 <- unsafeRead a (child2 + off)
                      ac3 <- unsafeRead a (child3 + off)
                      return $ case cmp ac1 ac2 of
                                 LT -> case cmp ac2 ac3 of
                                         LT -> (child3, ac3)
                                         _  -> (child2, ac2)
                                 _  -> case cmp ac1 ac3 of
                                         LT -> (child3, ac3)
                                         _  -> (child1, ac1)
  | child2 < len = do ac1 <- unsafeRead a (child1 + off)
                      ac2 <- unsafeRead a (child2 + off)
                      return $ case cmp ac1 ac2 of
                                 LT -> (child2, ac2)
                                 _  -> (child1, ac1)
  | otherwise    = do ac1 <- unsafeRead a (child1 + off) ; return (child1, ac1)
 where
 child2 = child1 + 1
 child3 = child1 + 2
 child4 = child1 + 3
{-# INLINE maximumChild #-}
