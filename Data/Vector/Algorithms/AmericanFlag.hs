{-# LANGUAGE FlexibleContexts #-}

-- ---------------------------------------------------------------------------
-- |
-- Module      : Data.Vector.Algorithms.AmericanFlag
-- Copyright   : (c) 2011 Dan Doel
-- Maintainer  : Dan Doel <dan.doel@gmail.com>
-- Stability   : Experimental
-- Portability : Non-portable ()
--
-- This module implements American flag sort: an in-place, unstable, bucket
-- sort. Also in contrast to radix sort, the values are inspected in a big
-- endian order, and buckets are sorted via recursive splitting. This,
-- however, makes it sensible for sorting strings in lexicographic order
-- (provided indexing is fast).

module Data.Vector.Algorithms.AmericanFlag ( sortBy
                                           ) where

import Prelude hiding (read, length)

import Control.Monad
import Control.Monad.Primitive

import Data.Vector.Generic.Mutable
import qualified Data.Vector.Primitive.Mutable as PV

import qualified Data.Vector.Unboxed.Mutable as U

import Data.Vector.Algorithms.Common

import qualified Data.Vector.Algorithms.Insertion as I

sortBy :: (PrimMonad m, MVector v e)
       => Int               -- size of auxiliary arrays
       -> (Int -> e -> Int) -- big-endian radix
       -> v (PrimState m) e -- the array to be sorted
       -> m ()
sortBy buckets radix v = return ()
{-# INLINE sortBy #-}

flagLoop :: (PrimMonad m, MVector v e)
         => Comparison e
         -> Int                          -- which pass we're on
         -> (Int -> e -> Int)            -- radix function
         -> PV.MVector (PrimState m) Int -- auxiliary count array
         -> PV.MVector (PrimState m) Int -- auxiliary pile array
         -> v (PrimState m) e            -- source array
         -> m ()
flagLoop cmp pass radix count pile v
  | len < threshold = I.sortByBounds' cmp v 0 0 len
  | otherwise       = do body (radix pass) count pile v
                         recurse 0 0
 where
 len = length v
 recurse i b = do e <- unsafeRead count i
                  flagLoop cmp (pass+1) radix count pile (unsafeSlice b (e-b) v)
                  when (i < len) $ recurse (i+1) e

-- One pass of the flag sort
body :: (PrimMonad m, MVector v e)
     => (e -> Int)                   -- radix function
     -> PV.MVector (PrimState m) Int -- count array
     -> PV.MVector (PrimState m) Int -- pile array
     -> v (PrimState m) e            -- source array
     -> m ()
body rdx count pile v = do set count 0
                           countLoop rdx v count
                           accumulate count pile
                           permuteLoop rdx count pile v 0
{-# INLINE body #-}

accumulate :: (PrimMonad m)
           => PV.MVector (PrimState m) Int -> PV.MVector (PrimState m) Int -> m ()
accumulate count pile = loop 0 0
 where
 len = length count

 loop i acc
   | i < len = do ci <- unsafeRead count i
                  let acc' = acc + ci
                  unsafeWrite pile i acc
                  unsafeWrite count i acc'
                  loop (i+1) acc'
   | otherwise    = return ()
{-# INLINE accumulate #-}

permuteLoop :: (PrimMonad m, MVector v e)
            => (e -> Int)
            -> PV.MVector (PrimState m) Int
            -> PV.MVector (PrimState m) Int
            -> v (PrimState m) e
            -> Int
            -> m ()
permuteLoop rdx count pile v = go
 where
 len = length v
 clen = length count

 go i = do e <- unsafeRead v i
           j <- inc pile (rdx e)
           when (i /= j) $ follow i j e
           go (i+1)
 
 follow base cur e
   | base == cur = unsafeWrite v base e
   | otherwise   = do en <- unsafeRead v cur
                      next <- inc pile (rdx en)
                      unsafeWrite v cur e
                      follow base next en
{-# INLINE permuteLoop #-}

threshold :: Int
threshold = 25

