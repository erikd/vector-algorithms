{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# lANGUAGE ScopedTypeVariables #-}

-- ---------------------------------------------------------------------------
-- |
-- Module      : Data.Vector.Algorithms.AmericanFlag
-- Copyright   : (c) 2011 Dan Doel
-- Maintainer  : Dan Doel <dan.doel@gmail.com>
-- Stability   : Experimental
-- Portability : Non-portable (FlexibleContexts, ScopedTypeVariables)
--
-- This module implements American flag sort: an in-place, unstable, bucket
-- sort. Also in contrast to radix sort, the values are inspected in a big
-- endian order, and buckets are sorted via recursive splitting. This,
-- however, makes it sensible for sorting strings in lexicographic order
-- (provided indexing is fast).
--
-- The algorithm works as follows: at each stage, the array is looped over,
-- counting the number of elements for each bucket. Then, starting at the
-- beginning of the array, elements are permuted in place to reside in the
-- proper bucket, following chains until they reach back to the current
-- base index. Finally, each bucket is sorted recursively. This lends itself
-- well to the aforementioned variable-length strings, and so the algorithm
-- takes a stopping predicate, which is given a representative of the stripe,
-- rather than running for a set number of iterations.

module Data.Vector.Algorithms.AmericanFlag ( sort
                                           , sortBy
                                           , terminate
                                           , Lexicographic(..)
                                           ) where

import Prelude hiding (read, length)

import Control.Monad
import Control.Monad.Primitive

import Data.Proxy

import Data.Word
import Data.Int
import Data.Bits

import qualified Data.ByteString as B

import Data.Vector.Generic.Mutable
import qualified Data.Vector.Primitive.Mutable as PV

import qualified Data.Vector.Unboxed.Mutable as U

import Data.Vector.Algorithms.Common

import qualified Data.Vector.Algorithms.Insertion as I

import Foreign.Storable

-- | The methods of this class specify the information necessary to sort
-- arrays using the default ordering. The name 'Lexicographic' is meant
-- to convey that index should return results in a similar way to indexing
-- into a string.
class Lexicographic e where
  -- | Computes the length of a representative of a stripe. It should take 'n'
  -- passes to sort values of extent 'n'. The extent may not be uniform across
  -- all values of the type.
  extent    :: e -> Int

  -- | The size of the bucket array necessary for sorting es
  size      :: Proxy e -> Int
  -- | Determines which bucket a given element should inhabit for a
  -- particular iteration.
  index     :: Int -> e -> Int

instance Lexicographic Word8 where
  extent _ = 1
  {-# INLINE extent #-}
  size _ = 256
  {-# INLINE size #-}
  index _ n = fromIntegral n
  {-# INLINE index #-}

instance Lexicographic Word16 where
  extent _ = 2
  {-# INLINE extent #-}
  size _ = 256
  {-# INLINE size #-}
  index 0 n = fromIntegral $ (n `shiftR`  8) .&. 255
  index 1 n = fromIntegral $ n .&. 255
  index _ _ = 0
  {-# INLINE index #-}

instance Lexicographic Word32 where
  extent _ = 4
  {-# INLINE extent #-}
  size _ = 256
  {-# INLINE size #-}
  index 0 n = fromIntegral $ (n `shiftR` 24) .&. 255
  index 1 n = fromIntegral $ (n `shiftR` 16) .&. 255
  index 2 n = fromIntegral $ (n `shiftR`  8) .&. 255
  index 3 n = fromIntegral $ n .&. 255
  index _ _ = 0
  {-# INLINE index #-}

instance Lexicographic Word64 where
  extent _ = 8
  {-# INLINE extent #-}
  size _ = 256
  {-# INLINE size #-}
  index 0 n = fromIntegral $ (n `shiftR` 56) .&. 255
  index 1 n = fromIntegral $ (n `shiftR` 48) .&. 255
  index 2 n = fromIntegral $ (n `shiftR` 40) .&. 255
  index 3 n = fromIntegral $ (n `shiftR` 32) .&. 255
  index 4 n = fromIntegral $ (n `shiftR` 24) .&. 255
  index 5 n = fromIntegral $ (n `shiftR` 16) .&. 255
  index 6 n = fromIntegral $ (n `shiftR`  8) .&. 255
  index 7 n = fromIntegral $ n .&. 255
  index _ _ = 0
  {-# INLINE index #-}

instance Lexicographic Word where
  extent _ = sizeOf (0 :: Word)
  {-# INLINE extent #-}
  size _ = 256
  {-# INLINE size #-}
  index 0 n = fromIntegral $ (n `shiftR` 56) .&. 255
  index 1 n = fromIntegral $ (n `shiftR` 48) .&. 255
  index 2 n = fromIntegral $ (n `shiftR` 40) .&. 255
  index 3 n = fromIntegral $ (n `shiftR` 32) .&. 255
  index 4 n = fromIntegral $ (n `shiftR` 24) .&. 255
  index 5 n = fromIntegral $ (n `shiftR` 16) .&. 255
  index 6 n = fromIntegral $ (n `shiftR`  8) .&. 255
  index 7 n = fromIntegral $ n .&. 255
  index _ _ = 0
  {-# INLINE index #-}

instance Lexicographic Int8 where
  extent _ = 1
  {-# INLINE extent #-}
  size _ = 256
  {-# INLINE size #-}
  index _ n = 255 .&. fromIntegral n `xor` 128
  {-# INLINE index #-}

instance Lexicographic Int16 where
  extent _ = 2
  {-# INLINE extent #-}
  size _ = 256
  {-# INLINE size #-}
  index 0 n = fromIntegral $ ((n `xor` minBound) `shiftR` 8) .&. 255
  index 1 n = fromIntegral $ n .&. 255
  index _ _ = 0
  {-# INLINE index #-}

instance Lexicographic Int32 where
  extent _ = 4
  {-# INLINE extent #-}
  size _ = 256
  {-# INLINE size #-}
  index 0 n = fromIntegral $ ((n `xor` minBound) `shiftR` 24) .&. 255
  index 1 n = fromIntegral $ (n `shiftR` 16) .&. 255
  index 2 n = fromIntegral $ (n `shiftR`  8) .&. 255
  index 3 n = fromIntegral $ n .&. 255
  index _ _ = 0
  {-# INLINE index #-}

instance Lexicographic Int64 where
  extent _ = 8
  {-# INLINE extent #-}
  size _ = 256
  {-# INLINE size #-}
  index 0 n = fromIntegral $ ((n `xor` minBound) `shiftR` 56) .&. 255
  index 1 n = fromIntegral $ (n `shiftR` 48) .&. 255
  index 2 n = fromIntegral $ (n `shiftR` 40) .&. 255
  index 3 n = fromIntegral $ (n `shiftR` 32) .&. 255
  index 4 n = fromIntegral $ (n `shiftR` 24) .&. 255
  index 5 n = fromIntegral $ (n `shiftR` 16) .&. 255
  index 6 n = fromIntegral $ (n `shiftR`  8) .&. 255
  index 7 n = fromIntegral $ n .&. 255
  index _ _ = 0
  {-# INLINE index #-}

instance Lexicographic Int where
  extent _ = sizeOf (0 :: Int)
  {-# INLINE extent #-}
  size _ = 256
  {-# INLINE size #-}
  index 0 n = ((n `xor` minBound) `shiftR` 56) .&. 255
  index 1 n = (n `shiftR` 48) .&. 255
  index 2 n = (n `shiftR` 40) .&. 255
  index 3 n = (n `shiftR` 32) .&. 255
  index 4 n = (n `shiftR` 24) .&. 255
  index 5 n = (n `shiftR` 16) .&. 255
  index 6 n = (n `shiftR`  8) .&. 255
  index 7 n = n .&. 255
  index _ _ = 0
  {-# INLINE index #-}

instance Lexicographic B.ByteString where
  extent = B.length
  {-# INLINE extent #-}
  size _ = 257
  {-# INLINE size #-}
  index i b
    | i >= B.length b = 0
    | otherwise       = fromIntegral (B.index b i) + 1
  {-# INLINE index #-}

instance (Lexicographic a, Lexicographic b) => Lexicographic (a, b) where
  extent (a,b) = extent a + extent b
  {-# INLINE extent #-}
  size _ = size (Proxy :: Proxy a) `max` size (Proxy :: Proxy b)
  {-# INLINE size #-}
  index i (a,b)
    | i >= extent a = index i b
    | otherwise     = index i a
  {-# INLINE index #-}

instance (Lexicographic a, Lexicographic b) => Lexicographic (Either a b) where
  extent (Left  a) = 1 + extent a
  extent (Right b) = 1 + extent b
  {-# INLINE extent #-}
  size _ = size (Proxy :: Proxy a) `max` size (Proxy :: Proxy b)
  {-# INLINE size #-}
  index 0 (Left  _) = 0
  index 0 (Right _) = 1
  index n (Left  a) = index (n-1) a
  index n (Right b) = index (n-1) b
  {-# INLINE index #-}

-- | Given a representative of a stripe and an index number, this
-- function determines whether to stop sorting.
terminate :: Lexicographic e => e -> Int -> Bool
terminate e i = i >= extent e
{-# INLINE terminate #-}

-- | Sorts an array using the default ordering. Both Lexicographic and
-- Ord are necessary because the algorithm falls back to insertion sort
-- for sufficiently small arrays.
sort :: forall e m v. (PrimMonad m, MVector v e, Lexicographic e, Ord e)
     => v (PrimState m) e -> m ()
sort v = sortBy compare terminate (size p) index v
 where p :: Proxy e
       p = Proxy
{-# INLINABLE sort #-}

-- | A fully parameterized version of the sorting algorithm. Again, this
-- function takes both radix information and a comparison, because the
-- algorithms falls back to insertion sort for small arrays.
sortBy :: (PrimMonad m, MVector v e)
       => Comparison e       -- ^ a comparison for the insertion sort flalback
       -> (e -> Int -> Bool) -- ^ determines whether a stripe is complete
       -> Int                -- ^ the number of buckets necessary
       -> (Int -> e -> Int)  -- ^ the big-endian radix function
       -> v (PrimState m) e  -- ^ the array to be sorted
       -> m ()
sortBy cmp stop buckets radix v
  | length v == 0 = return ()
  | otherwise     = do count <- new buckets
                       pile <- new buckets
                       countLoop (radix 0) v count
                       flagLoop cmp stop radix count pile v
{-# INLINE sortBy #-}

flagLoop :: (PrimMonad m, MVector v e)
         => Comparison e
         -> (e -> Int -> Bool)           -- number of passes
         -> (Int -> e -> Int)            -- radix function
         -> PV.MVector (PrimState m) Int -- auxiliary count array
         -> PV.MVector (PrimState m) Int -- auxiliary pile array
         -> v (PrimState m) e            -- source array
         -> m ()
flagLoop cmp stop radix count pile v = go 0 v
 where

 go pass v = do e <- unsafeRead v 0
                unless (stop e $ pass - 1) $ go' pass v

 go' pass v
   | len < threshold = I.sortByBounds cmp v 0 len
   | otherwise       = do accumulate count pile
                          permute (radix pass) count pile v
                          recurse 0
  where
  len = length v
  ppass = pass + 1

  recurse i
    | i < len   = do j <- countStripe (radix ppass) (radix pass) count v i
                     go ppass (unsafeSlice i (j - i) v)
                     recurse j
    | otherwise = return ()
{-# INLINE flagLoop #-}

accumulate :: (PrimMonad m)
           => PV.MVector (PrimState m) Int
           -> PV.MVector (PrimState m) Int
           -> m ()
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

permute :: (PrimMonad m, MVector v e)
        => (e -> Int)                       -- radix function
        -> PV.MVector (PrimState m) Int     -- count array
        -> PV.MVector (PrimState m) Int     -- pile array
        -> v (PrimState m) e                -- source array
        -> m ()
permute rdx count pile v = go 0
 where
 len = length v

 go i
   | i < len   = do e <- unsafeRead v i
                    let r = rdx e
                    p <- unsafeRead pile r
                    m <- if r > 0
                            then unsafeRead count (r-1)
                            else return 0
                    case () of
                      -- if the current element is already in the right pile,
                      -- go to the end of the pile
                      _ | m <= i && i < p  -> go p
                      -- if the current element happens to be in the right
                      -- pile, bump the pile counter and go to the next element
                        | i == p           -> unsafeWrite pile r (p+1) >> go (i+1)
                      -- otherwise follow the chain
                        | otherwise        -> follow i e p >> go (i+1)
   | otherwise = return ()
 
 follow i e j = do en <- unsafeRead v j
                   let r = rdx en
                   p <- inc pile r
                   if p == j
                      -- if the target happens to be in the right pile, don't move it.
                      then follow i e (j+1)
                      else unsafeWrite v j e >> if i == p
                                             then unsafeWrite v i en
                                             else follow i en p
{-# INLINE permute #-}

countStripe :: (PrimMonad m, MVector v e)
            => (e -> Int)                   -- radix function
            -> (e -> Int)                   -- stripe function
            -> PV.MVector (PrimState m) Int -- count array
            -> v (PrimState m) e            -- source array
            -> Int                          -- starting position
            -> m Int                        -- end of stripe: [lo,hi)
countStripe rdx str count v lo = do set count 0
                                    e <- unsafeRead v lo
                                    go (str e) e (lo+1)
 where
 len = length v

 go !s e i = inc count (rdx e) >>
            if i < len
               then do en <- unsafeRead v i
                       if str en == s
                          then go s en (i+1)
                          else return i
                else return len
{-# INLINE countStripe #-}

threshold :: Int
threshold = 25

