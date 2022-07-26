{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- ---------------------------------------------------------------------------
-- |
-- Module      : Data.Vector.Algorithms.Common
-- Copyright   : (c) 2008-2011 Dan Doel
-- Maintainer  : Dan Doel
-- Stability   : Experimental
-- Portability : Portable
--
-- Common operations and utility functions for all sorts

module Data.Vector.Algorithms.Common
  ( type Comparison
  , copyOffset
  , inc
  , countLoop
  , midPoint
  , uniqueMutableBy
  )
  where

import Prelude hiding (read, length)

import Control.Monad.Primitive

import Data.Vector.Generic.Mutable
import Data.Word (Word)

import qualified Data.Vector.Primitive.Mutable as PV

-- | A type of comparisons between two values of a given type.
type Comparison e = e -> e -> Ordering

copyOffset :: (PrimMonad m, MVector v e)
           => v (PrimState m) e -> v (PrimState m) e -> Int -> Int -> Int -> m ()
copyOffset from to iFrom iTo len =
  unsafeCopy (unsafeSlice iTo len to) (unsafeSlice iFrom len from)
{-# INLINE copyOffset #-}

inc :: (PrimMonad m, MVector v Int) => v (PrimState m) Int -> Int -> m Int
inc arr i = unsafeRead arr i >>= \e -> unsafeWrite arr i (e+1) >> return e
{-# INLINE inc #-}

-- shared bucket sorting stuff
countLoop :: (PrimMonad m, MVector v e)
          => (e -> Int)
          -> v (PrimState m) e -> PV.MVector (PrimState m) Int -> m ()
countLoop rdx src count = set count 0 >> go 0
 where
 len = length src
 go i
   | i < len    = unsafeRead src i >>= inc count . rdx >> go (i+1)
   | otherwise  = return ()
{-# INLINE countLoop #-}

midPoint :: Int -> Int -> Int
midPoint a b =
  toInt $ (toWord a + toWord b) `div` 2
  where
    toWord :: Int -> Word
    toWord = fromIntegral

    toInt :: Word -> Int
    toInt = fromIntegral
{-# INLINE midPoint #-}

-- Adapted from Andrew Martin's uniquqMutable in the primitive-sort package
uniqueMutableBy :: forall m v a . (PrimMonad m, MVector v a)
  => Comparison a -> v (PrimState m) a -> m (v (PrimState m) a)
uniqueMutableBy cmp mv = do
  let !len = basicLength mv
  if len > 1
    then do
      !a0 <- unsafeRead mv 0
      let findFirstDuplicate :: a -> Int -> m Int
          findFirstDuplicate !prev !ix = if ix < len
            then do
              a <- unsafeRead mv ix
              if cmp a prev == EQ
                then return ix
                else findFirstDuplicate a (ix + 1)
            else return ix
      dupIx <- findFirstDuplicate a0 1
      if dupIx == len
        then return mv
        else do
          let deduplicate :: a -> Int -> Int -> m Int
              deduplicate !prev !srcIx !dstIx = if srcIx < len
                then do
                  a <- unsafeRead mv srcIx
                  if cmp a prev == EQ
                    then deduplicate a (srcIx + 1) dstIx
                    else do
                      unsafeWrite mv dstIx a
                      deduplicate a (srcIx + 1) (dstIx + 1)
                else return dstIx
          !a <- unsafeRead mv dupIx
          !reducedLen <- deduplicate a (dupIx + 1) dupIx
          resizeVector mv reducedLen
    else return mv
{-# INLINABLE uniqueMutableBy #-}

-- Used internally in uniqueMutableBy: copies the elements of a vector to one
-- of a smaller size.
resizeVector
  :: (MVector v a, PrimMonad m)
  =>  v (PrimState m) a -> Int -> m (v (PrimState m) a)
resizeVector !src !sz = do
  dst <- unsafeNew sz
  copyToSmaller dst src
  pure dst
{-# inline resizeVector #-}

-- Used internally in resizeVector: copy a vector from a larger to
-- smaller vector. Should not be used if the source vector
-- is smaller than the target vector.
copyToSmaller
  :: (MVector v a, PrimMonad m)
  => v (PrimState m) a -> v (PrimState m) a -> m ()
copyToSmaller !dst !src = stToPrim $ do_copy 0
    where
      !n = basicLength dst

      do_copy i | i < n = do
                            x <- basicUnsafeRead src i
                            basicUnsafeWrite dst i x
                            do_copy (i+1)
                | otherwise = return ()
