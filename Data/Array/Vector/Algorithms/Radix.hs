{-# LANGUAGE ScopedTypeVariables, BangPatterns, TypeOperators #-}

-- ---------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Vector.Algorithms.Radix
-- Copyright   : (c) 2008 Dan Doel
-- Maintainer  : Dan Doel <dan.doel@gmail.com>
-- Stability   : Experimental
-- Portability : Non-portable (scoped type variables, bang patterns)
--
-- This module provides a radix sort for a subclass of unboxed arrays. The 
-- radix class gives information on
--   * the number of passes needed for the data type
--
--   * the size of the auxiliary arrays
--
--   * how to compute the pass-k radix of a value
--
-- Radix sort is not a comparison sort, so it is able to achieve O(n) run
-- time, though it also uses O(n) auxiliary space. In addition, there is a
-- constant space overhead of 2*size*sizeOf(Int) for the sort, so it is not
-- advisable to use this sort for large numbers of very small arrays.
--
-- A standard example (upon which one could base their own Radix instance)
-- is Word32:
--
--   * We choose to sort on r = 8 bits at a time
--
--   * A Word32 has b = 32 bits total
--
--   Thus, b/r = 4 passes are required, 2^r = 256 elements are needed in an
--   auxiliary array, and the radix function is:
--
--    > radix k e = (e `shiftR` (k*8)) .&. 256

module Data.Array.Vector.Algorithms.Radix (sort, Radix(..)) where

import Control.Monad
import Control.Monad.ST

import Data.Array.Vector
import Data.Array.Vector.Algorithms.Common

import Data.Bits
import Data.Int
import Data.Word


import Foreign.Storable

class UA e => Radix e where
  -- | The number of passes necessary to sort an array of es
  passes :: e -> Int
  -- | The size of an auxiliary array
  size   :: e -> Int
  -- | The radix function parameterized by the current pass
  radix  :: Int -> e -> Int

instance Radix Int where
  passes _ = sizeOf (undefined :: Int)
  {-# INLINE passes #-}
  size _ = 256
  {-# INLINE size #-}
  radix 0 e = e .&. 255
  radix i e
    | i == passes e - 1 = radix' (e + minBound)
    | otherwise         = radix' e
   where radix' e = (e `shiftR` (i `shiftL` 3)) .&. 255
  {-# INLINE radix #-}

instance Radix Int8 where
  passes _ = 1
  {-# INLINE passes #-}
  size _ = 256
  {-# INLINE size #-}
  radix _ e = fromIntegral e + 128
  {-# INLINE radix #-}

instance Radix Int16 where
  passes _ = 2
  {-# INLINE passes #-}
  size _ = 256
  {-# INLINE size #-}
  radix 0 e = fromIntegral (e .&. 255)
  radix 1 e = fromIntegral (((e + minBound) `shiftR` 8) .&. 255)
  {-# INLINE radix #-}

instance Radix Int32 where
  passes _ = 4
  {-# INLINE passes #-}
  size _ = 256
  {-# INLINE size #-}
  radix 0 e = fromIntegral (e .&. 255)
  radix 1 e = fromIntegral ((e `shiftR` 8) .&. 255)
  radix 2 e = fromIntegral ((e `shiftR` 16) .&. 255)
  radix 3 e = fromIntegral (((e + minBound) `shiftR` 24) .&. 255)
  {-# INLINE radix #-}

instance Radix Int64 where
  passes _ = 8
  {-# INLINE passes #-}
  size _ = 256
  {-# INLINE size #-}
  radix 0 e = fromIntegral (e .&. 255)
  radix 1 e = fromIntegral ((e `shiftR` 8) .&. 255)
  radix 2 e = fromIntegral ((e `shiftR` 16) .&. 255)
  radix 3 e = fromIntegral ((e `shiftR` 24) .&. 255)
  radix 4 e = fromIntegral ((e `shiftR` 32) .&. 255)
  radix 5 e = fromIntegral ((e `shiftR` 40) .&. 255)
  radix 6 e = fromIntegral ((e `shiftR` 48) .&. 255)
  radix 7 e = fromIntegral (((e + minBound) `shiftR` 56) .&. 255)
  {-# INLINE radix #-}

instance Radix Word where
  passes _ = sizeOf (undefined :: Word)
  {-# INLINE passes #-}
  size _ = 256
  {-# INLINE size #-}
  radix 0 e = fromIntegral (e .&. 255)
  radix i e = fromIntegral ((e `shiftR` (i `shiftL` 3)) .&. 255)
  {-# INLINE radix #-}

instance Radix Word8 where
  passes _ = 1
  {-# INLINE passes #-}
  size _ = 256
  {-# INLINE size #-}
  radix _ = fromIntegral
  {-# INLINE radix #-}

instance Radix Word16 where
  passes _ = 2
  {-# INLINE passes #-}
  size   _ = 256
  {-# INLINE size #-}
  radix 0 e = fromIntegral (e .&. 255)
  radix 1 e = fromIntegral ((e `shiftR` 8) .&. 255)
  {-# INLINE radix #-}

instance Radix Word32 where
  passes _ = 4
  {-# INLINE passes #-}
  size   _ = 256
  {-# INLINE size #-}
  radix 0 e = fromIntegral (e .&. 255)
  radix 1 e = fromIntegral ((e `shiftR` 8) .&. 255)
  radix 2 e = fromIntegral ((e `shiftR` 16) .&. 255)
  radix 3 e = fromIntegral ((e `shiftR` 24) .&. 255)
  {-# INLINE radix #-}

instance Radix Word64 where
  passes _ = 8
  {-# INLINE passes #-}
  size   _ = 256
  {-# INLINE size #-}
  radix 0 e = fromIntegral (e .&. 255)
  radix 1 e = fromIntegral ((e `shiftR` 8) .&. 255)
  radix 2 e = fromIntegral ((e `shiftR` 16) .&. 255)
  radix 3 e = fromIntegral ((e `shiftR` 24) .&. 255)
  radix 4 e = fromIntegral ((e `shiftR` 32) .&. 255)
  radix 5 e = fromIntegral ((e `shiftR` 40) .&. 255)
  radix 6 e = fromIntegral ((e `shiftR` 48) .&. 255)
  radix 7 e = fromIntegral ((e `shiftR` 56) .&. 255)
  {-# INLINE radix #-}

instance (Radix i, Radix j) => Radix (i :*: j) where
  passes ~(i :*: j) = passes i + passes j
  {-# INLINE passes #-}
  size   ~(i :*: j) = size i `max` size j
  {-# INLINE size #-}
  radix k ~(i :*: j) | k < passes j = radix k j
                     | otherwise    = radix (k - passes j) i
  {-# INLINE radix #-}

-- | Sorts an array based on the Radix instance.
sort :: forall e s. Radix e => MUArr e s -> ST s ()
sort arr = do
  tmp    <- newMU len
  count  <- newMU (size e)
  prefix <- newMU (size e)
  radixLoop arr tmp count prefix (passes e)
 where
 len = lengthMU arr
 e :: e
 e = undefined
{-# INLINE sort #-}

radixLoop :: Radix e => MUArr e s -> MUArr e s -> MUArr Int s -> MUArr Int s -> Int -> ST s ()
radixLoop src dst count prefix passes = go False src dst 0
 where
 len = lengthMU src
 go !swap !src !dst k
   | k < passes = do zero count
                     countLoop k src count
                     writeMU prefix 0 0
                     prefixLoop count prefix
                     moveLoop k src dst prefix
                     go (not swap) dst src (k+1)
   | otherwise  = when swap (mcopyMU src dst 0 0 len)
{-# INLINE radixLoop #-}

zero :: MUArr Int s -> ST s ()
zero a = go 0
 where
 len = lengthMU a
 go i
   | i < len   = writeMU a i 0 >> go (i+1)
   | otherwise = return ()
{-# INLINE zero #-}

countLoop :: Radix e => Int -> MUArr e s -> MUArr Int s -> ST s ()
countLoop k src count = go 0
 where
 len = lengthMU src
 go i
   | i < len    = readMU src i >>= inc count . radix k >> go (i+1)
   | otherwise  = return ()
{-# INLINE countLoop #-}

prefixLoop :: MUArr Int s -> MUArr Int s -> ST s ()
prefixLoop count prefix = go 1 0
 where
 len = lengthMU count
 go i pi
   | i < len   = do ci <- readMU count (i-1)
                    let pi' = pi + ci
                    writeMU prefix i pi'
                    go (i+1) pi'
   | otherwise = return ()
{-# INLINE prefixLoop #-}

moveLoop :: Radix e => Int -> MUArr e s -> MUArr e s -> MUArr Int s -> ST s ()
moveLoop k src dst prefix = go 0
 where
 len = lengthMU src
 go i
   | i < len    = do srci <- readMU src i
                     pf   <- inc prefix (radix k srci)
                     writeMU dst pf srci
                     go (i+1)
   | otherwise  = return ()
{-# INLINE moveLoop #-}

inc :: MUArr Int s -> Int -> ST s Int
inc arr i = readMU arr i >>= \e -> writeMU arr i (e+1) >> return e
{-# INLINE inc #-}