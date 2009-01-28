{-# LANGUAGE Rank2Types #-}

module Blocks where

import Control.Monad
import Control.Monad.ST

import Data.Array.Vector

import System.CPUTime

-- Some conveniences for doing evil stuff in the ST monad.
-- All the tests get run in IO, but uvector stuff happens
-- in ST, so we temporarily coerce.
clock :: ST s Integer
clock = unsafeIOToST getCPUTime

-- Strategies for filling the initial arrays
rand :: (MTRandom e) => MTGen -> Int -> ST s e
rand g _ = unsafeIOToST (random g)

ascend :: Num e => Int -> ST s e
ascend = return . fromIntegral

descend :: Num e => e -> Int -> ST s e
descend m n = return $ m - fromIntegral n

modulo :: Num e => e -> Int -> ST s e
modulo m n = return $ fromIntegral n `mod` m

-- This is the worst case for the median-of-three quicksort
-- used in the introsort implementation.
medianKiller :: Num e => e -> Int -> ST s e
medianKiller m n'
  | n < k     = return $ if even n then n + 1 else n + k
  | otherwise = return $ (n - k + 1) * 2
 where
 n = fromIntegral n'
 k = m `div` 2
{-# INLINE medianKiller #-}

initialize :: (UA e) => MUArr es -> Int -> (Int -> ST s e) -> ST s ()
initialize arr len fill = init $ len - 1
 where init n = fill n >>= writeMU arr n >> when (n > 0) (init $ n - 1)
{-# INLINE initialize #-}

speedTest :: (UA e) => Int
                    -> (forall s. Int -> ST s e)
                    -> (forall s. MUArr e s -> ST s ())
                    -> IO Integer
speedTest n fill algo = stToIO $ do
  arr <- newMU n
  initialize arr n fill
  t0 <- clock
  algo arr
  t1 <- clock
  return $ t1 - t0
{-# INLINE speedTest #-}


