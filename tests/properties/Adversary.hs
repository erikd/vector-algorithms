{-# LANGUAGE RankNTypes #-}
-- The comparison functions in this module have side effects (hidden behind
-- unsafePerformIO), so stop GHC from sharing or floating their calls.
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

-- | Worst-case inputs for quicksort-based algorithms, built with McIlroy's
-- adversary ("A Killer Adversary for Quicksort", Software: Practice and
-- Experience, 1999), and a way of counting comparisons.
--
-- The adversary runs the algorithm on a vector of item ids with a comparison
-- that decides item values lazily. Items start as "gas" (larger than any
-- decided value), and whenever two gas items are compared, one of them is
-- frozen to the next smallest value, choosing the one that looks like the
-- current pivot candidate. This makes every partition as unbalanced as
-- possible. Since the algorithm is deterministic, rerunning it on the values
-- the adversary decided reproduces exactly the same worst-case behaviour.
module Adversary
  ( CmpAlgo
  , killerInput
  , countComparisons
  ) where

import Control.Monad
import Control.Monad.ST (ST, stToIO)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

-- | An in-place algorithm parameterised by its comparison.
type CmpAlgo = forall s. (Int -> Int -> Ordering) -> MV.MVector s Int -> ST s ()

-- | An input of length @n@ that drives the given algorithm into its worst
-- case, as far as McIlroy's adversary can find one.
killerInput :: CmpAlgo -> Int -> V.Vector Int
killerInput algo n = unsafePerformIO $ do
  vals      <- MV.replicate n gas
  nsolid    <- newIORef 0
  candidate <- newIORef 0
  let freeze z = do s <- readIORef nsolid
                    MV.write vals z s
                    writeIORef nsolid (s + 1)
      cmpIO x y = do
        vx <- MV.read vals x
        vy <- MV.read vals y
        when (vx == gas && vy == gas) $ do
          c <- readIORef candidate
          freeze (if x == c then x else y)
        vx' <- MV.read vals x
        vy' <- MV.read vals y
        if vx' == gas
          then writeIORef candidate x
          else when (vy' == gas) $ writeIORef candidate y
        pure $! compare vx' vy'
      cmp x y = unsafePerformIO (cmpIO x y)
      {-# NOINLINE cmp #-}
  stToIO $ V.thaw (V.enumFromN 0 n) >>= algo cmp
  V.freeze vals
 where
 gas = n

-- | Run the algorithm on a copy of the input, returning the number of
-- comparisons it made and the resulting vector.
countComparisons :: CmpAlgo -> V.Vector Int -> (Int, V.Vector Int)
countComparisons algo v = unsafePerformIO $ do
  count <- newIORef (0 :: Int)
  let cmp x y = unsafePerformIO $ do modifyIORef' count (+ 1)
                                     pure $! compare x y
      {-# NOINLINE cmp #-}
  r <- stToIO $ do mv <- V.thaw v
                   algo cmp mv
                   V.freeze mv
  c <- readIORef count
  pure (c, r)
