{-# language BangPatterns, RankNTypes, ScopedTypeVariables #-}
module Data.Vector.Algorithms where

import Prelude hiding (length)
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST (runST)

import Data.Vector.Generic.Mutable
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed.Mutable as UMV
import qualified Data.Bit as Bit

import Data.Vector.Algorithms.Common (Comparison)
import Data.Vector.Algorithms.Intro (sortUniqBy)
import qualified Data.Vector.Algorithms.Search  as S

-- | The `nub` function which removes duplicate elements from a vector.
nub :: forall v e . (V.Vector v e, Ord e) => v e -> v e
nub = nubBy compare
{-# INLINE nub #-}

-- | A version of `nub` with a custom comparison predicate.
--
-- /Note:/ This function makes use of `sortByUniq` using the intro
-- sort algorithm.
nubBy ::
  forall v e . (V.Vector v e) =>
  Comparison e -> v e -> v e
nubBy cmp vec = runST $ do
  mv <- V.unsafeThaw vec -- safe as the nubByMut algorithm copies the input
  destMV <- nubByMut sortUniqBy cmp mv
  v <- V.unsafeFreeze destMV
  pure (V.force v)
{-# INLINE nubBy #-}

-- | The `nubByMut` function takes in an in-place sort algorithm
-- and uses it to do a de-deduplicated sort. It then uses this to
-- remove duplicate elements from the input.
--
-- /Note:/ Since this algorithm needs the original input and so
-- copies before sorting in-place. As such, it is safe to use on
-- immutable inputs.
nubByMut ::
  forall m v e . (PrimMonad m, MVector v e) =>
  (Comparison e -> v (PrimState m) e -> m (v (PrimState m) e))
  -> Comparison e -> v (PrimState m) e -> m (v (PrimState m) e)
nubByMut alg cmp inp = do
  let len = length inp
  inp' <- clone inp
  sortUniqs <- alg cmp inp'
  let uniqLen = length sortUniqs
  bitmask <- UMV.replicate uniqLen (Bit.Bit False) -- bitmask to track which elements have
                                                   -- already been seen.
  dest ::  v (PrimState m) e <- unsafeNew uniqLen  -- return vector
  let
    go :: Int -> Int -> m ()
    go !srcInd !destInd
      | srcInd == len = pure ()
      | destInd == uniqLen = pure ()
      | otherwise = do
          curr    <- unsafeRead inp srcInd                -- read current element
          sortInd <- S.binarySearchBy cmp sortUniqs curr  -- find sorted index
          bit <- UMV.unsafeRead bitmask sortInd           -- check if we have already seen
                                                          -- this element in bitvector
          case bit of
            -- if we have seen it then iterate
            Bit.Bit True -> go (srcInd + 1) destInd
            -- if we haven't then write it into output
            -- and mark that it has been seen
            Bit.Bit False -> do
              UMV.unsafeWrite bitmask sortInd (Bit.Bit True)
              unsafeWrite dest destInd curr
              go (srcInd + 1) (destInd + 1)
  go 0 0
  pure dest
{-# INLINABLE nubByMut #-}
