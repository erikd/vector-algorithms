{-# LANGUAGE TypeOperators #-}

module Util where

import Control.Monad
import Control.Monad.ST

import Data.Word
import Data.Int

import qualified Data.ByteString as B

import qualified Data.Vector as V

import Data.Vector.Mutable hiding (length)

import Test.QuickCheck


mfromList :: [e] -> ST s (MVector s e)
mfromList l = do v <- new (length l)
                 fill l 0 v
 where
 fill []     _ v = return v
 fill (x:xs) i v = do write v i x
                      fill xs (i+1) v

instance (Arbitrary e) => Arbitrary (V.Vector e) where
  arbitrary = fmap V.fromList arbitrary

instance Arbitrary B.ByteString where
  arbitrary = B.pack `fmap` arbitrary


-- | A large vector of 'Int' keys with a particular structure.
--
-- The default QuickCheck size limit keeps ordinary 'Arbitrary' vectors under
-- about 100 elements, which is too small to reach the run merging and
-- galloping code in Tim sort, the deeper merge levels of merge sort, or the
-- heap sort fallback in introsort. This generator produces vectors with
-- thousands of elements, in shapes that commonly expose sorting bugs.
data LargeVec = LargeVec String (V.Vector Int)

instance Show LargeVec where
  show (LargeVec shape v) =
    "LargeVec " ++ shape ++ " (length " ++ show (V.length v) ++ ") " ++ show v

instance Arbitrary LargeVec where
  arbitrary = do
    n <- choose (64, 5000)
    (shape, gen) <- elements largeShapes
    LargeVec shape . V.fromList <$> gen n

  -- Shrinking does not preserve the shape, but any smaller failing vector is
  -- still a valid (and more readable) counterexample.
  shrink (LargeVec shape v)
    | len < 2   = []
    | otherwise = [ LargeVec shape (V.take half v)
                  , LargeVec shape (V.drop half v)
                  , LargeVec shape (V.take (len - 1) v)
                  , LargeVec shape (V.drop 1 v)
                  ]
   where
   len  = V.length v
   half = len `div` 2

largeShapes :: [(String, Int -> Gen [Int])]
largeShapes =
  [ ("random",          \n -> vectorOf n (choose (-n, n)))
  , ("few-distinct",    \n -> vectorOf n (choose (0, 4)))
  , ("all-equal",       \n -> pure (Prelude.replicate n 7))
  , ("ascending",       \n -> pure [1 .. n])
  , ("ascending-dups",  \n -> scanl1 (+) <$> vectorOf n (choose (0, 1)))
  , ("descending",      \n -> pure [n, n - 1 .. 1])
  , ("descending-dups", \n -> scanl1 (-) <$> vectorOf n (choose (0, 1)))
  , ("organ-pipe",      \n -> let h = n `div` 2
                               in pure ([1 .. h] ++ [n - h, n - h - 1 .. 1]))
  , ("sawtooth",        \n -> do m <- choose (2, 200)
                                 pure [ i `mod` m | i <- [0 .. n - 1] ])
  , ("sorted-runs",     sortedRuns)
  , ("nearly-sorted",   nearlySorted)
  , ("sorted-tail",     \n -> do k <- choose (1, 20)
                                 t <- vectorOf k (choose (-n, n))
                                 pure ([1 .. n - k] ++ t))
  ]

-- | Concatenated ascending runs of random lengths. Runs drawn from disjoint
-- value ranges make Tim sort's galloping mode kick in.
sortedRuns :: Int -> Gen [Int]
sortedRuns n = go n
 where
 go 0 = pure []
 go r = do
   len  <- choose (1, min r 300)
   base <- choose (-n, n)
   step <- choose (0, 3)
   rest <- go (r - len)
   pure (Prelude.take len [base, base + step ..] ++ rest)

-- | An ascending sequence with a few random elements swapped.
nearlySorted :: Int -> Gen [Int]
nearlySorted n = do
  k     <- choose (1, 10)
  swaps <- vectorOf k ((,) <$> choose (0, n - 1) <*> choose (0, n - 1))
  -- Apply the swaps in turn, so the result is a permutation of [1 .. n].
  pure . V.toList $ V.modify (\mv -> Control.Monad.mapM_ (uncurry (swap mv)) swaps) (V.fromList [1 .. n])

-- | A vector together with a sub-range [l, u) of it and a count k with
-- 0 <= k <= u - l, for testing the @...ByBounds@ functions. The vector is
-- either a small arbitrary one or a 'LargeVec'.
data BoundsCase = BoundsCase (V.Vector Int) Int Int Int

instance Show BoundsCase where
  show (BoundsCase v l u k) =
    "BoundsCase (length " ++ show (V.length v) ++ ") l=" ++ show l
      ++ " u=" ++ show u ++ " k=" ++ show k ++ " " ++ show v

instance Arbitrary BoundsCase where
  arbitrary = do
    v <- oneof [ V.fromList <$> listOf (choose (-20, 20))
               , (\(LargeVec _ lv) -> lv) <$> arbitrary
               ]
    let n = V.length v
    l <- choose (0, n)
    u <- choose (l, n)
    k <- choose (0, u - l)
    pure (BoundsCase v l u k)

  shrink (BoundsCase v l u k) =
    [ BoundsCase (V.take u v) l u k | u < n ]
      ++ [ BoundsCase (V.take (u + (n - u) `div` 2) v) l u k | n - u > 1 ]
      ++ [ BoundsCase (V.drop l v) 0 (u - l) k | l > 0 ]
      ++ [ BoundsCase (V.drop (l `div` 2) v) (l - l `div` 2) (u - l `div` 2) k | l > 1 ]
      ++ [ BoundsCase (V.take a v V.++ V.drop b v) l (u - d) (min k (u - l - d))
         | (a, b) <- cuts, b > a, let d = b - a ]
      ++ [ BoundsCase v l u (k `div` 2) | k > 0 ]
   where
   n    = V.length v
   half = (u - l) `div` 2
   -- Remove the upper half, lower half or last element of [l, u). An empty
   -- range has nothing to cut, and (u - 1, u) would lie outside it.
   cuts | u > l     = [(l + half, u), (l, l + half), (u - 1, u)]
        | otherwise = []
