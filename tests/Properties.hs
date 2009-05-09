{-# LANGUAGE Rank2Types #-}

module Properties where

import Optimal

import Control.Monad
import Control.Monad.ST

import Data.List
import Data.Ord

import Data.Array.Vector

import Data.Array.Vector.Algorithms.Optimal (Comparison)
import Data.Array.Vector.Algorithms.Radix
import Data.Array.Vector.Algorithms.Combinators

import Test.QuickCheck

import Util

prop_sorted :: (UA e, Ord e) => UArr e -> Property
prop_sorted arr | lengthU arr < 2 = property True
                | otherwise       = check (headU arr) (tailU arr)
 where
 check e arr | nullU arr = property True
             | otherwise = e <= headU arr .&. check (headU arr) (tailU arr)

prop_fullsort :: (UA e, Ord e)
              => (forall s. MUArr e s -> ST s ()) -> UArr e -> Property
prop_fullsort algo arr = prop_sorted $ apply algo arr

prop_schwartzian :: (UA e, UA k, Ord k)
                 => (e -> k)
                 -> (forall e s. (UA e) => (e -> e -> Ordering) -> MUArr e s -> ST s ())
                 -> UArr e -> Property
prop_schwartzian f algo arr
  | lengthU arr < 2 = property True
  | otherwise       = let srt = apply (algo `usingKeys` f) arr
                      in check (headU srt) (tailU srt)
 where
 check e arr | nullU arr = property True
             | otherwise = f e <= f (headU arr) .&. check (headU arr) (tailU arr)

longGen :: (UA e, Arbitrary e) => Int -> Gen (UArr e)
longGen k = liftM2 (\l r -> toU (l ++ r)) (vectorOf k arbitrary) arbitrary

sanity :: Int
sanity = 100

prop_partialsort :: (UA e, Ord e, Arbitrary e, Show e)
                 => (forall s. MUArr e s -> Int -> ST s ())
                 -> Positive Int -> Property
prop_partialsort algo (Positive k) =
  let k' = k `mod` sanity
  in forAll (longGen k') $
       prop_sorted . takeU k' . apply (\marr -> algo marr k')


prop_select :: (UA e, Ord e, Arbitrary e, Show e)
            => (forall s. MUArr e s -> Int -> ST s ())
            -> Positive Int -> Property
prop_select algo (Positive k) =
  let k' = k `mod` sanity
  in forAll (longGen k') $ \arr ->
       let arr' = apply (\marr -> algo marr k') arr
           (l, r) = splitAtU k' arr'
       in allU (\e -> allU (e<=) r) l

prop_stable :: (forall e s. (UA e) => Comparison e -> MUArr e s -> ST s ())
            -> UArr Int -> Property
-- prop_stable algo arr = property $ apply algo arr == arr
prop_stable algo arr = stable $ apply (algo (comparing fstS)) $ zipU arr ix
 where
 ix = toU [1 .. lengthU arr]

stable arr | nullU arr = property True
           | otherwise = let e :*: i = headU arr
                         in allU (\(e' :*: i') -> e < e' || i < i') (tailU arr)
                            .&. stable (tailU arr)

prop_stable_radix :: (forall e s. UA e => 
                                  Int -> Int -> (Int -> e -> Int) -> MUArr e s -> ST s ())
                  -> UArr Int -> Property
prop_stable_radix algo arr =
  stable . apply (algo (passes e) (size e) (\k (e :*: _) -> radix k e))
         $ zipU arr ix
 where
 ix = toU [1 .. lengthU arr]
 e = headU arr
 


prop_optimal :: Int
             -> (forall e s. (UA e) => Comparison e -> MUArr e s -> Int -> ST s ())
             -> Property
prop_optimal n algo = label "sorting" sortn .&. label "stability" stabn
 where
 arrn  = toU [0..n-1]
 sortn = all ( (== arrn)
             . apply (\a -> algo compare a 0)
             . toU)
         $ permutations [0..n-1]
 stabn = all ( (== arrn)
             . sndS
             . unzipU
             . apply (\a -> algo (comparing fstS) a 0))
         $ stability n
