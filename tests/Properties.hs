
module Properties where

import Control.Monad
import Control.Monad.ST

import Data.Array.Vector

import Data.Array.Vector.Algorithms.Combinators

import Test.QuickCheck

prop_sorted :: (UA e, Ord e) => UArr e -> Property
prop_sorted arr | lengthU arr < 2 = property True
                | otherwise       = check (headU arr) (tailU arr)
 where
 check e arr | nullU arr = property True
             | otherwise = e <= headU arr .&. check (headU arr) (tailU arr)

prop_fullsort :: (UA e, Ord e)
              => (forall s. MUArr e s -> ST s ()) -> UArr e -> Property
prop_fullsort algo arr = prop_sorted $ apply algo arr

longGen :: (UA e, Arbitrary e) => Int -> Gen (UArr e)
longGen k = liftM2 (\l r -> toU (l ++ r)) (vectorOf k arbitrary) arbitrary

prop_partialsort :: (UA e, Ord e, Arbitrary e, Show e)
                 => (forall s. Int -> MUArr e s -> ST s ())
                 -> Int -> Property
prop_partialsort algo k = forAll (longGen k) $
                            prop_sorted . takeU k . apply (algo k)

prop_select :: (UA e, Ord e, Arbitrary e, Show e)
            => (forall s. Int -> MUArr e s -> ST s ())
            -> Int -> Property
prop_select algo k = forAll (longGen k) $ \arr ->
                         let arr' = apply (algo k) arr
                             (l, r) = splitAtU k arr'
                         in allU (\e -> allU (e<=) r) l

