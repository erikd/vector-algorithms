
module Properties where

import Control.Monad.ST

import Data.Array.Vector

import Data.Array.Vector.Algorithms.Combinators

import Test.QuickCheck

prop_sorted :: (UA e, Ord e) => UArr e -> Bool
prop_sorted arr | lengthU arr < 2 = True
                | otherwise       = check (headU arr) (tailU arr)
 where
 check e arr | nullU arr = True
             | otherwise = e < headU arr && check (headU arr) (tailU arr)

prop_fullsort :: (UA e, Ord e)
              => (forall s. MUArr e s -> ST s ()) -> UArr e -> Bool
prop_fullsort algo arr = prop_sorted $ apply algo arr

prop_partialsort :: (UA e, Ord e)
                 => (forall s. Int -> MUArr e s -> ST s ())
                 -> Int -> UArr e -> Property
prop_partialsort algo k arr = k <= lengthU arr ==>
                                (prop_sorted . takeU k . apply (algo k) $ arr)

prop_select :: (UA e, Ord e)
            => (forall s. Int -> MUArr e s -> ST s ())
            -> Int -> UArr e -> Property
prop_select algo k arr = k <= lengthU arr ==>
                         let arr' = apply (algo k) arr
                             (l, r) = splitAtU k arr'
                         in allU (\e -> allU (e<=) r) l

