{-# LANGUAGE RankNTypes, TypeOperators, FlexibleContexts, TypeApplications #-}

module Main (main) where

import Properties

import Adversary (CmpAlgo)

import Util

import Test.QuickCheck hiding (quickCheck, quickCheckWith)
import qualified Test.QuickCheck as QC

import Data.IORef
import System.Exit (exitFailure)
import System.IO.Unsafe (unsafePerformIO)

import Control.Monad
import Control.Monad.ST

import Data.Int
import Data.Word
import Data.Ord (Down (..), comparing)

import qualified Data.ByteString as B

import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as BoxedMV

import qualified Data.Vector.Generic as G
import Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector.Generic.Mutable as MV

import qualified Data.Vector.Algorithms.Insertion    as INS
import qualified Data.Vector.Algorithms.Intro        as INT
import qualified Data.Vector.Algorithms.Merge        as M
import qualified Data.Vector.Algorithms.Radix        as R
import qualified Data.Vector.Algorithms.Heap         as H
import qualified Data.Vector.Algorithms.Optimal      as O
import qualified Data.Vector.Algorithms.AmericanFlag as AF
import qualified Data.Vector.Algorithms.Tim          as T

import qualified Data.Vector.Algorithms.Search       as SR

type Algo      e r = forall s mv. MVector mv e => mv s e -> ST s r
type SizeAlgo  e r = forall s mv. MVector mv e => mv s e -> Int -> ST s r
type BoundAlgo e r = forall s mv. MVector mv e => mv s e -> Int -> Int -> ST s r
type MonoAlgo  e r = forall s . BoxedMV.MVector s e -> ST s r

newtype WrappedAlgo      e r = WrapAlgo      { unWrapAlgo      :: Algo      e r }
newtype WrappedSizeAlgo  e r = WrapSizeAlgo  { unWrapSizeAlgo  :: SizeAlgo  e r }
newtype WrappedBoundAlgo e r = WrapBoundAlgo { unWrapBoundAlgo :: BoundAlgo e r }
newtype WrappedMonoAlgo  e r = MonoAlgo      { unWrapMonoAlgo  :: MonoAlgo  e r }

args = stdArgs
       { maxSuccess = 1000
       , maxDiscardRatio = 2
       }

check_Int_sort = forM_ algos $ \(name,algo) ->
  quickCheckWith args (label name . prop_fullsort (unWrapAlgo algo))
 where
 algos :: [(String, WrappedAlgo Int ())]
 algos = [ ("introsort", WrapAlgo INT.sort)
         , ("insertion sort", WrapAlgo INS.sort)
         , ("merge sort", WrapAlgo M.sort)
         , ("heapsort", WrapAlgo H.sort)
         , ("timsort", WrapAlgo T.sort)
         ]

check_Int_sortUniq = forM_ algos $ \(name,algo) ->
  quickCheckWith args (label name . prop_full_sortUniq (unWrapMonoAlgo algo))
 where
 algos :: [(String, WrappedMonoAlgo Int (Vector Int))]
 algos = [ ("intro_sortUniq", MonoAlgo (runFreeze INT.sortUniq))
         , ("insertion sortUniq", MonoAlgo (runFreeze INS.sortUniq))
         , ("merge sortUniq", MonoAlgo (runFreeze M.sortUniq))
         , ("heap_sortUniq", MonoAlgo (runFreeze H.sortUniq))
         , ("tim_sortUniq", MonoAlgo (runFreeze T.sortUniq))
         , ("flag_sortUniq", MonoAlgo (runFreeze AF.sortUniq))
         ]

check_Int_partialsort = forM_ algos $ \(name,algo) ->
  quickCheckWith args (label name . prop_partialsort (unWrapSizeAlgo algo))
 where
 algos :: [(String, WrappedSizeAlgo Int ())]
 algos = [ ("intro-partialsort", WrapSizeAlgo INT.partialSort)
         , ("heap partialsort", WrapSizeAlgo H.partialSort)
         ]

check_Int_select = forM_ algos $ \(name,algo) ->
  quickCheckWith args (label name . prop_select (unWrapSizeAlgo algo))
 where
 algos :: [(String, WrappedSizeAlgo Int ())]
 algos = [ ("intro-select", WrapSizeAlgo INT.select)
         , ("heap select", WrapSizeAlgo H.select)
         ]

check_large = do
  forM_ sorts $ \(name, algo) ->
    qc (label name . prop_large_sort (unWrapAlgo algo))
  qc (label "merge sort stable" . prop_large_stable M.sortBy)
  qc (label "timsort stable" . prop_large_stable T.sortBy)
  forM_ uniqs $ \(name, algo) ->
    qc (label name . prop_large_sortUniq (unWrapMonoAlgo algo))
 where
 qc = quickCheckWith args { maxSuccess = 300 }
 sorts :: [(String, WrappedAlgo Int ())]
 sorts = [ ("introsort", WrapAlgo INT.sort)
         , ("merge sort", WrapAlgo M.sort)
         , ("heapsort", WrapAlgo H.sort)
         , ("timsort", WrapAlgo T.sort)
         , ("radix sort", WrapAlgo R.sort)
         , ("american flag sort", WrapAlgo AF.sort)
         ]
 uniqs :: [(String, WrappedMonoAlgo Int (Vector Int))]
 uniqs = [ ("intro sortUniq", MonoAlgo (runFreeze INT.sortUniq))
         , ("merge sortUniq", MonoAlgo (runFreeze M.sortUniq))
         , ("heap sortUniq", MonoAlgo (runFreeze H.sortUniq))
         , ("tim sortUniq", MonoAlgo (runFreeze T.sortUniq))
         , ("flag sortUniq", MonoAlgo (runFreeze AF.sortUniq))
         ]

check_bounds = do
  qc (label "intro sortByBounds"     . prop_sortByBounds (INT.sortByBounds compare))
  qc (label "heap sortByBounds"      . prop_sortByBounds (H.sortByBounds compare))
  qc (label "insertion sortByBounds" . prop_sortByBounds (INS.sortByBounds compare))
  qc (label "intro selectByBounds"   . prop_selectByBounds (INT.selectByBounds compare))
  qc (label "heap selectByBounds"    . prop_selectByBounds (H.selectByBounds compare))
  qc (label "intro partialSortByBounds" . prop_partialSortByBounds (INT.partialSortByBounds compare))
  qc (label "heap partialSortByBounds"  . prop_partialSortByBounds (H.partialSortByBounds compare))
 where
 qc = quickCheckWith args { maxSuccess = 500 }

check_heap_ops = do
  qc (label "heapify"    . prop_heapify heapify)
  qc (label "pop"        . prop_pop heapify (H.pop compare))
  qc (label "popTo"      . prop_popTo heapify (H.popTo compare))
  qc (label "sortHeap"   . prop_sortHeap heapify (H.sortHeap compare))
  qc (label "heapInsert" . prop_heapInsert (H.heapInsert compare))
 where
 qc :: Testable prop => prop -> IO ()
 qc = quickCheckWith args { maxSuccess = 500 }
 heapify :: HeapifyAlgo
 heapify = H.heapify compare

check_killer = do
  qc "intro sort"         (\cmp a -> INT.sortBy cmp a) resultSorted
  qc "intro select"       (\cmp a -> INT.selectBy cmp a (half a)) (resultHalf False)
  qc "intro partial sort" (\cmp a -> INT.partialSortBy cmp a (half a)) (resultHalf True)
 where
 qc :: String -> CmpAlgo -> (Vector Int -> Vector Int -> Property) -> IO ()
 qc name algo correct = quickCheckWith args { maxSuccess = 50 } (label name (prop_killer algo correct))
 half a = MV.length a `div` 2

check_flag_compound = do
  -- Few distinct first components, so that large groups of elements are
  -- distinguished only by their second component.
  qc "flag (Word8, Word8)"   $ prop_large_sort_via (\x -> (w8 (x `mod` 3), w8 x)) AF.sort
  qc "flag (Word16, Word16)" $ prop_large_sort_via (\x -> (w16 (x `mod` 3), w16 x)) AF.sort
  qc "flag (Int, Int)"       $ prop_large_sort_via (\x -> (x `mod` 3, x)) AF.sort
  qc "flag (Word8, Int)"     $ prop_large_sort_via (\x -> (w8 (x `mod` 3), x)) AF.sort
  qc "flag (Int, Word8)"     $ prop_large_sort_via (\x -> (x `mod` 3, w8 x)) AF.sort
  qc "flag Either"           $ prop_large_sort_via
                                 (\x -> if even x then Left (w16 (x `div` 7)) else Right x) AF.sort
  qc "flag (ByteString, Int)" $ prop_large_sort_via
                                 (\x -> (B.pack [fromIntegral (x `mod` 3)], x)) AF.sort
  -- A custom radix and comparison that sort into descending order.
  qc "flag sortBy descending" $ prop_large_sort_via (Down . w32)
       (AF.sortBy compare (\(Down e) i -> AF.terminate e i) 256
                  (\i (Down e) -> 255 - AF.index i e))
 where
 qc :: Testable prop => String -> prop -> IO ()
 qc name = quickCheckWith args { maxSuccess = 300 } . label name
 w8 :: Int -> Word8
 w8 = fromIntegral
 w16 :: Int -> Word16
 w16 = fromIntegral
 w32 :: Int -> Word32
 w32 = fromIntegral

check_search = do
  qc "binarySearchPBounds"         $ prop_searchPBounds SR.binarySearchPBounds
  qc "gallopingSearchLeftPBounds"  $ prop_searchPBounds SR.gallopingSearchLeftPBounds
  qc "gallopingSearchRightPBounds" $ prop_searchPBounds SR.gallopingSearchRightPBounds
  qc "binarySearchP"               $ prop_searchP SR.binarySearchP
  qc "gallopingSearchLeftP"        $ prop_searchP SR.gallopingSearchLeftP
  qc "gallopingSearchRightP"       $ prop_searchP SR.gallopingSearchRightP
  qc "binarySearchLByBounds"       $ prop_searchLBounds (SR.binarySearchLByBounds compare)
  qc "binarySearchRByBounds"       $ prop_searchRBounds (SR.binarySearchRByBounds compare)
  qc "binarySearchByBounds"        $ prop_searchAnyBounds (SR.binarySearchByBounds compare)
 where
 qc :: Testable prop => String -> prop -> IO ()
 qc name = quickCheckWith args { maxSuccess = 1000 } . label name

check_vector_types = do
  forM_ sorts $ \(name, algo) -> do
    qc name (prop_vector_types (unWrapAlgo algo) resultSorted)
    qc (name ++ " large") (\(LargeVec _ v) -> prop_vector_types (unWrapAlgo algo) resultSorted v)
  -- Insertion sort is quadratic, so only test it on small vectors.
  qc "insertion sort" (prop_vector_types INS.sort resultSorted)
  forM_ uniqs $ \(name, algo) -> do
    qc name (prop_vector_types_uniq (unWrapUniq algo))
    qc (name ++ " large") (\(LargeVec _ v) -> prop_vector_types_uniq (unWrapUniq algo) v)
  qc "insertion sortUniq" (prop_vector_types_uniq INS.sortUniq)
  forM_ halves $ \(name, ordered, algo) ->
    qc (name ++ " large")
       (\(LargeVec _ v) -> prop_vector_types (\a -> unWrapSizeAlgo algo a (half a)) (resultHalf ordered) v)
  qc "merge sort stable unboxed" (prop_stable_unboxed M.sortBy)
  qc "timsort stable unboxed"    (prop_stable_unboxed T.sortBy)
  qc "nub"       prop_nub_vector_types
  qc "nub large" (\(LargeVec _ v) -> prop_nub_vector_types v)
 where
 qc :: Testable prop => String -> prop -> IO ()
 qc name = quickCheckWith args { maxSuccess = 200 } . label name
 half a = MV.length a `div` 2
 sorts :: [(String, WrappedAlgo Int ())]
 sorts = [ ("introsort", WrapAlgo INT.sort)
         , ("merge sort", WrapAlgo M.sort)
         , ("heapsort", WrapAlgo H.sort)
         , ("timsort", WrapAlgo T.sort)
         , ("radix sort", WrapAlgo R.sort)
         , ("american flag sort", WrapAlgo AF.sort)
         ]
 uniqs :: [(String, WrappedUniq)]
 uniqs = [ ("intro sortUniq", WrapUniq INT.sortUniq)
         , ("merge sortUniq", WrapUniq M.sortUniq)
         , ("heap sortUniq", WrapUniq H.sortUniq)
         , ("tim sortUniq", WrapUniq T.sortUniq)
         , ("flag sortUniq", WrapUniq AF.sortUniq)
         ]
 halves :: [(String, Bool, WrappedSizeAlgo Int ())]
 halves = [ ("intro select", False, WrapSizeAlgo INT.select)
          , ("heap select", False, WrapSizeAlgo H.select)
          , ("intro partialSort", True, WrapSizeAlgo INT.partialSort)
          , ("heap partialSort", True, WrapSizeAlgo H.partialSort)
          ]

newtype WrappedUniq = WrapUniq { unWrapUniq :: GenUniqAlgo }

check_nub = do
  quickCheckWith args (label "nub Int" . (prop_nub @Int))
  qc "nubBy"       prop_nubBy
  qc "nubBy large" (\(LargeVec _ v) -> prop_nubBy v)
  forM_ sortUniqs $ \(name, alg) -> do
    qc ("nubByMut " ++ name) (prop_nubByMut (unWrapSortUniqBy alg))
    qc ("nubByMut " ++ name ++ " large") (\(LargeVec _ v) -> prop_nubByMut (unWrapSortUniqBy alg) v)
  -- Insertion sort is quadratic, so only test it on small vectors.
  qc "nubByMut insertion" (prop_nubByMut INS.sortUniqBy)
 where
 qc :: Testable prop => String -> prop -> IO ()
 qc name = quickCheckWith args { maxSuccess = 300 } . label name
 sortUniqs :: [(String, WrappedSortUniqBy)]
 sortUniqs = [ ("intro", WrapSortUniqBy INT.sortUniqBy)
             , ("merge", WrapSortUniqBy M.sortUniqBy)
             , ("heap",  WrapSortUniqBy H.sortUniqBy)
             , ("tim",   WrapSortUniqBy T.sortUniqBy)
             ]

newtype WrappedSortUniqBy = WrapSortUniqBy { unWrapSortUniqBy :: SortUniqBy }


check_radix_sorts = do
  qc (label "radix Word8"       . prop_fullsort (R.sort :: Algo Word8  ()))
  qc (label "radix Word16"      . prop_fullsort (R.sort :: Algo Word16 ()))
  qc (label "radix Word32"      . prop_fullsort (R.sort :: Algo Word32 ()))
  qc (label "radix Word64"      . prop_fullsort (R.sort :: Algo Word64 ()))
  qc (label "radix Word"        . prop_fullsort (R.sort :: Algo Word   ()))
  qc (label "radix Int8"        . prop_fullsort (R.sort :: Algo Int8   ()))
  qc (label "radix Int16"       . prop_fullsort (R.sort :: Algo Int16  ()))
  qc (label "radix Int32"       . prop_fullsort (R.sort :: Algo Int32  ()))
  qc (label "radix Int64"       . prop_fullsort (R.sort :: Algo Int64  ()))
  qc (label "radix Int"         . prop_fullsort (R.sort :: Algo Int    ()))
  qc (label "radix (Int, Int)"  . prop_fullsort (R.sort :: Algo (Int, Int) ()))

  qc (label "flag Word8"       . prop_fullsort (AF.sort :: Algo Word8  ()))
  qc (label "flag Word16"      . prop_fullsort (AF.sort :: Algo Word16 ()))
  qc (label "flag Word32"      . prop_fullsort (AF.sort :: Algo Word32 ()))
  qc (label "flag Word64"      . prop_fullsort (AF.sort :: Algo Word64 ()))
  qc (label "flag Word"        . prop_fullsort (AF.sort :: Algo Word   ()))
  qc (label "flag Int8"        . prop_fullsort (AF.sort :: Algo Int8   ()))
  qc (label "flag Int16"       . prop_fullsort (AF.sort :: Algo Int16  ()))
  qc (label "flag Int32"       . prop_fullsort (AF.sort :: Algo Int32  ()))
  qc (label "flag Int64"       . prop_fullsort (AF.sort :: Algo Int64  ()))
  qc (label "flag Int"         . prop_fullsort (AF.sort :: Algo Int    ()))
  qc (label "flag ByteString"  . prop_fullsort (AF.sort :: Algo B.ByteString ()))
 where
 qc algo = quickCheckWith args algo

{-
check_schwartzian = do
  quickCheckWith args (prop_schwartzian i2w INS.sortBy)
 where
 i2w :: Int -> Word
 i2w = fromIntegral
-}

check_stable = do quickCheckWith args (label "insertion sort" . prop_stable INS.sortBy)
                  quickCheckWith args (label "merge sort" . prop_stable M.sortBy)
                  quickCheckWith args (label "radix sort" . prop_stable_radix R.sortBy)
                  quickCheckWith args (label "tim sort" . prop_stable T.sortBy)


check_optimal = do qc . label "size 2" $ prop_optimal 2 O.sort2ByOffset
                   qc . label "size 3" $ prop_optimal 3 O.sort3ByOffset
                   qc . label "size 4" $ prop_optimal 4 O.sort4ByOffset
                   qcIx . label "size 2 by index" $ prop_optimal_byIndex 2 sort2
                   qcIx . label "size 3 by index" $ prop_optimal_byIndex 3 sort3
                   qcIx . label "size 4 by index" $ prop_optimal_byIndex 4 sort4
 where
 qc = quickCheck
 qcIx :: Property -> IO ()
 qcIx = quickCheckWith args { maxSuccess = 500 }
 sort2, sort3, sort4 :: ByIndexAlgo
 sort2 ixs a = case ixs of [i, j]       -> O.sort2ByIndex (comparing fst) a i j
                           _            -> error "sort2: expected 2 indices"
 sort3 ixs a = case ixs of [i, j, k]    -> O.sort3ByIndex (comparing fst) a i j k
                           _            -> error "sort3: expected 3 indices"
 sort4 ixs a = case ixs of [i, j, k, l] -> O.sort4ByIndex (comparing fst) a i j k l
                           _            -> error "sort4: expected 4 indices"

check_permutation = do
  qc $ label "insertion"    . prop_permutation (INS.sort :: Algo Int ())
  qc $ label "introsort"    . prop_permutation (INT.sort :: Algo Int ())
  qc $ label "heapsort"     . prop_permutation (H.sort :: Algo Int ())

  qc $ label "mergesort"    . prop_permutation (M.sort :: Algo Int    ())
  qc $ label "timsort"      . prop_permutation (T.sort :: Algo Int    ())
  qc $ label "radix I8"     . prop_permutation (R.sort :: Algo Int8   ())
  qc $ label "radix I16"    . prop_permutation (R.sort :: Algo Int16  ())
  qc $ label "radix I32"    . prop_permutation (R.sort :: Algo Int32  ())
  qc $ label "radix I64"    . prop_permutation (R.sort :: Algo Int64  ())
  qc $ label "radix Int"    . prop_permutation (R.sort :: Algo Int    ())
  qc $ label "radix W8"     . prop_permutation (R.sort :: Algo Word8  ())
  qc $ label "radix W16"    . prop_permutation (R.sort :: Algo Word16 ())
  qc $ label "radix W32"    . prop_permutation (R.sort :: Algo Word32 ())
  qc $ label "radix W64"    . prop_permutation (R.sort :: Algo Word64 ())
  qc $ label "radix Word"   . prop_permutation (R.sort :: Algo Word   ())
  qc $ label "radix (Int, Int)" . prop_permutation (R.sort :: Algo (Int, Int) ())
  qc $ label "flag I8"      . prop_permutation (AF.sort :: Algo Int8   ())
  qc $ label "flag I16"     . prop_permutation (AF.sort :: Algo Int16  ())
  qc $ label "flag I32"     . prop_permutation (AF.sort :: Algo Int32  ())
  qc $ label "flag I64"     . prop_permutation (AF.sort :: Algo Int64  ())
  qc $ label "flag Int"     . prop_permutation (AF.sort :: Algo Int    ())
  qc $ label "flag W8"      . prop_permutation (AF.sort :: Algo Word8  ())
  qc $ label "flag W16"     . prop_permutation (AF.sort :: Algo Word16 ())
  qc $ label "flag W32"     . prop_permutation (AF.sort :: Algo Word32 ())
  qc $ label "flag W64"     . prop_permutation (AF.sort :: Algo Word64 ())
  qc $ label "flag Word"    . prop_permutation (AF.sort :: Algo Word   ())
  qc $ label "flag ByteString" . prop_permutation (AF.sort :: Algo B.ByteString ())
  qc $ label "intropartial" . prop_sized (\x -> const (prop_permutation x))
                                         (INT.partialSort :: SizeAlgo Int ())
  qc $ label "introselect"  . prop_sized (\x -> const (prop_permutation x))
                                         (INT.select :: SizeAlgo Int ())
  qc $ label "heappartial"  . prop_sized (\x -> const (prop_permutation x))
                                         (H.partialSort :: SizeAlgo Int ())
  qc $ label "heapselect"   . prop_sized (\x -> const (prop_permutation x))
                                         (H.select :: SizeAlgo Int ())

 where
 qc prop = quickCheckWith args prop

check_corners = do
  qc "insertion empty"    $ prop_empty       (INS.sort        :: Algo Int ())
  qc "introsort empty"    $ prop_empty       (INT.sort        :: Algo Int ())
  qc "intropartial empty" $ prop_sized_empty (INT.partialSort :: SizeAlgo Int ())
  qc "introselect empty"  $ prop_sized_empty (INT.select      :: SizeAlgo Int ())
  qc "heapsort empty"     $ prop_empty       (H.sort          :: Algo Int ())
  qc "heappartial empty"  $ prop_sized_empty (H.partialSort   :: SizeAlgo Int ())
  qc "heapselect empty"   $ prop_sized_empty (H.select        :: SizeAlgo Int ())
  qc "mergesort empty"    $ prop_empty       (M.sort          :: Algo Int ())
  qc "timsort empty"      $ prop_empty       (T.sort          :: Algo Int ())
  qc "radixsort empty"    $ prop_empty       (R.sort          :: Algo Int ())
  qc "flagsort empty"     $ prop_empty       (AF.sort         :: Algo Int ())
 where
 qc s prop = quickCheckWith (stdArgs { maxSuccess = 2 }) (label s prop)

type SAlgo e r = forall s mv. MVector mv e => mv s e -> e -> ST s r
type BoundSAlgo e r = forall s mv. MVector mv e => mv s e -> e -> Int -> Int -> ST s r

check_search_range = do
  qc $ (label "binarySearchL" .)
         . prop_search_inrange (SR.binarySearchLByBounds compare :: BoundSAlgo Int Int)
  qc $ (label "binarySearchL lo-bound" .)
         . prop_search_lowbound (SR.binarySearchL :: SAlgo Int Int)
  qc $ (label "binarySearch" .)
         . prop_search_inrange (SR.binarySearchByBounds compare :: BoundSAlgo Int Int)
  qc $ (label "binarySearchR" .)
         . prop_search_inrange (SR.binarySearchRByBounds compare :: BoundSAlgo Int Int)
  qc $ (label "binarySearchR hi-bound" .)
         . prop_search_upbound (SR.binarySearchR :: SAlgo Int Int)
 where
 qc prop = quickCheckWith args prop

-- | The number of properties that have failed so far.
failures :: IORef Int
failures = unsafePerformIO (newIORef 0)
{-# NOINLINE failures #-}

-- | Like 'QC.quickCheckWith', but records failures so that the test suite
-- can exit with a failure status.
quickCheckWith :: Testable prop => Args -> prop -> IO ()
quickCheckWith a p = do
  r <- QC.quickCheckWithResult a p
  unless (isSuccess r) $ modifyIORef' failures (+ 1)

quickCheck :: Testable prop => prop -> IO ()
quickCheck = quickCheckWith stdArgs

main :: IO ()
main = do
  runTests
  n <- readIORef failures
  when (n > 0) $ do
    putStrLn (show n ++ " properties failed")
    exitFailure

runTests :: IO ()
runTests = do
          putStrLn "Int tests:"
          check_Int_sort
          check_Int_sortUniq
          check_Int_partialsort
          check_Int_select
          putStrLn "Radix sort tests:"
          check_radix_sorts
--          putStrLn "Schwartzian transform (Int -> Word):"
--          check_schwartzian
          putStrLn "Stability:"
          check_stable
          putStrLn "Optimals:"
          check_optimal
          putStrLn "Permutation:"
          check_permutation
          putStrLn "Search in range:"
          check_search_range
          putStrLn "Corner cases:"
          check_corners
          putStrLn "Algorithms:"
          check_nub
          putStrLn "Large structured inputs:"
          check_large
          putStrLn "Sub-range (ByBounds) operations:"
          check_bounds
          putStrLn "Heap operations:"
          check_heap_ops
          putStrLn "Introsort worst-case inputs:"
          check_killer
          putStrLn "American flag sort on compound types:"
          check_flag_compound
          putStrLn "Searching:"
          check_search
          putStrLn "Unboxed, storable and primitive vectors:"
          check_vector_types
