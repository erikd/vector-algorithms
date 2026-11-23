{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Properties where

import Prelude

import Optimal

import Adversary

import Control.Monad
import Control.Monad.ST

import Data.List
import Data.Ord

import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MV

import Data.Vector.Generic (modify)

import qualified Data.Vector.Generic.Mutable as G
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Primitive as PV
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as UV

import Data.Vector.Algorithms.Optimal (Comparison)
import Data.Vector.Algorithms.Radix (radix, passes, size)
import qualified Data.Vector.Algorithms as Alg

import qualified Data.Map as M

import Test.QuickCheck hiding (Sorted)

import Util

prop_sorted :: (Ord e) => Vector e -> Property
prop_sorted arr | V.length arr < 2 = property True
                | otherwise        = check (V.head arr) (V.tail arr)
 where
 check e arr | V.null arr = property True
             | otherwise  = e <= V.head arr .&. check (V.head arr) (V.tail arr)

prop_sorted_uniq :: (Ord e) => Vector e -> Property
prop_sorted_uniq arr | V.length arr < 2 = property True
                     | otherwise        = check (V.head arr) (V.tail arr)
 where
 check e arr | V.null arr = property True
             | otherwise  = e < V.head arr .&. check (V.head arr) (V.tail arr)

prop_empty :: (Ord e) => (forall s. MV.MVector s e -> ST s ()) -> Property
prop_empty algo = prop_sorted (modify algo $ V.fromList [])

prop_fullsort :: (Ord e)
              => (forall s mv. G.MVector mv e => mv s e -> ST s ()) -> Vector e -> Property
prop_fullsort algo arr = prop_sorted $ modify algo arr

runFreeze
  :: forall e . (Ord e)
  => (forall s mv . G.MVector mv e => mv s e -> ST s (mv s e))
  -> (forall s v mv. (GV.Vector v e, mv ~ GV.Mutable v) => mv s e -> ST s (v e))
runFreeze alg mv = do
  mv <- alg mv
  GV.unsafeFreeze mv

prop_full_sortUniq
  :: (Ord e, Show e)
  => (forall s . MV.MVector s e -> ST s (Vector e))
  -> Vector e -> Property
prop_full_sortUniq algo arr = runST $ do
  mv <- V.unsafeThaw arr
  arr' <- algo mv
  pure (prop_sorted_uniq arr')

{-
prop_schwartzian :: (UA e, UA k, Ord k)
                 => (e -> k)
                 -> (forall e s. (UA e) => (e -> e -> Ordering) -> MUArr e s -> ST s ())
                 -> UArr e -> Property
prop_schwartzian f algo arr
  | lengthU arr < 2 = property True
  | otherwise       = let srt = modify (algo `usingKeys` f) arr
                      in check (headU srt) (tailU srt)
 where
 check e arr | nullU arr = property True
             | otherwise = f e <= f (headU arr) .&. check (headU arr) (tailU arr)
-}

longGen :: (Arbitrary e) => Int -> Gen (Vector e)
longGen k = liftM2 (\l r -> V.fromList (l ++ r)) (vectorOf k arbitrary) arbitrary

sanity :: Int
sanity = 100

prop_partialsort :: (Ord e, Arbitrary e, Show e)
                 => (forall s mv. G.MVector mv e => mv s e -> Int -> ST s ())
                 -> Positive Int -> Property
prop_partialsort = prop_sized $ \algo k v -> do
  let newVec = modify algo v
      vhead = V.take k newVec
      vtail = V.drop k newVec
  prop_sorted vhead
    .&&.
      -- Every element in the head should be <= every element in the tail.
      (V.null vhead || V.null vtail || V.maximum vhead <= V.minimum vtail)

prop_sized_empty :: (Ord e) => (forall s. MV.MVector s e -> Int -> ST s ()) -> Property
prop_sized_empty algo = prop_empty (flip algo 0) .&&. prop_empty (flip algo 10)

prop_select :: (Ord e, Arbitrary e, Show e)
            => (forall s mv. G.MVector mv e => mv s e -> Int -> ST s ())
            -> Positive Int -> Property
prop_select = prop_sized $ \algo k arr ->
  let vec' = modify algo arr
      l    = V.slice 0 k vec'
      r    = V.slice k (V.length vec' - k) vec'
  in V.all (\e -> V.all (e <=) r) l

prop_sized :: (Arbitrary e, Show e, Testable prop)
           => ((forall s mv. G.MVector mv e => mv s e -> ST s ())
                 -> Int -> Vector e -> prop)
           -> (forall s mv. G.MVector mv e => mv s e -> Int -> ST s ())
           -> Positive Int -> Property
prop_sized prop algo (Positive k) =
  let k' = k `mod` sanity
  in forAll (longGen k') $ prop (\marr -> algo marr k') k'

prop_stable :: (forall e s mv. G.MVector mv e => Comparison e -> mv s e -> ST s ())
            -> Vector Int -> Property
-- prop_stable algo arr = property $ modify algo arr == arr
prop_stable algo arr = stable $ modify (algo (comparing fst)) $ V.zip arr ix
 where
 ix = V.fromList [1 .. V.length arr]

stable arr | V.null arr = property True
           | otherwise  = let (e, i) = V.head arr
                          in V.all (\(e', i') -> e < e' || i < i') (V.tail arr)
                            .&. stable (V.tail arr)

prop_stable_radix :: (forall e s mv. G.MVector mv e => Int -> Int -> (Int -> e -> Int)
                        -> mv s e -> ST s ())
                  -> Vector Int -> Property
prop_stable_radix algo arr =
  stable . modify (algo (passes e) (size e) (\k (e, _) -> radix k e))
         $ V.zip arr ix
 where
 ix = V.fromList [1 .. V.length arr]
 e = V.head arr

prop_optimal :: Int
             -> (forall e s mv. G.MVector mv e => Comparison e -> mv s e -> Int -> ST s ())
             -> Property
prop_optimal n algo = label "sorting" sortn .&. label "stability" stabn
 where
 arrn  = V.fromList [0..n-1]
 sortn = all ( (== arrn)
             . modify (\a -> algo compare a 0)
             . V.fromList)
         $ permutations [0..n-1]
 stabn = all ( (== arrn)
             . snd
             . V.unzip
             . modify (\a -> algo (comparing fst) a 0))
         $ stability n

type Bag e = M.Map e Int

toBag :: (Ord e) => Vector e -> Bag e
toBag = M.fromListWith (+) . flip zip (repeat 1) . V.toList

prop_permutation :: (Ord e) => (forall s mv. G.MVector mv e => mv s e -> ST s ())
                 -> Vector e -> Property
prop_permutation algo arr = property $
                            toBag arr == toBag (modify algo arr)

newtype SortedVec e = Sorted (Vector e)

instance (Show e) => Show (SortedVec e) where
  show (Sorted a) = show a

instance (Arbitrary e, Ord e) => Arbitrary (SortedVec e) where
  arbitrary = fmap (Sorted . V.fromList . sort)
                $ liftM2 (++) (vectorOf 20 arbitrary) arbitrary

ixRanges :: Vector e -> Gen (Int, Int)
ixRanges vec = do i <- fmap (`mod` len) arbitrary
                  j <- fmap (`mod` len) arbitrary
                  return $ if i < j then (i, j) else (j, i)
 where len = V.length vec

prop_search_inrange :: (Ord e)
                    => (forall s. MVector s e -> e -> Int -> Int -> ST s Int)
                    -> SortedVec e -> e -> Property
prop_search_inrange algo (Sorted arr) e = forAll (ixRanges arr) $ \(i, j) ->
  let k = runST (mfromList (V.toList arr) >>= \marr -> algo marr e i j)
  in property $ i <= k && k <= j
 where
 len = V.length arr

prop_search_insert :: (e -> e -> Bool) -> (e -> e -> Bool)
                   -> (forall s. MVector s e -> e -> ST s Int)
                   -> SortedVec e -> e -> Property
prop_search_insert lo hi algo (Sorted arr) e =
  property $ (k == 0   || (arr V.! (k-1)) `lo` e)
          && (k == len || (arr V.! k) `hi` e)
 where
 len = V.length arr
 k = runST (mfromList (V.toList arr) >>= \marr -> algo marr e)

prop_search_lowbound :: (Ord e)
                     => (forall s. MVector s e -> e -> ST s Int)
                     -> SortedVec e -> e -> Property
prop_search_lowbound = prop_search_insert (<) (>=)

prop_search_upbound :: (Ord e)
                    => (forall s. MVector s e -> e -> ST s Int)
                    -> SortedVec e -> e -> Property
prop_search_upbound = prop_search_insert (<=) (>)

prop_nub :: (Ord e, Show e) => Vector e -> Property
prop_nub v =
  V.fromList (nub (V.toList v)) === Alg.nub v

-- | Sorting a large structured vector gives the same result as
-- 'Data.List.sort'. This checks both sortedness and that the output is a
-- permutation of the input, in time linear in the vector length.
prop_large_sort :: (forall s mv. G.MVector mv Int => mv s Int -> ST s ())
                -> LargeVec -> Property
prop_large_sort algo (LargeVec _ v) =
  modify algo v === V.fromList (sort (V.toList v))

-- | Sorting a large structured vector of (key, original index) pairs by key
-- gives the same result as the stable 'Data.List.sortBy'. This also checks
-- stability.
prop_large_stable :: (forall e s mv. G.MVector mv e => Comparison e -> mv s e -> ST s ())
                  -> LargeVec -> Property
prop_large_stable algo (LargeVec _ v) =
  modify (algo (comparing fst)) kv === V.fromList (sortBy (comparing fst) (V.toList kv))
 where
 kv = V.imap (\i k -> (k, i)) v

-- | 'sortUniq' on a large structured vector gives the sorted distinct
-- elements.
prop_large_sortUniq :: (forall s . MV.MVector s Int -> ST s (Vector Int))
                    -> LargeVec -> Property
prop_large_sortUniq algo (LargeVec _ v) = runST $ do
  mv <- V.thaw v
  v' <- algo mv
  pure (v' === V.fromList (M.keys (toBag v)))

-- ---------------------------------------------------------------------------
-- Properties of the @...ByBounds@ functions

-- | The elements of the result outside [l, u) are unchanged from the input.
outsideUnchanged :: Vector Int -> Int -> Int -> Vector Int -> Property
outsideUnchanged v l u r =
  counterexample "elements outside [l, u) changed" $
    V.take l r == V.take l v && V.drop u r == V.drop u v

sliceList :: Int -> Int -> Vector e -> [e]
sliceList l u = V.toList . V.slice l (u - l)

prop_sortByBounds :: (forall s. MV.MVector s Int -> Int -> Int -> ST s ())
                  -> BoundsCase -> Property
prop_sortByBounds algo (BoundsCase v l u _) =
  outsideUnchanged v l u r .&&. sliceList l u r === sort (sliceList l u v)
 where
 r = modify (\a -> algo a l u) v

-- | [l, l+k) holds the k smallest elements of the input's [l, u), and
-- [l+k, u) holds the rest.
prop_selectByBounds :: (forall s. MV.MVector s Int -> Int -> Int -> Int -> ST s ())
                    -> BoundsCase -> Property
prop_selectByBounds algo (BoundsCase v l u k) =
  outsideUnchanged v l u r
    .&&. sort (sliceList l (l + k) r) === lo
    .&&. sort (sliceList (l + k) u r) === hi
 where
 r = modify (\a -> algo a k l u) v
 (lo, hi) = splitAt k (sort (sliceList l u v))

-- | [l, l+k) holds the k smallest elements of the input's [l, u) in order,
-- and [l+k, u) holds the rest.
prop_partialSortByBounds :: (forall s. MV.MVector s Int -> Int -> Int -> Int -> ST s ())
                         -> BoundsCase -> Property
prop_partialSortByBounds algo (BoundsCase v l u k) =
  outsideUnchanged v l u r
    .&&. sliceList l (l + k) r === lo
    .&&. sort (sliceList (l + k) u r) === hi
 where
 r = modify (\a -> algo a k l u) v
 (lo, hi) = splitAt k (sort (sliceList l u v))

-- ---------------------------------------------------------------------------
-- Properties of the heap operations exported by Data.Vector.Algorithms.Heap.
-- The heap is a 4-ary max-heap: the children of index i are 4i+1 .. 4i+4.

isHeap :: [Int] -> Bool
isHeap xs = and [ h V.! ((i - 1) `div` 4) >= h V.! i | i <- [1 .. V.length h - 1] ]
 where h = V.fromList xs

heapProp :: [Int] -> Property
heapProp xs = counterexample ("not a heap: " ++ show xs) (isHeap xs)

type HeapifyAlgo = forall s. MV.MVector s Int -> Int -> Int -> ST s ()

-- | 'heapify' makes [l, u) a heap, without changing its elements or anything
-- outside it.
prop_heapify :: HeapifyAlgo -> BoundsCase -> Property
prop_heapify heapify (BoundsCase v l u _) =
  outsideUnchanged v l u r
    .&&. heapProp (sliceList l u r)
    .&&. sort (sliceList l u r) === sort (sliceList l u v)
 where
 r = modify (\a -> heapify a l u) v

-- | Given a heap in [l, u), 'popTo' to an index t outside [l, u) moves the
-- maximum to t and leaves a heap of the remaining elements plus the old
-- value at t in [l, u).
prop_popTo :: HeapifyAlgo
           -> (forall s. MV.MVector s Int -> Int -> Int -> Int -> ST s ())
           -> BoundsCase -> Property
prop_popTo heapify popTo (BoundsCase v l u _) =
  u > l && u < V.length v ==>
    forAll (choose (u, V.length v - 1)) $ \t ->
      let h = modify (\a -> heapify a l u) v
          r = modify (\a -> popTo a l u t) h
          old = sliceList l u h
      in r V.! t === maximum old
           .&&. heapProp (sliceList l u r)
           .&&. sort (sliceList l u r) === sort (delete (maximum old) old ++ [h V.! t])
           .&&. counterexample "elements outside [l, u) and t changed"
                  (V.toList (V.take l r) == V.toList (V.take l h)
                   && and [ r V.! i == h V.! i | i <- [u .. V.length v - 1], i /= t ])

-- | Given a heap in [l, u), 'pop' with upper bound u-1 moves the maximum to
-- u-1 and leaves a heap in [l, u-1).
prop_pop :: HeapifyAlgo
         -> (forall s. MV.MVector s Int -> Int -> Int -> ST s ())
         -> BoundsCase -> Property
prop_pop heapify pop (BoundsCase v l u _) =
  u - l >= 1 ==>
    outsideUnchanged v l u r
      .&&. r V.! (u - 1) === maximum (sliceList l u v)
      .&&. heapProp (sliceList l (u - 1) r)
      .&&. sort (sliceList l u r) === sort (sliceList l u v)
 where
 r = modify (\a -> heapify a l u >> pop a l (u - 1)) v

-- | Given a heap in [l, u), 'sortHeap' with l <= m < u puts the largest
-- u-m elements, sorted, into [m, u).
prop_sortHeap :: HeapifyAlgo
              -> (forall s. MV.MVector s Int -> Int -> Int -> Int -> ST s ())
              -> BoundsCase -> Property
prop_sortHeap heapify sortHeap (BoundsCase v l u k) =
  u > l ==>
    let m = min (l + k) (u - 1)
        r = modify (\a -> heapify a l u >> sortHeap a l m u) v
        sorted = sort (sliceList l u v)
    in outsideUnchanged v l u r
         .&&. sliceList m u r === drop (m - l) sorted
         .&&. sort (sliceList l u r) === sorted

-- | Building a heap by repeated 'heapInsert' gives a heap of the inserted
-- elements.
prop_heapInsert :: (forall s. MV.MVector s Int -> Int -> Int -> Int -> ST s ())
                -> LargeVec -> Property
prop_heapInsert heapInsert (LargeVec _ v) =
  heapProp (V.toList r) .&&. sort (V.toList r) === sort (V.toList v)
 where
 r = runST $ do
   a <- MV.new (V.length v)
   V.imapM_ (\i e -> heapInsert a 0 i e) v
   V.freeze a

-- ---------------------------------------------------------------------------
-- Worst-case inputs for introsort

-- | On an input built by McIlroy's adversary for this algorithm, the result
-- is correct and the number of comparisons stays within 6 n lg n.
--
-- Without introsort's heap sort fallback these inputs need about n^2/4
-- comparisons, which exceeds the bound for every n >= 500 tested here, so
-- this checks that the fallback works. With it, the observed count is at
-- most about 3.9 n lg n.
prop_killer :: CmpAlgo -> (Vector Int -> Vector Int -> Property) -> Property
prop_killer algo correct = forAll (choose (500, 5000)) $ \n ->
  let input  = killerInput algo n
      (c, r) = countComparisons algo input
      bound  = 6 * fromIntegral n * logBase 2 (fromIntegral n) :: Double
  in correct input r
       .&&. counterexample ("n = " ++ show n ++ ": " ++ show c
                            ++ " comparisons exceeds 6 n lg n = " ++ show (round bound :: Int))
              (fromIntegral c <= bound)

-- | The result is the sorted input.
resultSorted :: Vector Int -> Vector Int -> Property
resultSorted input r = r === V.fromList (sort (V.toList input))

-- | The first half of the result holds the smallest half of the input, in
-- order if @ordered@ is set.
resultHalf :: Bool -> Vector Int -> Vector Int -> Property
resultHalf ordered input r =
  (if ordered then front else sort front) === lo .&&. sort back === hi
 where
 k = V.length input `div` 2
 (front, back) = splitAt k (V.toList r)
 (lo, hi) = splitAt k (sort (V.toList input))

-- ---------------------------------------------------------------------------
-- Sorting large vectors of other element types

-- | Like 'prop_large_sort', but first maps the 'Int' keys to another element
-- type. Mapping with a function that has few distinct values in its first
-- component gives large groups of elements that only differ later on.
prop_large_sort_via :: (Ord e, Show e)
                    => (Int -> e)
                    -> (forall s mv. G.MVector mv e => mv s e -> ST s ())
                    -> LargeVec -> Property
prop_large_sort_via f algo (LargeVec _ v) =
  modify algo w === V.fromList (sort (V.toList w))
 where
 w = V.map f v

-- ---------------------------------------------------------------------------
-- Searching

type PSearchBounds = forall s. (Int -> Bool) -> MV.MVector s Int -> Int -> Int -> ST s Int
type CmpSearchBounds = forall s. MV.MVector s Int -> Int -> Int -> Int -> ST s Int

runSearch :: Vector Int -> (forall s. MV.MVector s Int -> ST s Int) -> Int
runSearch v search = runST (V.thaw v >>= search)

-- | The index in [l, u] at which a monotone predicate turns from False to
-- True on the sorted range [l, u), or u if it is False throughout.
modelP :: (Int -> Bool) -> Vector Int -> Int -> Int -> Int
modelP p v l u = l + length (takeWhile (not . p) (sliceList l u v))

-- | A predicate search over [l, u) agrees with the model, for the monotone
-- predicates (>= e) and (> e).
prop_searchPBounds :: PSearchBounds -> SearchCase -> Property
prop_searchPBounds search (SearchCase v l u e) = conjoin
  [ counterexample name $ runSearch v (\a -> search p a l u) === modelP p v l u
  | (name, p) <- [(">= e", (>= e)), ("> e", (> e))] ]

-- | A predicate search over the whole vector agrees with the model.
prop_searchP :: (forall s. (Int -> Bool) -> MV.MVector s Int -> ST s Int)
             -> SearchCase -> Property
prop_searchP search (SearchCase v _ _ e) = conjoin
  [ counterexample name $ runSearch v (search p) === modelP p v 0 (V.length v)
  | (name, p) <- [(">= e", (>= e)), ("> e", (> e))] ]

-- | 'binarySearchLByBounds' finds the first index in [l, u] at which e can be
-- inserted, and 'binarySearchRByBounds' the last.
prop_searchLBounds, prop_searchRBounds :: CmpSearchBounds -> SearchCase -> Property
prop_searchLBounds search (SearchCase v l u e) =
  runSearch v (\a -> search a e l u) === modelP (>= e) v l u
prop_searchRBounds search (SearchCase v l u e) =
  runSearch v (\a -> search a e l u) === modelP (> e) v l u

-- | 'binarySearchByBounds' finds some index in [l, u] at which e can be
-- inserted while keeping [l, u) sorted.
prop_searchAnyBounds :: CmpSearchBounds -> SearchCase -> Property
prop_searchAnyBounds search (SearchCase v l u e) =
  counterexample ("result " ++ show i) $
    modelP (>= e) v l u <= i && i <= modelP (> e) v l u
 where
 i = runSearch v (\a -> search a e l u)

-- ---------------------------------------------------------------------------
-- Optimal sorts at arbitrary indices

-- | A @sortNByIndex@ function from "Data.Vector.Algorithms.Optimal", taking
-- its n indices as a list.
type ByIndexAlgo = forall s. [Int] -> MV.MVector s (Int, Int) -> ST s ()

-- | Placing n elements at distinct positions of a larger vector, given in
-- any order, and sorting them by index: read back in the order of the
-- indices, the elements are stably sorted (by their first component), and
-- every other position is unchanged. This is checked for every permutation
-- of n distinct keys and every input in the exhaustive stability set.
prop_optimal_byIndex :: Int -> ByIndexAlgo -> Property
prop_optimal_byIndex n algo =
  forAll (choose (n, 20)) $ \len ->
  forAll (take n <$> shuffle [0 .. len - 1]) $ \ixs ->
    conjoin [ check len ixs (V.toList xs) | xs <- inputs ]
 where
 inputs :: [Vector (Int, Int)]
 inputs = map (\p -> V.fromList (zip p [0 ..])) (permutations [0 .. n - 1])
            ++ stability n
 check len ixs xs =
   let filler = V.generate len (\i -> (-1, -1 - i))
       before = filler V.// zip ixs xs
       after  = modify (algo ixs) before
       others = [ i | i <- [0 .. len - 1], i `notElem` ixs ]
   in counterexample ("indices " ++ show ixs ++ ", input " ++ show xs) $
        map (after V.!) ixs === sortBy (comparing fst) xs
          .&&. counterexample "positions outside the indices changed"
                 (map (after V.!) others == map (before V.!) others)

-- ---------------------------------------------------------------------------
-- nub

type SortUniqBy = forall s. Comparison Int -> MV.MVector s Int -> ST s (MV.MVector s Int)

-- | The first element of each equivalence class, in the order of the input,
-- for a comparison that is 'compare' on the given key.
firstOccurrencesOn :: (Int -> Int) -> [Int] -> [Int]
firstOccurrencesOn key = go M.empty
 where
 go _ [] = []
 go seen (x:xs)
   | M.member (key x) seen = go seen xs
   | otherwise             = x : go (M.insert (key x) () seen) xs

-- | The comparisons nub is tested with, as a name, the comparison, and the
-- key it compares on (the descending comparison has the same classes as
-- 'compare').
nubComparisons :: [(String, Comparison Int, Int -> Int)]
nubComparisons =
  [ ("compare",       compare,                id)
  , ("descending",    flip compare,           id)
  , ("modulo 7",      comparing (`mod` 7),    (`mod` 7))
  , ("absolute",      comparing abs,          abs)
  ]

-- | 'Alg.nubByMut' with the given sortUniqBy keeps the first element of each
-- equivalence class in input order, and leaves its input unchanged.
prop_nubByMut :: SortUniqBy -> Vector Int -> Property
prop_nubByMut alg v = conjoin
  [ counterexample name $
      let (r, v') = runST $ do
            mv <- V.thaw v
            d  <- Alg.nubByMut alg cmp mv
            (,) <$> V.freeze d <*> V.freeze mv
      in V.toList r === firstOccurrencesOn key (V.toList v)
           .&&. counterexample "input changed" (v' == v)
  | (name, cmp, key) <- nubComparisons ]

-- | 'Alg.nubBy' keeps the first element of each equivalence class in input
-- order.
prop_nubBy :: Vector Int -> Property
prop_nubBy v = conjoin
  [ counterexample name $
      V.toList (Alg.nubBy cmp v) === firstOccurrencesOn key (V.toList v)
  | (name, cmp, key) <- nubComparisons ]

-- ---------------------------------------------------------------------------
-- Other vector types
--
-- The algorithms are INLINE and get specialised to each vector type, so
-- also run them on unboxed, storable and primitive vectors, not just the
-- boxed vectors used everywhere else.

type GenAlgo = forall s mv. G.MVector mv Int => mv s Int -> ST s ()
type GenUniqAlgo = forall s mv. G.MVector mv Int => mv s Int -> ST s (mv s Int)

-- | Run an in-place algorithm on the input converted to each of the other
-- vector types, and check each result with the given property.
prop_vector_types :: GenAlgo -> (Vector Int -> Vector Int -> Property)
                  -> Vector Int -> Property
prop_vector_types algo correct v = conjoin
  [ counterexample "unboxed"   $ correct v (via (undefined :: UV.Vector Int))
  , counterexample "storable"  $ correct v (via (undefined :: SV.Vector Int))
  , counterexample "primitive" $ correct v (via (undefined :: PV.Vector Int))
  ]
 where
 via :: GV.Vector w Int => w Int -> Vector Int
 via proxy = GV.convert (GV.modify algo (GV.convert v `asTypeOf` proxy))

-- | 'prop_vector_types' for algorithms returning a vector of unique
-- elements: the result is the sorted distinct elements of the input.
prop_vector_types_uniq :: GenUniqAlgo -> Vector Int -> Property
prop_vector_types_uniq algo v = conjoin
  [ counterexample "unboxed"   $ via (undefined :: UV.Vector Int) === expected
  , counterexample "storable"  $ via (undefined :: SV.Vector Int) === expected
  , counterexample "primitive" $ via (undefined :: PV.Vector Int) === expected
  ]
 where
 expected = M.keys (toBag v)
 via :: GV.Vector w Int => w Int -> [Int]
 via proxy = runST $ do
   mv <- GV.thaw (GV.convert v `asTypeOf` proxy)
   r  <- algo mv
   GV.toList . (`asTypeOf` proxy) <$> GV.freeze r

-- | A stable sort is stable on unboxed vectors of (key, original index)
-- pairs.
prop_stable_unboxed :: (forall s mv. G.MVector mv (Int, Int)
                          => Comparison (Int, Int) -> mv s (Int, Int) -> ST s ())
                    -> LargeVec -> Property
prop_stable_unboxed algo (LargeVec _ v) =
  UV.toList (GV.modify (algo (comparing fst)) kv)
    === sortBy (comparing fst) (UV.toList kv)
 where
 kv = UV.imap (\i k -> (k, i)) (UV.convert v)

-- | 'Alg.nub' on each of the other vector types keeps the first occurrence
-- of each element, in input order.
prop_nub_vector_types :: Vector Int -> Property
prop_nub_vector_types v = conjoin
  [ counterexample "unboxed"   $ UV.toList (Alg.nub (UV.convert v)) === expected
  , counterexample "storable"  $ SV.toList (Alg.nub (SV.convert v)) === expected
  , counterexample "primitive" $ PV.toList (Alg.nub (PV.convert v)) === expected
  ]
 where
 expected = firstOccurrencesOn id (V.toList v)
