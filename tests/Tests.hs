{-# LANGUAGE ImpredicativeTypes, Rank2Types, TypeOperators #-}

module Main (main) where

import Properties

import Util

import Test.QuickCheck

import Control.Monad
import Control.Monad.ST

import Data.Int
import Data.Word

import Data.Array.Vector

import Data.Array.Vector.Algorithms.Insertion as INS
import Data.Array.Vector.Algorithms.Intro     as INT
import Data.Array.Vector.Algorithms.Merge     as M
import Data.Array.Vector.Algorithms.Radix     as R
import Data.Array.Vector.Algorithms.TriHeap   as TH

args = stdArgs
       { maxSuccess = 300
       , maxDiscard = 200
       }

check_Int_sort = forM_ algos $ \(name,algo) ->
  quickCheckWith args (label name . prop_fullsort algo)
 where
 algos :: [(String, forall s. MUArr Int s -> ST s ())]
 algos = [ ("introsort", INT.sort)
         , ("insertion sort", INS.sort)
         , ("merge sort", M.sort)
         , ("tri-heapsort", TH.sort)
         ]

check_Int_partialsort = forM_ algos $ \(name,algo) ->
  quickCheckWith args (label name . prop_partialsort algo)
 where
 algos :: [(String, forall s. MUArr Int s -> Int -> ST s ())]
 algos = [ ("intro-partialsort", INT.partialSort)
         , ("tri-heap partialsort", TH.partialSort)
         ]

check_Int_select = forM_ algos $ \(name,algo) ->
  quickCheckWith args (label name . prop_select algo)
 where
 algos :: [(String, forall s. MUArr Int s -> Int -> ST s ())]
 algos = [ ("intro-select", INT.select)
         , ("tri-heap select", TH.select)
         ]

check_radix_sorts = do
  qc (label "Word8"  . prop_fullsort (R.sort :: MUArr Word8 s  -> ST s ()))
  qc (label "Word16" . prop_fullsort (R.sort :: MUArr Word16 s -> ST s ()))
  qc (label "Word32" . prop_fullsort (R.sort :: MUArr Word32 s -> ST s ()))
  qc (label "Word64" . prop_fullsort (R.sort :: MUArr Word64 s -> ST s ()))
  qc (label "Word"   . prop_fullsort (R.sort :: MUArr Word s   -> ST s ()))
  qc (label "Int8"   . prop_fullsort (R.sort :: MUArr Int8 s   -> ST s ()))
  qc (label "Int16"  . prop_fullsort (R.sort :: MUArr Int16 s  -> ST s ()))
  qc (label "Int32"  . prop_fullsort (R.sort :: MUArr Int32 s  -> ST s ()))
  qc (label "Int64"  . prop_fullsort (R.sort :: MUArr Int64 s  -> ST s ()))
  qc (label "Int"    . prop_fullsort (R.sort :: MUArr Int s    -> ST s ()))
  qc (label "Int :*: Int"
        . prop_fullsort (R.sort :: MUArr (Int :*: Int) s -> ST s ()))
 where
 qc algo = quickCheckWith args algo

main = do putStrLn "Int tests:"
          check_Int_sort
          check_Int_partialsort
          check_Int_select
          putStrLn "Radix sort tests:"
          check_radix_sorts

