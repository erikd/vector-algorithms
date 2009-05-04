
module Tests(main) where

import Properties

import Util

import Test.QuickCheck

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

prop_Int_sort = mapM_ (quickCheckWith args) props
 where
 props :: [UArr Int -> Property]
 props = [ label "intro"     . prop_fullsort INT.sort
         , label "insertion" . prop_fullsort INS.sort
         , label "merge"     . prop_fullsort M.sort
         , label "radix"     . prop_fullsort R.sort
         , label "tri-heap"  . prop_fullsort TH.sort
         ]


main = prop_Int_sort
