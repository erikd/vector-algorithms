{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Control.Monad
import Control.Monad.ST

import Data.Char
import Data.Array.Vector

import qualified Data.Array.Vector.Algorithms.Insertion as INS
import qualified Data.Array.Vector.Algorithms.Intro     as INT
import qualified Data.Array.Vector.Algorithms.TriHeap   as TH
import qualified Data.Array.Vector.Algorithms.Merge     as M
import qualified Data.Array.Vector.Algorithms.Radix     as R

import System.Environment
import System.Console.GetOpt
import System.Random.Mersenne

import Blocks

-- Does nothing. For testing the speed/heap allocation of the building blocks.
noalgo :: (UA e) => MUArr e s -> ST s ()
noalgo _ = return ()

-- Allocates a temporary buffer, like mergesort for similar purposes as noalgo.
alloc :: (UA e) => MUArr e s -> ST s ()
alloc arr | len <= 4  = arr `seq` return ()
          | otherwise = (newMU (len `div` 2) :: ST s (MUArr Int s)) >> return ()
 where len = lengthMU arr

displayTime :: String -> Integer -> IO ()
displayTime s elapsed = putStrLn $
    s ++ " : " ++ show (fromIntegral elapsed / 1e12) ++ " seconds"

run :: String -> IO Integer -> IO ()
run s t = t >>= displayTime s

sortSuite :: String -> MTGen -> Int -> (forall s. MUArr Int s -> ST s ()) -> IO ()
sortSuite str g n sort = do
  putStrLn $ "Testing: " ++ str
  run "Random            " $ speedTest n (rand g >=> modulo n) sort
  run "Sorted            " $ speedTest n ascend sort
  run "Reverse-sorted    " $ speedTest n (descend n) sort
  run "Random duplicates " $ speedTest n (rand g >=> modulo 1000) sort
  let m = 4 * (n `div` 4)
  run "Median killer     " $ speedTest m (medianKiller m) sort

partialSortSuite :: String -> MTGen -> Int -> Int
                 -> (forall s. MUArr Int s -> Int -> ST s ()) -> IO ()
partialSortSuite str g n k sort = sortSuite str g n (\a -> sort a k)

-- -----------------
-- Argument handling
-- -----------------

data Algorithm = DoNothing
               | Allocate
               | InsertionSort
               | IntroSort
               | IntroPartialSort
               | IntroSelect
               | TriHeapSort
               | TriHeapPartialSort
               | TriHeapSelect
               | MergeSort
               | RadixSort
               deriving (Show, Read, Enum)

data Options = O { algos :: [Algorithm], elems :: Int, portion :: Int } deriving (Show)

defaultOptions :: Options
defaultOptions = O [] 10000 1000

type OptionsT = Options -> Maybe Options

options :: [OptDescr OptionsT]
options = [ Option ['A'] ["algorithm"] (ReqArg parseAlgo "ALGO") "Specify an algorithm to be run"
          ]

parseAlgo :: String -> Options -> Maybe Options
parseAlgo "None" o = Just $ o { algos = [] }
parseAlgo "All"  o = Just $ o { algos = [DoNothing .. RadixSort] }
parseAlgo s      o = fmap (\v -> o { algos = v : algos o }) $ readMaybe s

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
                [(x,s)] | all isSpace s -> Just x
                _                       -> Nothing

runTest :: MTGen -> Int -> Int -> Algorithm -> IO ()
runTest g n k alg = case alg of
  DoNothing          -> sortSuite        "no algorithm"          g n   noalgo
  Allocate           -> sortSuite        "allocate"              g n   alloc
  InsertionSort      -> sortSuite        "insertion sort"        g n   INS.sort
  IntroSort          -> sortSuite        "introsort"             g n   INT.sort
  IntroPartialSort   -> partialSortSuite "partial introsort"     g n k INT.partialSort
  IntroSelect        -> partialSortSuite "introselect"           g n k INT.select
  TriHeapSort        -> sortSuite        "tri-heap sort"         g n   TH.sort
  TriHeapPartialSort -> partialSortSuite "partial tri-heap sort" g n k TH.partialSort
  TriHeapSelect      -> partialSortSuite "tri-heap select"       g n k TH.select
  MergeSort          -> sortSuite        "merge sort"            g n   M.sort
  RadixSort          -> sortSuite        "radix sort"            g n   R.sort
  _                  -> putStrLn $ "Currently unsupported algorithm: " ++ show alg

main = do args <- getArgs
          gen  <- getStdGen
          case getOpt Permute options args of
            (fs, _, []) -> case foldl (>>=) (Just defaultOptions) fs of
              Nothing   -> putStrLn "Bad argument."
              Just opts -> mapM_ (runTest gen (elems opts) (portion opts)) (algos opts)
            _           -> putStrLn "Error..."


