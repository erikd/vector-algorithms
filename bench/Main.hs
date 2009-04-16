{-# LANGUAGE Rank2Types #-}

module Main (main) where

import Control.Monad
import Control.Monad.ST
import Control.Monad.Error

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

type OptionsT = Options -> Either String Options

options :: [OptDescr OptionsT]
options = [ Option ['A'] ["algorithm"] (ReqArg parseAlgo "ALGO")
               "Specify an algorithm to be run"
          , Option ['n'] ["num-elems"] (ReqArg parseN    "N")
               "Specify the size of arrays in algorithms"
          , Option ['k'] ["portion"]   (ReqArg parseK    "K")
               "Specify the number of elements to partial sort/select in relevant algorithms."
          ]

parseAlgo :: String -> Options -> Either String Options
parseAlgo "None" o = Right $ o { algos = [] }
parseAlgo "All"  o = Right $ o { algos = [DoNothing .. RadixSort] }
parseAlgo s      o = fmap (\v -> o { algos = v : algos o }) $ readEither s

parseNum :: (Int -> Options) -> String -> Either String Options
parseNum f = fmap f . readEither

parseN, parseK :: String -> Options -> Either String Options
parseN s o = parseNum (\n -> o { elems   = n }) s
parseK s o = parseNum (\k -> o { portion = k }) s

readEither :: Read a => String -> Either String a
readEither s = case reads s of
  [(x,s)] | all isSpace s -> Right x
  _                       -> Left $ "Bad parse: " ++ s

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

main :: IO ()
main = do args <- getArgs
          gen  <- getStdGen
          case getOpt Permute options args of
            (fs, _, []) -> case foldl (>>=) (Right defaultOptions) fs of
              Left err   -> putStrLn $ "Error parsing arguments: " ++ err
              Right opts -> mapM_ (runTest gen (elems opts) (portion opts)) (algos opts)
            _            -> putStrLn "Error..."


