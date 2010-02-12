{-# LANGUAGE CPP #-}

-- ---------------------------------------------------------------------------
-- |
-- Module      : Data.Vector.Algorithms.Common
-- Copyright   : (c) 2008-2010 Dan Doel
-- Maintainer  : Dan Doel
-- Stability   : Experimental
-- Portability : Portable
--
-- Common operations and utility functions for all sorts

module Data.Vector.Algorithms.Common where

import Prelude hiding (read, length)

import Control.Monad.Primitive

import Data.Vector.Generic.Mutable

#include "vector.h"

-- | A type of comparisons between two values of a given type.
type Comparison e = e -> e -> Ordering

-- | Swaps the elements at two positions in an array.
swap :: (PrimMonad m, MVector v e) => v (PrimState m) e -> Int -> Int -> m ()
swap arr i j = do ei <- read arr i
                  read arr j >>= write arr i
                  write arr j ei
{-# INLINE swap #-}


copyOffset :: (PrimMonad m, MVector v e)
           => v (PrimState m) e -> v (PrimState m) e -> Int -> Int -> Int -> m ()
copyOffset from to iFrom iTo len =
    BOUNDS_CHECK(checkIndex) "copyOffset" iFrom             (length from)
  $ BOUNDS_CHECK(checkIndex) "copyOffset" (iFrom + len - 1) (length from)  
  $ BOUNDS_CHECK(checkIndex) "copyOffset" iTo               (length to)  
  $ BOUNDS_CHECK(checkIndex) "copyOffset" (iTo + len - 1)   (length to)
  $ go 0
 where
 go n | n < len   = unsafeRead from (iFrom + n) >>= unsafeWrite to (iTo + n) >> go (n+1)
      | otherwise = return ()
{-# INLINE copyOffset #-}

