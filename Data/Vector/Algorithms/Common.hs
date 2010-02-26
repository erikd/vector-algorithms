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
swap arr i j = do ei <- unsafeRead arr i
                  unsafeRead arr j >>= unsafeWrite arr i
                  unsafeWrite arr j ei
{-# INLINE swap #-}


copyOffset :: (PrimMonad m, MVector v e)
           => v (PrimState m) e -> v (PrimState m) e -> Int -> Int -> Int -> m ()
copyOffset from to iFrom iTo len =
  unsafeCopy (unsafeSlice iTo len to) (unsafeSlice iFrom len from)
{-# INLINE copyOffset #-}
