
-- ---------------------------------------------------------------------------
-- |
-- Module      : Data.Vector.Algorithms.Tim
-- Stability   : Experimental
--
-- Timsort is a complex, adaptive, bottom-up merge sort. It is designed to
-- minimize comparisons as much as possible, even at some cost in overhead.
-- Thus, it may not be ideal for sorting simple primitive types, for which
-- comparison is cheap. It may, however, be significantly faster for sorting
-- arrays of complex values (strings would be an example, though an algorithm
-- not based on comparison would probably be superior in that particular
-- case).
--
-- For more information on the details of the algorithm, read on.
--
-- The first step of the algorithm is to identify runs of elements. These can
-- either be non-decreasing or strictly decreasing sequences of elements in
-- the input. Strictly decreasing sequences are used rather than
-- non-increasing so that they can be easily reversed in place without the
-- sort becoming unstable.
--
-- If the natural runs are too short, they are padded to a minimum value. The
-- minimum is chosen based on the length of the array, and padded runs are put
-- in order using insertion sort. The length of the minimum run size is
-- determined as follows:
--
--   * If the length of the array is less than 64, the minimum size is the
--     length of the array, and insertion sort is used for the entirety
--
--   * Otherwise, a value between 32 and 64 is chosen such that N/min is
--     either equal to or just below a power of two. This avoids having a
--     small chunk left over to merge into much larger chunks at the end.
--
-- This is accomplished by taking the the mininum to be the lowest six bits
-- containing the highest set bit, and adding one if any other bits are set.
-- For instance:
--
--     length: 00000000 00000000 00000000 00011011 = 25
--     min:    00000000 00000000 00000000 00011011 = 25
--
--     length: 00000000 11111100 00000000 00000000 = 63 * 2^18
--     min:    00000000 00000000 00000000 00111111 = 63
--
--     length: 00000000 11111100 00000000 00000001 = 63 * 2^18 + 1
--     min:    00000000 00000000 00000000 01000000 = 64
--
-- Once chunks can be produced, the next step is merging them. The indices of
-- all runs are stored in a stack. When we identify a new run, we push it onto
-- the stack. However, certain invariants are maintained of the stack entries.
-- Namely:
--
--   if stk = _ :> z :> y :> x
--     length x + length y < length z
--
--   if stk = _ :> y :> x
--     length x < length y
--
-- This ensures that the chunks stored are decreasing, and that the chunk
-- sizes follow something like the fibonacci sequence, ensuring there at most
-- log-many chunks at any time. If pushing a new chunk on the stack would
-- violate either of the invariants, we first perform a merge.
--
-- If length x + length y >= length z, then y is merged with the smaller of x
-- and z (if they are tied, x is chosen). If, further,  length x >= length y
-- then they are merged.
module Data.Vector.Algorithms.Tim
  (
  ) where

-- | Stores the position data for a merge-eligible chunk. The chunk denoted is
-- [start, end)
data Chunk = Chunk { start :: {-# UNPACK #-} !Int
                   , end   :: {-# UNPACK #-} !Int
                   }

size :: Chunk -> Int
size (Chunk start end) = end - start

-- | Computes the minimum run size for the sort. The goal is to choose a size
-- such that there are almost if not exactly 2^n chunks of that size in the
-- array.
minrun :: Int -> Int
minrun n0 = (n0 `unsafeShiftR` extra) + if (lowMask .&. n0) > 0 then 1 else 0
 where
 -- smear the bits down from the most significant bit
 !n1 = n0 .|. unsafeShiftR n0 1
 !n2 = n1 .|. unsafeShiftR n1 2
 !n3 = n2 .|. unsafeShiftR n2 4
 !n4 = n3 .|. unsafeShiftR n3 8
 !n5 = n4 .|. unsafeShiftR n4 16
 !n6 = n5 .|. unsafeShiftR n5 32

 -- mask for the bits lower than the 6 highest bits
 !lowMask = n6 `unsafeShiftR` 6

 !extra = popCount lowMask

-- | Stores a list of chunks that are waiting to be merged. The following
-- invariants are maintained at all times during the algorithm:
--
-- 1) In cs :> a@(Chunk s1 e1) :> b@(Chunk s2 e2)
--      e1 = s2
--      size a > size b
-- 2) In cs :> a :> b :> c
--     size a > size b + size c
--
-- If adding a new chunk would violate one of these invariants, merges are
-- performed to maintain them.
data Chunks = None | Chunks :> {-# UNPACK #-} !Chunk


