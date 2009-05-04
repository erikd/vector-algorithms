
module Util where

import Data.Array.Vector

import Test.QuickCheck


instance (Arbitrary e, UA e) => Arbitrary (UArr e) where
  arbitrary = fmap toU arbitrary