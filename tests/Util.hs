{-# LANGUAGE TypeOperators #-}

module Util where

import Control.Monad

import Data.Word
import Data.Int

import Data.Array.Vector

import Test.QuickCheck


instance (Arbitrary e, UA e) => Arbitrary (UArr e) where
  arbitrary = fmap toU arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (a :*: b) where
  arbitrary = (:*:) `fmap` arbitrary `ap` arbitrary

instance Arbitrary Int8 where
  arbitrary = fromInteger `fmap` arbitrary

instance Arbitrary Int16 where
  arbitrary = fromInteger `fmap` arbitrary

instance Arbitrary Int32 where
  arbitrary = fromInteger `fmap` arbitrary

instance Arbitrary Int64 where
  arbitrary = fromInteger `fmap` arbitrary

instance Arbitrary Word8 where
  arbitrary = fromInteger `fmap` arbitrary

instance Arbitrary Word16 where
  arbitrary = fromInteger `fmap` arbitrary

instance Arbitrary Word32 where
  arbitrary = fromInteger `fmap` arbitrary

instance Arbitrary Word64 where
  arbitrary = fromInteger `fmap` arbitrary

instance Arbitrary Word where
  arbitrary = fromInteger `fmap` arbitrary
