{-# LANGUAGE OverloadedStrings #-}

module Core.U256Spec (spec) where

import Test.Hspec
import Test.QuickCheck
import Algebirc.Core.U256
import Data.Word (Word64)
import Data.Bits

-- QuickCheck generator for U256
instance Arbitrary U256 where
  arbitrary = U256 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

spec :: Spec
spec = describe "Algebirc.Core.U256 (Hardware Constant-Time 256-bit Unboxed)" $ do
  
  describe "Conversion" $ do
    it "toU256 and fromU256 are isomorphic for 256-bit boundaries" $ property $ \n ->
      let n' = abs n `mod` (2^(256 :: Int))
      in fromU256 (toU256 n') === n'
  
  describe "Constant-Time Select (ctSelect256)" $ do
    it "Returns first argument when cond == 1" $ property $ \a b ->
      ctSelect256 1 a b === a
    
    it "Returns second argument when cond == 0" $ property $ \a b ->
      ctSelect256 0 a b === b

  describe "Constant-Time Zero Check (ctIsZero256)" $ do
    it "Returns 1 for U256 0 0 0 0" $ do
      ctIsZero256 (U256 0 0 0 0) `shouldBe` 1
    
    it "Returns 0 for non-zero values" $ property $ \v@(U256 l0 l1 l2 l3) ->
      (l0 /= 0 || l1 /= 0 || l2 /= 0 || l3 /= 0) ==>
        ctIsZero256 v === 0

  describe "Constant-Time Equality Check (ctEq256)" $ do
    it "Returns 1 when equal" $ property $ \a ->
      ctEq256 a a === 1
      
    it "Returns 0 when not equal" $ property $ \a b ->
      a /= b ==> ctEq256 a b === 0

  describe "Addition (add256)" $ do
    it "Computes addition modulo 2^256 correctly" $ property $ \a b ->
      let intA = fromU256 a
          intB = fromU256 b
          expected = (intA + intB) `mod` (2^(256 :: Int))
      in fromU256 (add256 a b) === expected

  describe "Subtraction (sub256)" $ do
    it "Computes subtraction modulo 2^256 correctly" $ property $ \a b ->
      let intA = fromU256 a
          intB = fromU256 b
          expected = (intA - intB) `mod` (2^(256 :: Int))
      in fromU256 (sub256 a b) === expected
