{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.ResultantSpec (spec) where

import Prelude
import Test.Hspec
import Test.QuickCheck
import qualified Data.Vector as V

import Algebirc.Core.Types
import Algebirc.Geometry.HyperellipticCurve (polyResultant)

-- | Explicit 2x2 Sylvester determinant for linear polynomials A and B
-- A(x) = a1*x + a0
-- B(x) = b1*x + b0
-- Res(A, B) = a1*b0 - a0*b1
prop_linearResultant :: Integer -> Integer -> Integer -> Integer -> Property
prop_linearResultant a1 a0 b1 b0 =
  let p = 257  -- standard test prime
      a1' = if a1 `mod` p == 0 then 1 else a1 `mod` p
      b1' = if b1 `mod` p == 0 then 1 else b1 `mod` p
      polyA = V.fromList [a0 `mod` p, a1']
      polyB = V.fromList [b0 `mod` p, b1']
      expected = ((a1' * (b0 `mod` p)) - (a0 `mod` p) * b1') `mod` p
      expectedPos = (expected + p) `mod` p
      res = polyResultant p polyA polyB
  in counterexample ("Failed on A=" ++ show (V.toList polyA) ++ " B=" ++ show (V.toList polyB)) $
       res === expectedPos

-- | Explicit 3x2 Sylvester determinant for quadratic A and linear B
-- A(x) = a2*x^2 + a1*x + a0
-- B(x) = b1*x + b0
-- Res(A,B) = a2*b0^2 - a1*b0*b1 + a0*b1^2
prop_quadraticLinearResultant :: Integer -> Integer -> Integer -> Integer -> Integer -> Property
prop_quadraticLinearResultant a2 a1 a0 b1 b0 =
  let p = 257
      a2' = if a2 `mod` p == 0 then 1 else a2 `mod` p
      b1' = if b1 `mod` p == 0 then 1 else b1 `mod` p
      polyA = V.fromList [a0 `mod` p, a1 `mod` p, a2']
      polyB = V.fromList [b0 `mod` p, b1']
      
      a0' = a0 `mod` p; a1' = a1 `mod` p; b0' = b0 `mod` p
      expected = (a2' * b0' * b0' - a1' * b0' * b1' + a0' * b1' * b1') `mod` p
      expectedPos = (expected + p) `mod` p
      res = polyResultant p polyA polyB
  in counterexample "Failed 3x2 Resultant Identity" $
       res === expectedPos

spec :: Spec
spec = describe "Algebirc.Core.Resultant (Algebra Engine V2)" $ do
  describe "Resultant over GF(p)" $ do
    it "Computes correct Resultant for linear polynomials (2x2 Sylvester) against naive formula" $
      property prop_linearResultant
    it "Computes correct Resultant for quadratic vs linear polynomials (3x2 Sylvester) against naive formula" $
      property prop_quadraticLinearResultant
