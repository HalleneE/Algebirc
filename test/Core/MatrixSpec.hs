{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.MatrixSpec (spec) where

import Prelude
import Test.Hspec
import Test.QuickCheck
import qualified Data.Vector as V

import Algebirc.Core.Types
import Algebirc.Core.Matrix

-- Helper: scalar polynomial element in GF(p)[x]
cToP :: Integer -> Poly
cToP c = V.singleton c

prop_matDet2x2Identity :: Integer -> Property
prop_matDet2x2Identity _ = 
  let p = 257
      m = PolyMatrix 2 2 $ V.fromList [cToP 1, cToP 0, cToP 0, cToP 1]
      det = matDet2x2 p m
  in V.toList det === [1]

prop_matDet3x3Identity :: Integer -> Property
prop_matDet3x3Identity _ = 
  let p = 257
      m = PolyMatrix 3 3 $ V.fromList 
        [ cToP 1, cToP 0, cToP 0
        , cToP 0, cToP 1, cToP 0
        , cToP 0, cToP 0, cToP 1 
        ]
      det = matDet3x3 p m
  in V.toList det === [1]

prop_matMulIdentity :: Integer -> Property
prop_matMulIdentity p' =
  let p = 257
      x = cToP (p' `mod` p)
      m1 = PolyMatrix 2 2 $ V.fromList [x, cToP 2, cToP 3, cToP 4]
      id2 = PolyMatrix 2 2 $ V.fromList [cToP 1, cToP 0, cToP 0, cToP 1]
      res = matMulPoly p m1 id2
  in map V.toList (V.toList (matData res)) === [[p' `mod` p], [2], [3], [4]]

spec :: Spec
spec = describe "Algebirc.Core.Matrix (Polynomial Matrix Ops)" $ do
  describe "Matrix Determinant" $ do
    it "2x2 Determinant of Identity is 1" $ property prop_matDet2x2Identity
    it "3x3 Determinant of Identity is 1" $ property prop_matDet3x3Identity
  describe "Matrix Multiplication" $ do
    it "Multiplication with Identity computes identically" $ property prop_matMulIdentity
