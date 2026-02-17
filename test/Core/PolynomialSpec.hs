module Core.PolynomialSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Algebirc.Core.Types
import Algebirc.Core.Polynomial

spec :: Spec
spec = do
  describe "Degree-Bounded Polynomials" $ do
    let p = 257
        maxDeg = 64

    -- Canonical form
    describe "Canonical Form" $ do
      it "is idempotent" $ do
        let poly = mkBoundedPoly p maxDeg [Term 3 2, Term 2 1, Term 1 0]
        polyNormalize poly `shouldBe` poly

      it "merges duplicate exponents" $ do
        let poly = mkBoundedPoly p maxDeg [Term 3 2, Term 5 2, Term 1 0]
        polyDegree poly `shouldBe` 2
        -- 3x^2 + 5x^2 = 8x^2
        polyEval poly 1 `shouldBe` ((8 + 1) `mod` p)

      it "removes zero coefficients" $ do
        let poly = mkBoundedPoly p maxDeg [Term 0 5, Term 1 0]
        polyDegree poly `shouldBe` 0

      it "sorts descending by exponent" $ do
        let poly = mkBoundedPoly p maxDeg [Term 1 0, Term 2 3, Term 3 1]
        case polyTerms poly of
          (t:_) -> termExp t `shouldBe` 3
          []    -> expectationFailure "empty polynomial"

    -- Degree cap
    describe "Degree Cap" $ do
      it "truncates terms exceeding max degree" $ do
        let poly = mkBoundedPoly p 4 [Term 1 10, Term 1 3, Term 1 0]
        polyDegree poly `shouldBe` 3

      it "blocks composition that would overflow" $ do
        let f = mkBoundedPoly p 8 [Term 1 5]
            g = mkBoundedPoly p 8 [Term 1 3]
        -- deg(f∘g) = 5 * 3 = 15 > 8
        polyCompose f g `shouldBe` Left (DegreeOverflow 15 8)

    -- Arithmetic
    describe "Arithmetic" $ do
      it "addition is correct" $ do
        let f = mkBoundedPoly p maxDeg [Term 3 2, Term 1 0]
            g = mkBoundedPoly p maxDeg [Term 2 2, Term 4 1]
            h = polyAdd f g
        -- (3x² + 1) + (2x² + 4x) = 5x² + 4x + 1
        polyEval h 10 `shouldBe` ((500 + 40 + 1) `mod` p)

      it "subtraction is correct" $ do
        let f = mkBoundedPoly p maxDeg [Term 5 1]
            g = mkBoundedPoly p maxDeg [Term 3 1]
            h = polySub f g
        polyEval h 10 `shouldBe` 20

      it "multiplication respects degree cap" $ do
        let f = mkBoundedPoly p 10 [Term 1 6]
            g = mkBoundedPoly p 10 [Term 1 6]
        -- deg = 6 + 6 = 12 > 10
        polyMul f g `shouldBe` Left (DegreeOverflow 12 10)

    -- Evaluation
    describe "Evaluation" $ do
      it "Horner evaluation matches direct computation" $ do
        let poly = mkBoundedPoly p maxDeg [Term 2 3, Term 3 1, Term 7 0]
        -- f(5) = 2*125 + 3*5 + 7 = 272 mod 257 = 15
        polyEval poly 5 `shouldBe` 15

      it "zero polynomial evaluates to 0" $ do
        let z = zeroPoly p maxDeg
        polyEval z 42 `shouldBe` 0

    -- Lagrange Interpolation
    describe "Lagrange Interpolation" $ do
      it "interpolates through given points" $ do
        case lagrangeInterpolate p maxDeg [(1,3),(2,7),(3,13)] of
          Right poly -> do
            polyEval poly 1 `shouldBe` 3
            polyEval poly 2 `shouldBe` 7
            polyEval poly 3 `shouldBe` 13
          Left err -> expectationFailure (show err)

      it "rejects empty point set" $ do
        lagrangeInterpolate p maxDeg [] `shouldBe` Left (GenericError "Empty point set")
