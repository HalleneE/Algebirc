module PureAlgebraicSpec (spec) where

import Test.Hspec
import Algebirc.Geometry.EllipticCurve (modPow, modInv, modSqrt, branchlessSelect, isZeroField)
import Algebirc.Geometry.HyperellipticCurve (polyMul, polyMakeMonic)
import Algebirc.Geometry.RichelotIsogeny (findRootsPure, factorSextic)
import Algebirc.Geometry.Isogeny (algebraicSponge)
import Algebirc.Core.Matrix (FieldMatrix(..), matApplyField, mkCauchyMatrix)
import Algebirc.Core.Types
import qualified Data.Vector as V

spec :: Spec
spec = do
  describe "1. Branchless Arithmetic [P15]" $ do
    it "branchlessSelect correctly picks values" $ do
      let p = 257
      branchlessSelect p 1 100 200 `shouldBe` 100
      branchlessSelect p 0 100 200 `shouldBe` 200

    it "isZeroField correctly identifies zero" $ do
      let p = 257
      isZeroField p 0 `shouldBe` 1
      isZeroField p 42 `shouldBe` 0

  describe "2. MDS Matrices [P2]" $ do
    it "mkCauchyMatrix generates non-zero elements" $ do
      let p = 257
          n = 5
          m = mkCauchyMatrix p n
      V.all (/= 0) (fieldData m) `shouldBe` True

    it "matApplyField performs correct multiplication" $ do
      let p = 257
          m = FieldMatrix 2 2 (V.fromList [1, 2, 3, 4])
          v = [10, 20]
          -- 1*10 + 2*20 = 50
          -- 3*10 + 4*20 = 110
      matApplyField p m v `shouldBe` [50, 110]

  describe "3. Cantor-Zassenhaus Factorization [P5]" $ do
    it "findRootsPure finds all roots of a split polynomial" $ do
      let p = 257
          -- f(x) = (x-1)(x-2)(x-3) = x^3 - 6x^2 + 11x - 6
          f = V.fromList [251, 11, 251, 1] -- coefficients mod 257
          roots = findRootsPure p 42 f
      length roots `shouldBe` 3
      roots `shouldMatchList` [1, 2, 3]

    it "factorSextic splits a sextic into 3 quadratics" $ do
      let p = 257
          -- f(x) = (x^2+1)(x^2+2)(x^2+3)
          -- x^2+1 = (x-16)(x+16)
          -- x^2+2 = (x-131)(x-126)
          -- x^2+3 is irreducible
          g1 = V.fromList [1, 0, 1]
          g2 = V.fromList [2, 0, 1]
          g3 = V.fromList [3, 0, 1]
          f = polyMul p (polyMul p g1 g2) g3
          (r1, r2, r3) = factorSextic p 99 f
      length [r1, r2, r3] `shouldBe` 3
      polyMul p (polyMul p r1 r2) r3 `shouldBe` polyMakeMonic p f

  describe "4. Algebraic Sponge PRF [P7]" $ do
    it "algebraicSponge is deterministic" $ do
      let p = 257
          ivs = [10, 20, 30]
          s1 = algebraicSponge p ivs 10
          s2 = algebraicSponge p ivs 10
      s1 `shouldBe` s2

    it "algebraicSponge produces different outputs for different IVs" $ do
      let p = 257
          s1 = algebraicSponge p [1] 10
          s2 = algebraicSponge p [2] 10
      s1 `shouldNotBe` s2
