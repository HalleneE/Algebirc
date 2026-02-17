-- | Tests for Phase 5A: Nonlinear Bijective Transforms
module NonlinearSpec (spec) where

import Test.Hspec
import Algebirc.Core.Types
import Algebirc.Obfuscation.NonlinearTransform
import Algebirc.Obfuscation.DegreeControl
import qualified Algebirc.Analysis.Invertibility as Inv
import qualified Data.Vector.Unboxed as VU

-- | Test configuration
testCfg :: ObfuscationConfig
testCfg = defaultConfig { cfgFieldPrime = 257, cfgMaxDegree = 8 }

-- | Test secret key
testKey :: SecretKey
testKey = SecretKey
  { skSeed     = 12345
  , skRounds   = 3
  , skSBoxSeed = 67890
  , skPowerExp = 3
  }

-- | Simple test polynomial: 1 + 2x + 3x² + 99x³
testPoly :: BoundedPoly
testPoly = mkBoundedPoly 257 8
  [ Term 1 0, Term 2 1, Term 3 2, Term 99 3 ]

-- | Dense test polynomial with all positions filled
densePoly :: BoundedPoly
densePoly = mkBoundedPoly 257 8
  [ Term (fromIntegral i * 17 + 3) i | i <- [0..8] ]

spec :: Spec
spec = do
  describe "S-Box Construction" $ do
    it "generates a valid bijective S-box from seed" $ do
      case generateSBox 257 42 of
        Left err -> expectationFailure (show err)
        Right sb -> do
          sboxPrime sb `shouldBe` 257
          VU.length (sboxFwd sb) `shouldBe` 257

    it "rejects non-bijective forward table" $ do
      let bad = VU.fromList (replicate 257 0)  -- all zeros = not bijective
      case mkSBox 257 bad of
        Left _ -> return ()
        Right _ -> expectationFailure "Should reject non-bijective table"

    it "S-box forward/inverse round-trips for all elements" $ do
      case generateSBox 257 42 of
        Left err -> expectationFailure (show err)
        Right sb -> do
          let allRt = all (\x -> sboxInvert sb (sboxApply sb x) == x) [0..256]
          allRt `shouldBe` True

    it "S-box is nonlinear (not identity or simple shift)" $ do
      case generateSBox 257 42 of
        Left err -> expectationFailure (show err)
        Right sb -> do
          -- S(x) ≠ x for at least some x
          let diffs = length [ x | x <- [0..256], sboxApply sb x /= x ]
          diffs `shouldSatisfy` (> 100)  -- Most elements should be shuffled

  describe "S-Box Transform" $ do
    it "S-box transform round-trips polynomial" $ do
      case generateSBox 257 42 of
        Left err -> expectationFailure (show err)
        Right sb -> do
          let t = mkSBoxTransform sb
          case applyNonlinear testCfg t testPoly of
            Left err -> expectationFailure (show err)
            Right obf -> case invertNonlinear testCfg t obf of
              Left err -> expectationFailure (show err)
              Right recovered -> recovered `shouldBe` testPoly

    it "S-box transform changes coefficients" $ do
      case generateSBox 257 42 of
        Left err -> expectationFailure (show err)
        Right sb -> do
          let t = mkSBoxTransform sb
          case applyNonlinear testCfg t testPoly of
            Left err -> expectationFailure (show err)
            Right obf -> obf `shouldNotBe` testPoly

  describe "Power Map" $ do
    it "power map x^3 mod 257 round-trips (gcd(3,256)=1)" $ do
      case mkPowerMapTransform 3 257 of
        Left err -> expectationFailure (show err)
        Right t -> case applyNonlinear testCfg t testPoly of
          Left err -> expectationFailure (show err)
          Right obf -> case invertNonlinear testCfg t obf of
            Left err -> expectationFailure (show err)
            Right recovered -> recovered `shouldBe` testPoly

    it "power map x^5 mod 257 round-trips (gcd(5,256)=1)" $ do
      case mkPowerMapTransform 5 257 of
        Left err -> expectationFailure (show err)
        Right t -> case applyNonlinear testCfg t testPoly of
          Left err -> expectationFailure (show err)
          Right obf -> case invertNonlinear testCfg t obf of
            Left err -> expectationFailure (show err)
            Right recovered -> recovered `shouldBe` testPoly

    it "rejects non-coprime exponent (e=2, p-1=256)" $ do
      case mkPowerMapTransform 2 257 of
        Left _  -> return ()
        Right _ -> expectationFailure "Should reject e=2 (gcd(2,256)=2)"

    it "power map changes coefficients nonlinearly" $ do
      case mkPowerMapTransform 3 257 of
        Left err -> expectationFailure (show err)
        Right t -> case applyNonlinear testCfg t testPoly of
          Left err -> expectationFailure (show err)
          Right obf -> obf `shouldNotBe` testPoly

  describe "Feistel Network" $ do
    it "Feistel 3-round round-trips with S-box + quadratic F" $ do
      case generateSBox 257 42 of
        Left err -> expectationFailure (show err)
        Right sb -> do
          let t = mkFeistelTransform sb 3 testKey
          case applyNonlinear testCfg t testPoly of
            Left err -> expectationFailure (show err)
            Right obf -> case invertNonlinear testCfg t obf of
              Left err -> expectationFailure (show err)
              Right recovered -> recovered `shouldBe` testPoly

    it "Feistel 5-round round-trips" $ do
      case generateSBox 257 42 of
        Left err -> expectationFailure (show err)
        Right sb -> do
          let t = mkFeistelTransform sb 5 testKey
          case applyNonlinear testCfg t testPoly of
            Left err -> expectationFailure (show err)
            Right obf -> case invertNonlinear testCfg t obf of
              Left err -> expectationFailure (show err)
              Right recovered -> recovered `shouldBe` testPoly

    it "Feistel enforces minimum 3 rounds" $ do
      case generateSBox 257 42 of
        Left err -> expectationFailure (show err)
        Right sb -> do
          let t = mkFeistelTransform sb 1 testKey  -- request 1, should get 3
          transformRounds t `shouldBe` 3

    it "Feistel changes coefficients significantly" $ do
      case generateSBox 257 42 of
        Left err -> expectationFailure (show err)
        Right sb -> do
          let t = mkFeistelTransform sb 3 testKey
          case applyNonlinear testCfg t densePoly of
            Left err -> expectationFailure (show err)
            Right obf -> obf `shouldNotBe` densePoly

    it "Feistel with dense polynomial round-trips" $ do
      case generateSBox 257 42 of
        Left err -> expectationFailure (show err)
        Right sb -> do
          let t = mkFeistelTransform sb 4 testKey
          case applyNonlinear testCfg t densePoly of
            Left err -> expectationFailure (show err)
            Right obf -> case invertNonlinear testCfg t obf of
              Left err -> expectationFailure (show err)
              Right recovered -> recovered `shouldBe` densePoly

  describe "Key-Based Pipeline Generation" $ do
    it "generates a valid 6-layer pipeline from key" $ do
      case generatePipeline testCfg testKey of
        Left err -> expectationFailure (show err)
        Right pipeline -> length pipeline `shouldBe` 6

    it "pipeline structure extraction is correct" $ do
      case generatePipeline testCfg testKey of
        Left err -> expectationFailure (show err)
        Right pipeline -> do
          let structure = extractStructure testCfg pipeline
          psLayerCount structure `shouldBe` 6
          psFieldPrime structure `shouldBe` 257
          psDegree structure `shouldBe` 8

    it "different keys produce different pipelines" $ do
      let key2 = testKey { skSeed = 99999, skSBoxSeed = 11111 }
      case (generatePipeline testCfg testKey, generatePipeline testCfg key2) of
        (Right p1, Right p2) -> do
          let s1 = extractStructure testCfg p1
              s2 = extractStructure testCfg p2
          -- Different keys should produce different layer orderings
          psTransformTags s1 `shouldNotBe` psTransformTags s2
        _ -> expectationFailure "Pipeline generation failed"

  describe "Degree Control" $ do
    it "computes degree budget for pipeline" $ do
      case generatePipeline testCfg testKey of
        Left err -> expectationFailure (show err)
        Right pipeline -> do
          let budget = computeDegreeBudget testCfg pipeline 3
          dbExceeded budget `shouldBe` False
          dbBudget budget `shouldBe` 8

    it "validates pipeline within budget" $ do
      case generatePipeline testCfg testKey of
        Left err -> expectationFailure (show err)
        Right pipeline ->
          validatePipeline testCfg pipeline 3 `shouldBe` Nothing

    it "degree budget tracks per-layer metrics" $ do
      case generatePipeline testCfg testKey of
        Left err -> expectationFailure (show err)
        Right pipeline -> do
          let budget = computeDegreeBudget testCfg pipeline 3
          length (dbPerLayer budget) `shouldBe` 6

  describe "Avalanche Metric" $ do
    it "S-box has high avalanche (many coeffs change from 1-bit input change)" $ do
      case generateSBox 257 42 of
        Left err -> expectationFailure (show err)
        Right sb -> do
          let t = mkSBoxTransform sb
              pipeline p = applyNonlinear testCfg t p
          case measureAvalanche testCfg pipeline densePoly of
            Left err -> expectationFailure (show err)
            Right result -> do
              arTotalCoeffs result `shouldBe` 9  -- deg 8 + 1
              arChangedCoeffs result `shouldSatisfy` (> 0)

    it "Feistel has avalanche within its pair (paired architecture)" $ do
      case generateSBox 257 42 of
        Left err -> expectationFailure (show err)
        Right sb -> do
          let t = mkFeistelTransform sb 4 testKey
              pipeline p = applyNonlinear testCfg t p
          case measureAvalanche testCfg pipeline densePoly of
            Left err -> expectationFailure (show err)
            Right result ->
              -- Paired Feistel: perturbation at pos 0 affects pair (0,1)
              -- So at least 1-2 coefficients should change
              arChangedCoeffs result `shouldSatisfy` (>= 1)

  describe "Invertibility Analysis Integration" $ do
    it "S-box marked as invertible by Invertibility module" $ do
      case generateSBox 257 42 of
        Left err -> expectationFailure (show err)
        Right sb -> do
          let t = mkSBoxTransform sb
              result = Inv.checkInvertibility testCfg t
          case result of
            Inv.Invertible _ -> return ()
            _ -> expectationFailure $ "Expected Invertible, got: " ++ show result

    it "Feistel marked as invertible by Invertibility module" $ do
      case generateSBox 257 42 of
        Left err -> expectationFailure (show err)
        Right sb -> do
          let t = mkFeistelTransform sb 3 testKey
              result = Inv.checkInvertibility testCfg t
          case result of
            Inv.Invertible _ -> return ()
            _ -> expectationFailure $ "Expected Invertible, got: " ++ show result

    it "PowerMap marked as invertible by Invertibility module" $ do
      case mkPowerMapTransform 3 257 of
        Left err -> expectationFailure (show err)
        Right t -> do
          let result = Inv.checkInvertibility testCfg t
          case result of
            Inv.Invertible _ -> return ()
            _ -> expectationFailure $ "Expected Invertible, got: " ++ show result
