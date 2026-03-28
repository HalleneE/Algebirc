module NonlinearSpec (spec) where

import Test.Hspec
import Algebirc.Core.Types
import Algebirc.Obfuscation.NonlinearTransform
import Algebirc.Obfuscation.DegreeControl
import Algebirc.Obfuscation.Pipeline (buildPipeline, runPipelinePoly, invertPipelinePoly)

-- | Test configuration
testCfg :: ObfuscationConfig
testCfg = defaultConfig { cfgFieldPrime = 257, cfgMaxDegree = 8 }

-- | Test secret key
testKey :: SecretKey
testKey = SecretKey
  { skSeed     = 12345
  , skPowerExp = 3
  }

-- | Simple test polynomial: 1 + 2x + 3x² + 99x³
testPoly :: BoundedPoly
testPoly = mkBoundedPoly 257 8
  [ Term 1 0, Term 2 1, Term 3 2, Term 99 3 ]

spec :: Spec
spec = do
  describe "1. MDS Diffusion [P2]" $ do
    it "mdsDiffusePure changes all coefficients" $ do
      let p = 257
          cs = [1, 2, 3, 4, 5]
          mixed = mdsDiffusePure p 5 cs
      mixed `shouldNotBe` cs
      length mixed `shouldBe` 5

  describe "2. Power Map [P1]" $ do
    it "power map x^3 mod 257 round-trips" $ do
      case mkPowerMapTransform 3 257 of
        Left err -> expectationFailure (show err)
        Right t -> case applyNonlinear testCfg t testPoly of
          Left err -> expectationFailure (show err)
          Right obf -> case invertNonlinear testCfg t obf of
            Left err -> expectationFailure (show err)
            Right recovered -> recovered `shouldBe` testPoly

  describe "3. Pure Pipeline Generation" $ do
    it "generates a 5-layer pure pipeline" $ do
      case generatePipeline testCfg testKey of
        Left err -> expectationFailure (show err)
        Right pipeline -> length pipeline `shouldBe` 5

    it "pipeline round-trips exactly" $ do
      case buildPipeline testCfg of
        Left err -> expectationFailure (show err)
        Right pipeline -> do
          case runPipelinePoly testCfg pipeline testPoly of
            Left err -> expectationFailure (show err)
            Right obf -> case invertPipelinePoly testCfg pipeline obf of
              Left err -> expectationFailure (show err)
              Right recovered -> recovered `shouldBe` testPoly
