-- |
-- Module      : AnalysisSpec
-- Description : Tests for Phase 3 analysis modules
--
-- Tests:
-- 1. DegreeTracker: metrics match manual calculation
-- 2. Invertibility: correct verdicts for all transform types
-- 3. Leakage: affine collapse detection, entropy, scoring

module AnalysisSpec (spec) where

import Test.Hspec
import Algebirc.Core.Types
import Algebirc.Core.Group (generateFromSeed)
import Algebirc.Obfuscation.Transform
import Algebirc.Analysis.DegreeTracker
import Algebirc.Analysis.Invertibility
import Algebirc.Analysis.Leakage

spec :: Spec
spec = do
  degreeTrackerSpec
  invertibilitySpec
  leakageSpec

-- ============================================================
-- 1. DegreeTracker Tests
-- ============================================================

degreeTrackerSpec :: Spec
degreeTrackerSpec = describe "DegreeTracker" $ do
  let cfg = defaultConfig
      p = cfgFieldPrime cfg

  it "tracks term count growth through affine transforms" $ do
    let poly = mkBoundedPoly p 64 [Term 42 0, Term 100 1, Term 200 2]
        transforms = [mkAffineTransform 37 13]
    case trackedPipeline cfg transforms poly of
      Right (_, report) -> do
        trLayerCount report `shouldBe` 1
        trInputTerms report `shouldBe` 3
        -- After affine on maxDeg=64, all positions filled with b
        trOutputTerms report `shouldSatisfy` (> 3)
        length (trSnapshots report) `shouldBe` 2  -- INPUT + 1 layer
      Left err -> expectationFailure (show err)

  it "density increases with affine transforms (dense fill)" $ do
    let poly = mkBoundedPoly p 64 [Term 42 0, Term 100 1]
        transforms = [mkAffineTransform 37 13]
    case trackedPipeline cfg transforms poly of
      Right (_, report) -> do
        maxDensity report `shouldSatisfy` (> 0.5)
      Left err -> expectationFailure (show err)

  it "permutation preserves term count exactly" $ do
    let poly = mkBoundedPoly p 64 [Term 42 0, Term 100 1, Term 200 2, Term 55 3]
        perm = generateFromSeed 64 42
        transforms = [mkPermTransform perm]
    case trackedPipeline cfg transforms poly of
      Right (_, report) -> do
        trInputTerms report `shouldBe` trOutputTerms report
      Left err -> expectationFailure (show err)

  it "is no-op when analysis disabled" $ do
    let cfg' = cfg { cfgEnableAnalysis = False }
        poly = mkBoundedPoly p 64 [Term 42 0, Term 100 1]
        transforms = [mkAffineTransform 37 13]
    case trackedPipeline cfg' transforms poly of
      Right (_, report) -> do
        trSnapshots report `shouldBe` []
        trDirection report `shouldBe` "forward"
      Left err -> expectationFailure (show err)

  it "report has correct number of snapshots for multi-layer pipeline" $ do
    let poly = mkBoundedPoly p 64 [Term 42 0, Term 100 1, Term 200 2]
        transforms = [ mkAffineTransform 37 13
                     , mkPermTransform (generateFromSeed 64 99)
                     , mkAffineTransform 11 7
                     ]
    case trackedPipeline cfg transforms poly of
      Right (_, report) -> do
        length (trSnapshots report) `shouldBe` 4  -- INPUT + 3 layers
      Left err -> expectationFailure (show err)

  it "formatReport produces non-empty output" $ do
    let poly = mkBoundedPoly p 64 [Term 42 0, Term 100 1]
        transforms = [mkAffineTransform 37 13]
    case trackedPipeline cfg transforms poly of
      Right (_, report) -> do
        let output = formatReport report
        length output `shouldSatisfy` (> 50)  -- reasonable output
      Left err -> expectationFailure (show err)

  it "hasExplosion detects term count surge" $ do
    let poly = mkBoundedPoly p 64 [Term 42 0, Term 100 1]
        transforms = [mkAffineTransform 37 13]
    case trackedPipeline cfg transforms poly of
      Right (_, report) -> do
        hasExplosion 60 report `shouldBe` True   -- 65 terms > 60
        hasExplosion 100 report `shouldBe` False  -- 65 terms < 100
      Left err -> expectationFailure (show err)

-- ============================================================
-- 2. Invertibility Tests
-- ============================================================

invertibilitySpec :: Spec
invertibilitySpec = describe "Invertibility" $ do
  let cfg = defaultConfig
      p = cfgFieldPrime cfg

  it "affine with a=37, p=257: gcd(37,257)=1 → invertible" $ do
    let t = mkAffineTransform 37 13
        result = checkInvertibility cfg t
    case result of
      Invertible _ -> return ()
      other -> expectationFailure $ "Expected Invertible, got: " ++ show other

  it "affine with a=0: not invertible" $ do
    let t = mkAffineTransform 0 13
        result = checkInvertibility cfg t
    case result of
      NotInvertible _ -> return ()
      other -> expectationFailure $ "Expected NotInvertible, got: " ++ show other

  it "affine with a=p (≡ 0 mod p): not invertible" $ do
    let t = mkAffineTransform p 13
        result = checkInvertibility cfg t
    case result of
      NotInvertible _ -> return ()
      other -> expectationFailure $ "Expected NotInvertible, got: " ++ show other

  it "permutation: always invertible" $ do
    let perm = generateFromSeed 32 42
        t = mkPermTransform perm
        result = checkInvertibility cfg t
    case result of
      Invertible _ -> return ()
      other -> expectationFailure $ "Expected Invertible, got: " ++ show other

  it "polynomial sub degree-1: conditionally invertible" $ do
    let poly = mkBoundedPoly p 64 [Term 37 1, Term 13 0]  -- 37x + 13
        t = mkPolyTransform poly
        result = checkInvertibility cfg t
    case result of
      ConditionallyInvertible _ -> return ()
      other -> expectationFailure $ "Expected ConditionallyInvertible, got: " ++ show other

  it "polynomial sub degree-3: not invertible" $ do
    let poly = mkBoundedPoly p 64 [Term 1 3, Term 2 1, Term 3 0]  -- x^3 + 2x + 3
        t = mkPolyTransform poly
        result = checkInvertibility cfg t
    case result of
      NotInvertible _ -> return ()
      other -> expectationFailure $ "Expected NotInvertible, got: " ++ show other

  it "composite of all-invertible: invertible" $ do
    let ts = [ mkAffineTransform 37 13
             , mkPermTransform (generateFromSeed 64 42)
             , mkAffineTransform 11 7
             ]
        t = mkCompositeTransform ts
        result = checkInvertibility cfg t
    case result of
      Invertible _ -> return ()
      other -> expectationFailure $ "Expected Invertible, got: " ++ show other

  it "pipeline proof: all affine+perm → fully invertible" $ do
    let transforms = [ mkAffineTransform 37 13
                     , mkPermTransform (generateFromSeed 64 42)
                     , mkAffineTransform 11 7
                     ]
        proof = checkPipelineInvertibility cfg transforms
    case ppVerdict proof of
      Invertible _ -> ppLayerCount proof `shouldBe` 3
      other -> expectationFailure $ "Expected Invertible verdict, got: " ++ show other

  it "is no-op when analysis disabled" $ do
    let cfg' = cfg { cfgEnableAnalysis = False }
        t = mkAffineTransform 0 0  -- would fail normally
        result = checkInvertibility cfg' t
    case result of
      Invertible "analysis disabled" -> return ()
      other -> expectationFailure $ "Expected disabled result, got: " ++ show other

-- ============================================================
-- 3. Leakage Tests
-- ============================================================

leakageSpec :: Spec
leakageSpec = describe "Leakage Analysis" $ do
  let cfg = defaultConfig
      p = cfgFieldPrime cfg

  it "detects affine chain collapse (5 consecutive affines)" $ do
    let transforms = [ mkAffineTransform (fromIntegral i * 3 + 1) (fromIntegral i * 7)
                     | i <- [1..5 :: Int] ]
        findings = detectAffineCollapse cfg transforms
    length findings `shouldBe` 1
    lfCategory (head findings) `shouldBe` "AFFINE_COLLAPSE"
    lfSeverity (head findings) `shouldBe` HIGH

  it "no collapse warning for mixed pipeline" $ do
    let transforms = [ mkAffineTransform 37 13
                     , mkPermTransform (generateFromSeed 64 42)
                     , mkAffineTransform 11 7
                     ]
        findings = detectAffineCollapse cfg transforms
    findings `shouldBe` []

  it "detects degree-1 polynomial as linearization" $ do
    let poly = mkBoundedPoly p 64 [Term 37 1, Term 13 0]  -- 37x + 13
        transforms = [mkPolyTransform poly]
        findings = detectLinearization transforms
    length findings `shouldBe` 1
    lfCategory (head findings) `shouldBe` "LINEARIZATION"

  it "no linearization warning for degree-3 poly" $ do
    let poly = mkBoundedPoly p 64 [Term 1 3, Term 2 1, Term 3 0]
        transforms = [mkPolyTransform poly]
        findings = detectLinearization transforms
    findings `shouldBe` []

  it "entropy estimation produces findings for non-empty poly" $ do
    let poly = mkBoundedPoly p 64 [Term 42 0, Term 100 1, Term 200 2]
        findings = estimateEntropy cfg poly
    length findings `shouldSatisfy` (>= 1)
    lfCategory (head findings) `shouldBe` "ENTROPY"

  it "scheduler predictability is flagged as LOW" $ do
    let findings = schedulerPredictability cfg
    length findings `shouldBe` 1
    lfSeverity (head findings) `shouldBe` LOW

  it "security score: diverse pipeline scores higher" $ do
    let diverseTs = [ mkAffineTransform 37 13
                    , mkPermTransform (generateFromSeed 64 42)
                    , mkAffineTransform 11 7
                    ]
        affineOnlyTs = [ mkAffineTransform 37 13
                       , mkAffineTransform 11 7
                       , mkAffineTransform 3 5
                       ]
        diverseReport = analyzeLeakage cfg diverseTs Nothing
        affineReport = analyzeLeakage cfg affineOnlyTs Nothing
    -- Diverse pipeline should score higher
    lrSecurityScore diverseReport `shouldSatisfy` (> lrSecurityScore affineReport)

  it "full report format is non-empty" $ do
    let transforms = [mkAffineTransform 37 13, mkAffineTransform 11 7]
        report = analyzeLeakage cfg transforms Nothing
        output = formatLeakageReport report
    length output `shouldSatisfy` (> 50)

  it "is no-op when analysis disabled" $ do
    let cfg' = cfg { cfgEnableAnalysis = False }
        transforms = [mkAffineTransform 37 13]
        report = analyzeLeakage cfg' transforms Nothing
    lrSecurityScore report `shouldBe` (-1)
    lrFindings report `shouldBe` []
