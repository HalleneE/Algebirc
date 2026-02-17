{-# OPTIONS_GHC -Wno-type-defaults #-}
module AdvancedAnalysisSpec (advancedAnalysisSpec) where

import Test.Hspec
import Algebirc.Core.Types
import Algebirc.Analysis.FormalEntropy
import Algebirc.Analysis.AlgebraicLeakage
import Algebirc.Analysis.CircuitComplexity
import Algebirc.Analysis.LinearizationAttack
import Algebirc.Obfuscation.NonlinearTransform (generateSBox, mkSBoxTransform, mkFeistelTransform, mkPowerMapTransform)

-- ============================================================
-- Test Config & Fixtures
-- ============================================================

testCfg :: ObfuscationConfig
testCfg = defaultConfig { cfgFieldPrime = 257, cfgMaxDegree = 8, cfgEnableAnalysis = True }

testKey :: SecretKey
testKey = SecretKey { skSeed = 42, skRounds = 4, skSBoxSeed = 7, skPowerExp = 3 }

-- Dense polynomial with diverse coefficients
densePoly :: BoundedPoly
densePoly = mkBoundedPoly 257 8
  [ Term 12 8, Term 100 7, Term 45 6, Term 200 5, Term 7 4
  , Term 189 3, Term 33 2, Term 250 1, Term 1 0 ]

-- Sparse polynomial (only 3 terms out of 9 positions)
sparsePoly :: BoundedPoly
sparsePoly = mkBoundedPoly 257 8 [ Term 42 8, Term 13 3, Term 1 0 ]

-- Uniform polynomial (all coefficients different)
uniformPoly :: BoundedPoly
uniformPoly = mkBoundedPoly 257 8
  [ Term 10 8, Term 50 7, Term 90 6, Term 130 5, Term 170 4
  , Term 210 3, Term 30 2, Term 70 1, Term 110 0 ]

-- Constant polynomial (all zeros except c0)
constPoly :: BoundedPoly
constPoly = mkBoundedPoly 257 8 [ Term 42 0 ]

-- All-same-coefficients polynomial (low entropy)
lowEntropyPoly :: BoundedPoly
lowEntropyPoly = mkBoundedPoly 257 8
  [ Term 5 8, Term 5 7, Term 5 6, Term 5 5, Term 5 4
  , Term 5 3, Term 5 2, Term 5 1, Term 5 0 ]

-- Build affine transform
mkAffine :: Integer -> Integer -> Transform
mkAffine a b = Transform
  { transformTag    = AffineTransform
  , transformPoly   = Nothing
  , transformPerm   = Nothing
  , transformA      = Just a
  , transformB      = Just b
  , transformSubs   = []
  , transformSBox   = Nothing
  , transformExp    = Nothing
  , transformRounds = 0
  , transformKey    = Nothing
  }

-- Build permutation transform
mkPermT :: Transform
mkPermT = Transform
  { transformTag    = PermutationTransform
  , transformPoly   = Nothing
  , transformPerm   = Nothing  -- simplified for testing
  , transformA      = Nothing
  , transformB      = Nothing
  , transformSubs   = []
  , transformSBox   = Nothing
  , transformExp    = Nothing
  , transformRounds = 0
  , transformKey    = Nothing
  }

-- Build polynomial sub transform (degree-3)
mkPolySub :: Transform
mkPolySub = Transform
  { transformTag    = PolynomialTransform
  , transformPoly   = Just (mkBoundedPoly 257 8 [Term 1 3, Term 2 1, Term 5 0])
  , transformPerm   = Nothing
  , transformA      = Nothing
  , transformB      = Nothing
  , transformSubs   = []
  , transformSBox   = Nothing
  , transformExp    = Nothing
  , transformRounds = 0
  , transformKey    = Nothing
  }

-- ============================================================
-- Tests
-- ============================================================

advancedAnalysisSpec :: Spec
advancedAnalysisSpec = do
  describe "Formal Entropy" $ do
    it "dense poly has H∞ ≤ H_Shannon ≤ H_max (entropy ordering invariant)" $ do
      let ea = analyzeEntropy testCfg densePoly
      eaMinEntropy ea `shouldSatisfy` (<= eaShannon ea + 0.001)
      eaShannon ea `shouldSatisfy` (<= eaMaxEntropy ea + 0.001)

    it "uniform poly has higher Shannon entropy than constant poly" $ do
      let eaU = analyzeEntropy testCfg uniformPoly
          eaC = analyzeEntropy testCfg constPoly
      eaShannon eaU `shouldSatisfy` (> eaShannon eaC)

    it "low-entropy poly grades worse than diverse poly" $ do
      let eaLow  = analyzeEntropy testCfg lowEntropyPoly
          eaHigh = analyzeEntropy testCfg densePoly
      eaGrade eaLow `shouldSatisfy` (>= eaGrade eaHigh)  -- F > A in Char ordering

    it "entropy grade is A-F" $ do
      let ea = analyzeEntropy testCfg densePoly
      eaGrade ea `shouldSatisfy` (\c -> c `elem` ['A','B','C','D','F'])

    it "redundancy is between 0 and 1" $ do
      let ea = analyzeEntropy testCfg densePoly
      eaRedundancy ea `shouldSatisfy` (>= 0.0)
      eaRedundancy ea `shouldSatisfy` (<= 1.0)

    it "formatEntropy produces non-empty output" $ do
      let ea = analyzeEntropy testCfg densePoly
      formatEntropy ea `shouldSatisfy` (not . null)

    it "Shannon entropy is non-negative" $ do
      shannonEntropy [1, 2, 3, 4, 5] `shouldSatisfy` (> 0.0)
      shannonEntropy [] `shouldBe` 0.0

    it "min-entropy ≤ Shannon entropy" $ do
      let xs = [1, 1, 2, 3, 4, 5, 5, 5]
      minEntropy xs `shouldSatisfy` (<= shannonEntropy xs + 0.001)

  describe "Algebraic Leakage" $ do
    it "detects identity affine as fixed-point invariant" $ do
      let identityAffine = mkAffine 1 0
          aa = analyzeAlgebraic testCfg [identityAffine] densePoly
      aaInvariants aa `shouldSatisfy` (not . null)

    it "detects monoculture in all-affine pipeline" $ do
      let pipeline = [mkAffine 37 10, mkAffine 100 5, mkAffine 13 200]
          aa = analyzeAlgebraic testCfg pipeline densePoly
      aaInvariants aa `shouldSatisfy` (not . null)

    it "no monoculture for diverse pipeline" $ do
      case generateSBox 257 42 of
        Left _ -> expectationFailure "S-box failed"
        Right sb -> do
          case mkPowerMapTransform 3 257 of
            Left _ -> expectationFailure "PowerMap failed"
            Right pmT -> do
              let pipeline = [mkAffine 37 10, mkSBoxTransform sb, pmT]
                  aa = analyzeAlgebraic testCfg pipeline densePoly
                  hasMonoculture = any ("MONOCULTURE" `isPrefixOf`) (aaInvariants aa)
              hasMonoculture `shouldBe` False

    it "sparse polynomial triggers monomial leakage" $ do
      detectMonomialLeak testCfg sparsePoly `shouldBe` True

    it "dense polynomial has no monomial leakage" $ do
      detectMonomialLeak testCfg densePoly `shouldBe` False

    it "all-linear pipeline has high linear relation count" $ do
      let pipeline = [mkAffine 37 10, mkPermT, mkAffine 100 5]
          linRels = countLinearRelations testCfg pipeline densePoly
      linRels `shouldSatisfy` (> 0)

    it "nonlinear pipeline has fewer linear relations" $ do
      case generateSBox 257 42 of
        Left _ -> expectationFailure "S-box failed"
        Right sb -> do
          case mkPowerMapTransform 3 257 of
            Left _ -> expectationFailure "PowerMap failed"
            Right pmT -> do
              let linearPipeline = [mkAffine 37 10, mkPermT, mkAffine 100 5]
                  nonlinPipeline = [mkSBoxTransform sb, mkFeistelTransform sb 4 testKey, pmT]
                  linLinear = countLinearRelations testCfg linearPipeline densePoly
                  linNonlin = countLinearRelations testCfg nonlinPipeline densePoly
              linNonlin `shouldSatisfy` (<= linLinear)

    it "Gröbner basis size is positive" $ do
      computeGroebnerSize testCfg densePoly `shouldSatisfy` (> 0)

    it "formatAlgebraic produces non-empty output" $ do
      let aa = analyzeAlgebraic testCfg [mkAffine 37 10] densePoly
      formatAlgebraic aa `shouldSatisfy` (not . null)

  describe "Circuit Complexity" $ do
    it "gate count is monotonic with pipeline length" $ do
      let p1 = [mkAffine 37 10]
          p2 = [mkAffine 37 10, mkAffine 100 5]
          p3 = [mkAffine 37 10, mkAffine 100 5, mkPolySub]
          g1 = caGateCount (analyzeCircuit testCfg p1)
          g2 = caGateCount (analyzeCircuit testCfg p2)
          g3 = caGateCount (analyzeCircuit testCfg p3)
      g2 `shouldSatisfy` (>= g1)
      g3 `shouldSatisfy` (>= g2)

    it "permutation has zero gate count" $ do
      let (addG, mulG) = gateCount testCfg mkPermT
      addG `shouldBe` 0
      mulG `shouldBe` 0

    it "Feistel has higher gate count than affine" $ do
      case generateSBox 257 42 of
        Left _ -> expectationFailure "S-box failed"
        Right sb -> do
          let feistelT = mkFeistelTransform sb 4 testKey
              affineT = mkAffine 37 10
              (_, mulF) = gateCount testCfg feistelT
              (_, mulA) = gateCount testCfg affineT
          mulF `shouldSatisfy` (>= mulA)

    it "circuit depth is positive for non-trivial pipeline" $ do
      let pipeline = [mkAffine 37 10, mkPolySub]
      circuitDepth pipeline `shouldSatisfy` (> 0)

    it "lower bound is reasonable" $ do
      case generateSBox 257 42 of
        Left _ -> expectationFailure "S-box failed"
        Right sb -> do
          let pipeline = [mkSBoxTransform sb, mkFeistelTransform sb 3 testKey]
              lb = lowerBound pipeline
          lb `shouldSatisfy` (> 0)
          lb `shouldSatisfy` (<= caGateCount (analyzeCircuit testCfg pipeline))

    it "formatCircuit produces non-empty output" $ do
      let ca = analyzeCircuit testCfg [mkAffine 37 10]
      formatCircuit ca `shouldSatisfy` (not . null)

  describe "Linearization Attack" $ do
    it "all-linear pipeline is vulnerable" $ do
      let pipeline = [mkAffine 37 10, mkPermT, mkAffine 100 5]
          aa = analyzeAttack testCfg pipeline densePoly
      laVulnerable aa `shouldBe` True

    it "nonlinear pipeline is not vulnerable" $ do
      case generateSBox 257 42 of
        Left _ -> expectationFailure "S-box failed"
        Right sb -> do
          case mkPowerMapTransform 3 257 of
            Left _ -> expectationFailure "PowerMap failed"
            Right pmT -> do
              let pipeline = [mkSBoxTransform sb, mkFeistelTransform sb 4 testKey, pmT]
                  aa = analyzeAttack testCfg pipeline densePoly
              laVulnerable aa `shouldBe` False

    it "known pairs needed grows with nonlinear layers" $ do
      case generateSBox 257 42 of
        Left _ -> expectationFailure "S-box failed"
        Right sb -> do
          let p1 = [mkAffine 37 10]
              p2 = [mkAffine 37 10, mkSBoxTransform sb]
              p3 = [mkAffine 37 10, mkSBoxTransform sb, mkFeistelTransform sb 3 testKey]
              k1 = knownPairsNeeded p1
              k2 = knownPairsNeeded p2
              k3 = knownPairsNeeded p3
          k2 `shouldSatisfy` (>= k1)
          k3 `shouldSatisfy` (>= k2)

    it "matrix rank ≤ matrix dimension" $ do
      let pipeline = [mkAffine 37 10]
          matrix = buildLinearizationMatrix testCfg pipeline densePoly
          rank = matrixRank 257 matrix
          dim = cfgMaxDegree testCfg + 1
      rank `shouldSatisfy` (<= dim)

    it "attack complexity string is non-empty" $ do
      attackComplexity [mkAffine 37 10] `shouldSatisfy` (not . null)

    it "formatAttack produces non-empty output" $ do
      let aa = analyzeAttack testCfg [mkAffine 37 10] densePoly
      formatAttack aa `shouldSatisfy` (not . null)

-- Helper
isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
